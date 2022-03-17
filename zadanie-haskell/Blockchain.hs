module Blockchain where

import Control.Monad
import Data.Word
import HashTree
import Hashable32
import PPrint
import Utils

type Address = Hash

type Amount = Word32

coin :: Amount
coin = 1000

data Transaction = Tx
  { txFrom :: Address,
    txTo :: Address,
    txAmount :: Amount
  }
  deriving (Show)

instance Hashable Transaction where
  hash (Tx a b c) = hash [hash a, hash b, hash c]

data Block = Block {blockHdr :: BlockHeader, blockTxs :: [Transaction]}

instance Show Block where
  show (Block hdr txs) = unlines (show hdr : map show txs)

instance Hashable Block where
  hash = hash . blockHdr

data BlockHeader = BlockHeader
  { parent :: Hash,
    coinbase :: Transaction,
    txroot :: Hash, -- root of the Merkle tree
    nonce :: Hash
  }
  deriving (Show)

instance Hashable BlockHeader where
  hash (BlockHeader p c r n) = hash [p, hash c, r, n]

difficulty = 5

blockReward = 50 * coin

coinbaseTx miner = Tx {txFrom = 0, txTo = miner, txAmount = blockReward}

validNonce :: BlockHeader -> Bool
validNonce b = (hash b) `mod` (2 ^ difficulty) == 0

tx1 =
  Tx
    { txFrom = hash "Alice",
      txTo = hash "Bob",
      txAmount = 1 * coin
    }

type Miner = Address

type Nonce = Word32

mineBlock :: Miner -> Hash -> [Transaction] -> Block
mineBlock miner parent txs =
  let coinbase = coinbaseTx miner
   in let transactionsHash = treeHash $ buildTree (coinbase : txs)
       in Block (findValidHeader coinbase transactionsHash) txs
  where
    findValidHeader = findValidHeader' 0
    findValidHeader' :: Int -> Transaction -> Hash -> BlockHeader
    findValidHeader' nonce coinbase txsHash =
      let header = BlockHeader parent coinbase txsHash (hash nonce)
       in if validNonce header
            then header
            else findValidHeader' (nonce + 1) coinbase txsHash

genesis = block0

block0 = mineBlock (hash "Satoshi") 0 []

block1 = mineBlock (hash "Alice") (hash genesis) []

block2 = mineBlock (hash "Charlie") (hash block1) [tx1]

chain = [block2, block1, block0]

-- | Chain verification
-- >>> verifyChain [block1, block2]
-- Nothing
--
-- >>> VH <$> verifyChain [block2,block1,block0]
-- Just 0x0dbea380
validChain :: [Block] -> Bool
validChain c =
  case verifyChain c of
    Nothing -> False
    Just _ -> True

verifyChain :: [Block] -> Maybe Hash
verifyChain [] = Just 0
verifyChain (b : t) =
  case verifyChain t of
    Nothing -> Nothing
    Just parentHash -> verifyBlock b parentHash

verifyBlock :: Block -> Hash -> Maybe Hash
verifyBlock b@(Block hdr txs) parentHash = do
  guard (parent hdr == parentHash)
  guard (txroot hdr == treeHash (buildTree (coinbase hdr : txs)))
  guard (validNonce hdr)
  return (hash b)

-- | Transaction Receipts
-- NB the following will not work in VS Code, see below
-- >>> let charlie = hash "Charlie"
-- >>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
-- >>> block
-- BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
-- Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}
-- <BLANKLINE>
--
-- >>> receipt
-- TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}
-- >>> validateReceipt receipt (blockHdr block)
-- True

-- For VS Code, we need to use the "error trick"
{- Transaction Receipts
>>> let charlie = hash "Charlie"
>>> let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1]
>>> error $ show block
BlockHeader {parent = 797158976, coinbase = Tx {txFrom = 0, txTo = 1392748814, txAmount = 50000}, txroot = 2327748117, nonce = 3}
Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}

>>> receipt
TxReceipt {txrBlock = 230597504, txrProof = MerkleProof (Tx {txFrom = 2030195168, txTo = 2969638661, txAmount = 1000}) >0xbcc3e45a}

>>> validateReceipt receipt (blockHdr block)
True
-}

data TransactionReceipt = TxReceipt
  {txrBlock :: Hash, txrProof :: MerkleProof Transaction}
  deriving (Show)

validateReceipt :: TransactionReceipt -> BlockHeader -> Bool
validateReceipt r hdr =
  txrBlock r == hash hdr
    && verifyProof (txroot hdr) (txrProof r)

mineTransactions :: Miner -> Hash -> [Transaction] -> (Block, [TransactionReceipt])
mineTransactions miner parent txs =
  let block = mineBlock miner parent txs
   in let blockHash = hash (blockHdr block)
       in (block, makeReceipts block blockHash)
  where
    makeReceipts :: Block -> Hash -> [TransactionReceipt]
    makeReceipts (Block hdr txs) blockHash =
      let merkleTree = buildTree ((coinbase hdr) : txs)
       in map (makeReceipt blockHash merkleTree) txs
    makeReceipt blockHash tree tx =
      case buildProof tx tree of
        Nothing -> error "invalid merkle tree"
        Just proof -> TxReceipt blockHash proof
