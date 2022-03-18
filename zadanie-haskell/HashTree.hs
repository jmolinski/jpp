-- jm419502

module HashTree where

import Hashable32

data Tree a
  = Leaf Hash a
  | Twig Hash (Tree a)
  | Node Hash (Tree a) (Tree a)

leaf :: Hashable a => a -> Tree a
leaf a = Leaf (hash a) a

twig :: Hashable a => Tree a -> Tree a
twig l = Twig (hash (treeHash l, treeHash l)) l

node :: Hashable a => Tree a -> Tree a -> Tree a
node l r = Node (hash (treeHash l, treeHash r)) l r

buildTree :: Hashable a => [a] -> Tree a
buildTree lst = head (buildTree' (map leaf lst))
  where
    buildTree' :: Hashable a => [Tree a] -> [Tree a]
    buildTree' [x] = [x]
    buildTree' lst = buildTree' (combineLevel lst)
    combineLevel :: Hashable a => [Tree a] -> [Tree a]
    combineLevel [] = []
    combineLevel [x] = [twig x]
    combineLevel (x : y : t) = (node x y) : (combineLevel t)

treeHash :: Tree a -> Hash
treeHash (Leaf h _) = h
treeHash (Twig h _) = h
treeHash (Node h _ _) = h

drawTree :: Show a => Tree a -> String
drawTree t = drawIndentedTree 0 t ""
  where
    drawIndentedTree :: Show a => Int -> Tree a -> ShowS
    drawIndentedTree i t =
      showString (replicate (i * 2) ' ')
        . showString (showHash (treeHash t))
        . drawTree' i t
    drawTree' :: Show a => Int -> Tree a -> ShowS
    drawTree' _ (Leaf _ a) = showChar ' ' . showString (show a) . showChar '\n'
    drawTree' i (Twig _ l) = showString " +\n" . drawIndentedTree (i + 1) l
    drawTree' i (Node _ l r) =
      showString " -\n"
        . drawIndentedTree (i + 1) l
        . drawIndentedTree (i + 1) r

type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

instance Show a => Show (MerkleProof a) where
  showsPrec p (MerkleProof a path) =
    showParen (p > 10) $
      showString "MerkleProof "
        . showsPrec 11 a
        . showString " "
        . showString (showMerklePath path)

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof v t =
  let paths = merklePaths v t
   in if null paths
        then Nothing
        else Just $ MerkleProof v $ head paths

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths v (Leaf h _) = [[] | hash v == h]
merklePaths v (Twig _ t) = map (Left (treeHash t) :) (merklePaths v t)
merklePaths v (Node _ l r) =
  let left = (map (Left (treeHash r) :) (merklePaths v l))
   in let right = (map (Right (treeHash l) :) (merklePaths v r))
       in left ++ right

showMerklePath :: MerklePath -> [Char]
showMerklePath [] = ""
showMerklePath (Left h : t) = "<" ++ showHash h ++ showMerklePath t
showMerklePath (Right h : t) = ">" ++ showHash h ++ showMerklePath t

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a []) = hash a == h
verifyProof h (MerkleProof a path) =
  h == hashPath path
  where
    hashPath :: MerklePath -> Hash
    hashPath [] = hash a
    hashPath (Left hr : t) = hash (hashPath t, hr)
    hashPath (Right hl : t) = hash (hl, hashPath t)
