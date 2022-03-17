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
drawTree t = drawIndentedTree 0 t
  where
    drawIndentedTree :: Show a => Int -> Tree a -> String
    drawIndentedTree i t =
      replicate (i * 2) ' ' ++ showHash (treeHash t) ++ drawTree' i t
    drawTree' :: Show a => Int -> Tree a -> String
    drawTree' _ (Leaf h a) = " " ++ show a ++ "\n"
    drawTree' i (Twig h l) = " +\n" ++ drawIndentedTree (i + 1) l
    drawTree' i (Node h l r) = " -\n" ++ drawIndentedTree (i + 1) l ++ drawIndentedTree (i + 1) r

type MerklePath = [Either Hash Hash]

data MerkleProof a = MerkleProof a MerklePath

buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
buildProof v t =
  let paths = merklePaths v t
   in if null paths
        then Nothing
        else Just $ MerkleProof v $ head paths

merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
merklePaths v (Leaf h _) = [[] | hash v == h]
merklePaths v (Twig _ (Leaf lh _)) = [[Left lh] | hash v == lh]
merklePaths v (Twig _ t) = map (Left (treeHash t) :) (merklePaths v t)
merklePaths v (Node _ (Leaf lh _) (Leaf rh _)) = [[Left rh] | hash v == lh] ++ [[Right lh] | hash v == rh]
merklePaths v (Node _ l (Leaf rh _)) = (map (Left rh :) (merklePaths v l)) ++ [[Right (treeHash l)] | hash v == rh]
merklePaths v (Node _ (Leaf lh _) r) = [[Left (treeHash r)] | hash v == lh] ++ (map (Right lh :) (merklePaths v r))
merklePaths v (Node _ l r) = (map (Left (treeHash r) :) (merklePaths v l)) ++ (map (Right (treeHash l) :) (merklePaths v r))

showMerklePath :: MerklePath -> [Char]
showMerklePath [] = ""
showMerklePath (Left h : t) = "<" ++ showHash h ++ showMerklePath t
showMerklePath (Right h : t) = ">" ++ showHash h ++ showMerklePath t

instance Show a => Show (MerkleProof a) where
  show (MerkleProof a p) =
    (showString "(MerkleProof " . showString (show a) . showString " " . showString (showMerklePath p)) ")"

verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
verifyProof h (MerkleProof a []) = hash a == h
verifyProof h (MerkleProof a path) =
  h == hashPath path
  where
    hashPath :: MerklePath -> Hash
    hashPath [] = hash a
    hashPath (Left hr : t) = hash (hashPath t, hr)
    hashPath (Right hl : t) = hash (hl, hashPath t)
