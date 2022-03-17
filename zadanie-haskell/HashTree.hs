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

