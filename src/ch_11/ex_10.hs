module ExerciseNine where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

{-
 - 1) Write map for BinaryTree
 -}


testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

{-
 - Recursion Steps
 -
 - mapTree (+1) testTree'
 - mapTree (+1) Node (Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
 -
 - =
 - Node (mapTree (+1) Node (Leaf 3 Leaf)) (+1) 1 (mapTree (+1) Node (Leaf 4 Leaf))
 -      (Node (Leaf) (+1) 3 (Leaf))         2    (Node (Leaf) (+1) 5 (Leaf))
 -      (Node (Leaf)      4 (Leaf))         2    (Node (Leaf)      5 (Leaf))
 - =
 - Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
 -
 --}

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

{-
 - Convert BinaryTrees to Lists
 -}

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (preorder left) ++ (preorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."


{-
 - Write foldr for BinaryTree
-}

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf                = b
foldTree f b (Node left a right) = f a (foldTree f (foldTree f b left) right)

