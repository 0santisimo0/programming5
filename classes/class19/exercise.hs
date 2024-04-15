module Exercise where

data BTree a = Empty
            | Null
            | Node a (BTree a) (BTree a)
            deriving (Show, Eq)

buildTreeExample :: BTree Int
buildTreeExample = Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)

buildTree :: Int -> [(Int, Int)] -> BTree Int
buildTree 0 _ = Empty
buildTree n ((x,y):xs) = addNode x y (buildTree (n-1) xs)


getNode :: Int -> [BTree Int] -> BTree Int
getNode _ [] = Empty
getNode n (Empty:xs) = getNode n xs
getNode n (Node v l r:xs)
    | n == v = Node v l r
    | otherwise = getNode n (l:r:xs)


levOrder t = lo [t]
    where lo :: [BTree a] -> [a]
          lo [] = []
          lo (Empty:xs) = lo xs
          lo (Node v l r:xs) = v : lo (xs ++ [l, r])



addNode :: Int -> Int -> BTree Int -> BTree Int
addNode 1 0 Empty = Node 1 Empty Empty
addNode a b xs = insertBFS a b [xs] (Node 1 Empty Empty)


levelOrderA :: Int -> Int -> BTree Int -> BTree Int
levelOrderA a b t = lo [t]
  where
    lo :: [BTree Int] -> BTree Int
    lo [] = Empty
    lo (Empty:xs) = lo xs
    lo (Node v l r:xs)
      | isEmpty l && isEmpty r = Node v (Node a Empty Empty) (Node b Empty Empty)
      | otherwise = lo (xs ++ [l, r])

    isEmpty :: BTree a -> Bool
    isEmpty Empty = True
    isEmpty _ = False


insertBFS :: Int -> Int -> [BTree Int] -> BTree Int -> BTree Int
insertBFS _ _ [] ys = ys
insertBFS _ _ [Empty] ys = ys
insertBFS a b (Null:xs) ys = insertBFS a b xs ys
insertBFS (-1) (-1) (Node v Empty Empty:xs) ys = replace (Node v Null Null) ys
insertBFS (-1) b (Node v Empty Empty:xs) ys = replace (Node v Null (Node b Empty Empty)) ys
insertBFS a (-1) (Node v Empty Empty:xs) ys = replace (Node v (Node a Empty Empty) Null) ys
insertBFS a b (Node v Empty Empty:xs) ys = replace (Node v (Node a Empty Empty) (Node b Empty Empty)) ys
insertBFS a b (Node v l r:xs) ys = insertBFS a b (xs++[l,r]) (replace (Node v l r) ys)
insertBFS _ _ _ ys = ys



replace :: Eq a => BTree a -> BTree a -> BTree a
replace _ Empty = Empty
replace _ Null = Null
replace (Node n l r) (Node x left right)
    | x == n  = (Node n l r)
    | otherwise = Node x (replace (Node n l r) left) (replace (Node n l r) right)


swapTree :: Int -> [Int] -> BTree Int -> [Int]
swapTree _ (y:ys) tree = swapFromDepth y tree


swapFromDepth :: Int -> BTree Int -> [Int]
swapFromDepth _ _ = [1]


swapNodesAtDepth :: BTree a -> Int -> BTree a
swapNodesAtDepth tree depth = swapNodesAtDepth' tree depth 1
    where
        swapNodesAtDepth' Null _ _ = Null
        swapNodesAtDepth' Empty _ _ = Empty
        swapNodesAtDepth' (Node val left right) depth currDepth
            | currDepth == depth = Node val (swapSubTrees right) (swapSubTrees left)
            | otherwise = Node val (swapNodesAtDepth' left depth (currDepth + 1)) (swapNodesAtDepth' right depth (currDepth + 1))
        
        swapSubTrees Null = Null
        swapSubTrees Empty = Empty
        swapSubTrees (Node val left right) = Node val (swapSubTrees right) (swapSubTrees left)

-- inOrderTraversal :: BTree a -> [a]
-- inOrderTraversal Empty = []
-- inOrderTraversal (Node val left right) =
--     let leftTraversal = inOrderTraversal left
--         rightTraversal = inOrderTraversal right
--     in case (left, right) of
--         (Null, Null) -> [val]
--         (Null, _) -> val : rightTraversal
--         (_, Null) -> leftTraversal ++ [val]
--         _ -> leftTraversal ++ [val] ++ rightTraversal


inOrderTraversalIterative :: BTree a -> [a]
inOrderTraversalIterative tree = reverse $ inOrderTraversalHelper [tree] []
    where
        inOrderTraversalHelper :: [BTree a] -> [a] -> [a]
        inOrderTraversalHelper [] result = result
        inOrderTraversalHelper (Empty:xs) result = inOrderTraversalHelper xs result
        inOrderTraversalHelper (Null:xs) result = inOrderTraversalHelper xs result
        inOrderTraversalHelper ((Node val Null right):xs) result = inOrderTraversalHelper (right:xs) (val:result)
        inOrderTraversalHelper ((Node val left Null):xs) result = inOrderTraversalHelper (left:xs) (val:result)
        inOrderTraversalHelper ((Node val Null Null):xs) result = inOrderTraversalHelper xs (val:result)
        inOrderTraversalHelper ((Node val left right):xs) result = inOrderTraversalHelper (left:right:Node val Null Null:xs) result



main :: IO ()
main = do
    let tree1 = reverse [(1, 0), (2,3)]
    let tree2 = reverse [(1, 0), (2,3), (-1, 4), (-1, 5)]
    let tree3 = reverse [(1, 0), (2,3), (4, -1), (5, -1), (6, -1), (7, 8), (-1, 9), (-1, -1), (10, 11)]
    let res1 = buildTree 2 tree1
    let res2 = buildTree 4 tree2
    let res3 = buildTree 9 tree3

    let prueba1 = (Node 5 (Node 7 Empty Empty) (Node 8 Empty Empty))
    let prueba2 = Node 1 (Node 2 (Node 4 (Node 6 Empty Empty) Null) Null) (Node 3 (Node 5 Empty Empty) Null)

    let finalResult1 = swapTree 1 [1] res1

    putStrLn $ "BTree 1: " ++ show res1
    putStrLn $ "BTree 2: " ++ show res2
    putStrLn $ "BTree 3: " ++ show res3

    putStrLn $ "In-order traversal values: " ++ show (inOrderTraversalIterative res3)

    let treeRes3 = swapNodesAtDepth (swapNodesAtDepth res3 2) 4

    putStrLn $ "First nodes swap tree1: " ++ show (inOrderTraversalIterative (swapNodesAtDepth res1 1))
    putStrLn $ "First nodes swap tree2: " ++ show (inOrderTraversalIterative (swapNodesAtDepth res2 1))
    putStrLn $ "First nodes swap tree3: " ++ show (inOrderTraversalIterative treeRes3)
    putStrLn $ "First nodes swap tree3: " ++ show (inOrderTraversalIterative (swapNodesAtDepth (swapNodesAtDepth (swapNodesAtDepth res3 2) 4) 4))




    -- putStrLn $ "Result 3: " ++ show treeRestul

-- changeValueInNode :: BTree Int -> Int -> BTree Int
-- changeValueInNode Empty _ = Empty
-- changeValueInNode tree@(Node (2) l r) newValue = Node newValue l r



-- traverseTreeDfs :: BTree a -> [a]
-- traverseTreeDfs Empty = []
-- traverseTreeDfs (Node val left right) = 
--     val : (traverseTreeDfs left ++ traverseTreeDfs right)




isNodeEmpty :: BTree a -> Bool
isNodeEmpty Empty = True
isNodeEmpty _ = False
