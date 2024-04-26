
data BTree a = Empty
            | Node a (BTree a) (BTree a)
            deriving (Show)

tree :: BTree Int
tree = Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty)


addNode :: Int -> Int -> BTree Int -> BTree Int
addNode a _ Empty = Node a Empty Empty
addNode _ b Empty = Node b Empty Empty
addNode a b (Node x left right) = Node x (addNode a b left) (addNode a b right)