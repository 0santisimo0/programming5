module Chapter7 where
import Data.Char (ord, chr)

-- 8
faultyChannel :: String -> String
faultyChannel = tail

-- 9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:y:xs) = f x : g y : altMap f g xs
altMap f g [x]       = [f x]
