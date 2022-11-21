import Data.List

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeDirect :: Eq a => [a] -> [ListItem a]
encode xs = map (\x -> (length x,head x)) (group xs)
encodeDirect = map encodeHelper . encode
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x