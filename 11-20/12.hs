decode :: [(Int, a)] -> [a]
decode = concatMap (uncurry replicate)