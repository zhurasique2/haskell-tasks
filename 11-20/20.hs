removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = case back of
        [] -> error "err"
        x:rest -> (x, front ++ rest)
  where (front, back) = splitAt (k - 1) xs