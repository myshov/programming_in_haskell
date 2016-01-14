nest2gens :: [(Int,Int)]
nest2gens = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

nest1gens :: [(Int, Int)]
nest1gens = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
