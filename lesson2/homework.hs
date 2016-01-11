average = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

mylast xs = xs !! (length xs - 1)

mylast2 = head.reverse

myinit xs = take (length xs - 1) xs

myinit2 = reverse.drop 1.reverse
