
--- Find the last-but-one (or second-last) element of a list.
myButLast :: [a] -> a -- for any element a list, turn into a thing 
myButLast list = head (tail (reverse list)) --- one line compact implementation


--- recursive form
myButLast' :: [a] -> a
myButLast' (x:_:[]) = x
myButLast' (_:xs) = myButLast' xs
myButLast' _ = error "myButLast': list too short"  


