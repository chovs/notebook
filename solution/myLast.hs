-- find the last element of a list

myLast :: [a] -> a
myLast [] = error "No element in a list anymore"
myLast [x] = x
myLast (_:xs) = myLast xs
