-- model the context of the code
context :: [a] -> a
context [] = error "There is no context in the empty list"