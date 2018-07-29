lastButOne xs = xs !! (length xs - 2)
lastButOneR xs = head . tail . reverse $ xs
