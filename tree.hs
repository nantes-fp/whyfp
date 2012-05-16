module Tree where

import List

data Tree' a = Node a (List' (Tree' a))
