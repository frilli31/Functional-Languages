-- {-# OPTIONS_GHC -Wall #-}

module Chapter12 where

import Prelude

data Tree a = Leaf | Node (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
    show (Leaf) = "_"
    show (Node l s r) = "(" ++ show s ++ "," ++  show l ++ "," ++  show r ++ ")"

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf = Leaf
    fmap g (Node l s r) = Node (fmap g l)  (g s) (fmap g r)

instance Functor ((->) a) where
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap = (.)

-- Define an instance of the Applicative class for the type (a ->). 
-- If you are familiar with combinatory logic, you might recognise pure and 
-- <*> for this type as being the well-known K and S combinators.
instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    pure = const
    -- (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
    g <*> h = \x -> g x (h x)

-- here may be more than one way to make a parameterised type into an applicative functor. 
-- For example, the library Control.Applicative provides an alternative ‘zippy’ instance for lists, 
-- in which the function pure makes an infinite list of copies of its argument, and the operator <*> 
-- applies each argument function to the corresponding argument value at the same position. Complete 
-- the following declarations that implement this idea:

main :: IO ()
main = do
    print (Node (Node Leaf 2 Leaf) 1 Leaf)