module Data.JoinList where

import Prelude hiding (filter)

import Data.Semigroup

data JoinList a
    = Empty
    | Singleton a
    | Join (JoinList a) (JoinList a)
    deriving (Eq, Show)

instance Functor JoinList where
    fmap _ Empty = Empty
    fmap f (Singleton a) = Singleton (f a)
    fmap f (Join l r) = Join (f <$> l) (f <$> r)

instance Applicative JoinList where
    pure = Singleton

    (<*>) Empty as = Empty
    (<*>) (Singleton f) Empty = Empty
    (<*>) (Singleton f) (Singleton a) = Singleton (f a)
    (<*>) (Singleton f) (Join l r) = Join (f <$> l) (f <$> r)
    (<*>) (Join l r) as = Join (l <*> as) (r <*> as)

instance Semigroup (JoinList a) where
    (<>) = join

instance Monoid (JoinList a) where
    mempty = Empty
    mappend = (<>)

instance Monad JoinList where
    (>>=)

leftHead ::
    JoinList a
    -> Maybe a
leftHead Empty = Nothing
leftHead (Singleton a) = Just a
leftHead (Join l _) = leftHead l

rightHead ::
    JoinList a
    -> Maybe a
rightHead Empty = Nothing
rightHead (Singleton a) = Just a
rightHead (Join _ r) = rightHead r

cons ::
    a
    -> JoinList a
    -> JoinList a
cons a = Join (Singleton a)

snoc ::
    JoinList a
    -> a
    -> JoinList a
snoc ls = Join ls . Singleton

join ::
    JoinList a
    -> JoinList a
    -> JoinList a
join Empty r = r
join l Empty = l
join l r = Join l r

filter ::
    (a -> Bool)
    -> JoinList a
    -> JoinList a
filter _ Empty = Empty
filter predicate (Singleton a)
    | predicate a = Singleton a
    | otherwise = Empty
filter predicate (Join l r)
    | isEmpty (filter predicate l) = filter predicate r
    | isEmpty (filter predicate r) = filter predicate l
    | otherwise = Join (filter predicate l) (filter predicate r)

isEmpty ::
    JoinList a
    -> Bool
isEmpty Empty = True
isEmpty _ = False
