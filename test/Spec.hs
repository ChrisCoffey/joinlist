import Test.Tasty
import Test.QuickCheck
import Test.Tasty.QuickCheck

import Data.JoinList

import Debug.Trace

main :: IO ()
main = defaultMain listIsopmorphism

listIsopmorphism :: TestTree
listIsopmorphism = testGroup "(JoinList a) isomporphic to [a]" [
    -- construction
    testProperty "toList . fromList == id" $ \xs ->
        toList (fromFoldable xs) == (xs :: [Int])
    , testProperty "cons == (:)" $ \(xs, a) ->
        toList (a `cons` fromFoldable xs) == (a::Int):xs
    , testProperty "snoc == reverse . (:) . reeverse" $ \(xs, a) ->
        toList (fromFoldable xs `snoc` a) == (reverse . (:) (a:: Int) $ reverse xs)
    -- Accessing elements
    , testProperty "head == leftHead" $ \xs -> let
        val = case (xs :: [Int]) of
            [] -> Nothing
            _ -> Just $ head xs
        in leftHead (fromFoldable xs) == val
    , testProperty "last == rightHead" $ \xs -> let
        val = case (xs :: [Int]) of
            [] -> Nothing
            _ -> Just . head $ reverse xs
        b =rightHead (fromFoldable xs)
        in b == val

    , testProperty "join == ++" $ \(xs, ys) ->
        toList (fromFoldable xs `join` fromFoldable ys) == (xs ++ (ys:: [Int]))
    ]

instance Arbitrary a => Arbitrary (JoinList a) where
    arbitrary = do
        a <- arbitrary
        oneof [
            elements [Empty, Singleton a],
            pure (cons a) <*> arbitrary
            ]
