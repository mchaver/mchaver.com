class Functor f where
  fmap :: (a -> b) -> f a -> f b

## Covariant functor

`fmap` preserves the direction of the arrows
     g ::   a ->   b
fmap g :: f a -> f b
both arrows point to the right

modify the output

## Contravariant functor

class Contravariant f where
  contramp :: (b -> a) -> f a -> f b
  
contramap swaps the direction of the arrows
          
          g ::   a <-   b
contramap g :: f a -> f b

modify the input

data Pred a = Pred {unpred :: a -> Bool }


positive values are introduced by construction
negative values are introduced by need

```
contramap id = id
contramap f . contramap g = contramap (g . f)
  contramap :: (a -> b) -> f b -> f a
```


Profunctor

```
class Profunctor f where
  dimap ∷ (c -> a) -> (b -> d) -> f a b -> f c d

      g   ::   a   <-   c
        h ::     b ->     d
dimap g h :: f a b -> f c d
```


https://www.reddit.com/r/haskell/comments/1vc0mp/whats_up_with_contravariant/
https://www.reddit.com/r/haskell/comments/4rvtzy/what_is_the_motivation_behind_contravariant/
http://blog.sigfpe.com/2011/07/profunctors-in-haskell.html
https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors
https://ocharles.org.uk/blog/guest-posts/2013-12-22-24-days-of-hackage-profunctors.html


import Control.Applicative
import Data.Functor.Contravariant
import Data.Profunctor

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap g (Predicate p) = Predicate (p . g)

veryOdd ∷ Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

main ∷ IO ()
main = print $ getPredicate veryOdd <$> [0..11]

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGAUGE RankNTypes #-}

data L a b = forall s. L (s -> b) (s -> a -> s) s

instance Profunctor L where
  dimap h f (L result iterate initial) =
    L (f . result) (\s -> iterate s . h) initial

a is contravariant
b is covariant
data S a b = S (a -> Bool) (Maybe b)

instance Profunctor S where
  dimap h f (S pred res) =
    S (pred . h) (f <$> res)
    
runS :: S a b -> Int -> String
runS (S af b) = case

S odd (Just "Hi")

runS :: S a b -> a -> Either String (Maybe b)
runS (S af b) a =
  case af a of
    True  -> Right b
    False -> Left "it didnt work"
    
type with two type parameters
first parameter is contravariant 
second parameter is covariant
instance of Profunctor on type
a way to extract things from the type

https://www.stackbuilders.com/news/the-weak-and-the-strong-functors

ddd :: Maybe Int -> Bool
ddd (Just n)
  | n > 0 = True
  | n < 0 = False
ddd _ = False

let c = Costar ddd
runCostar (dimap id id c) (Just 1)

Choice 
Costar
