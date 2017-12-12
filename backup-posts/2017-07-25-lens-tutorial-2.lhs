fst snd
Data.Bifunctor first second
data Lens s a 
  = Lens
  { getter :: s -> a
  , setter :: a -> s -> s
  }

Data.Functor.Compose
Bifunctor

data OldLens s a = OldLens
  { get    :: s -> a
  , modify :: (a -> a) -> s -> s }

(@.) :: OldLens b c -> OldLens a b -> OldLens a c
(@.) _c _b = OldLens get' modify'
  where
    get' a = let b = get _b a
             in  get _c b

class Category cat where
  -- An “identity element” – doesn't change anything when composed.
  id :: cat a a
  -- Composition – if you replace “cat” with e.g. “~>”, its type becomes
  -- “(b ~> c) -> (a ~> b) -> (a ~> c)”.
  (.) :: cat b c -> cat a b -> cat a c
  
instance Category OldLens where
  id = OldLens id id
  (.) = (@.)

---

_all :: Eq a => a -> Lens' [a] a
_all ref = lens get set
  where
    get s     = ref
    set s new = map (\old -> if old == ref then new else old) s

_all' :: Eq a => a -> AppLens' [a] a
_all' ref f s = traverse update s
  where
    update old = if old == ref then f old else pure old

type AppLens s t a b = Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a

view :: Lens s t a b -> s -> a
over :: Lens s t a b -> (a -> b) -> s -> t
set  :: Lens s t a b -> b -> s -> t

view uses only Const
-- old type:
--      ((a ->    f    a) -> s ->    f    s) -> s -> a
view :: ((a -> Const a a) -> s -> Const a s) -> s -> a

over uses only Identity
-- old type:
--      ((a ->     f    b) -> s ->     f    t) -> (a -> b) -> s -> t
over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t

type Getting s a = (a -> Const a a) -> s -> Const a s 
type Setting s t a b = (a -> Identity b) -> s -> Identity t

view :: Getting s a -> s -> a
over :: Setting s t a b -> (a -> b) -> s -> t
set  :: Setting s t a b -> b -> s -> t

view :: Getting s a -> s -> a
-- view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view l = getConst . l Const

preview
  :: ((a -> Const (First a) a) -> s -> Const (First a) s)
  -> s
  -> Maybe a
preview l = getFirst . getConst . l (\x -> Const (First (Just x)))


newtype Any = Any Bool

has :: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has l = getAny . getConst . l (\_ -> Const (Any True))



type Getting r s a = (a -> Const r a) -> s -> Const r s
-- type Getting r s a = (a -> r) -> s -> r

view     :: Getting  a        s a -> s -> a
toListOf :: Getting [a]       s a -> s -> [a]
preview  :: Getting (First a) s a -> s -> Maybe a
has      :: Getting Any       s a -> s -> Bool




Data.Monoid.Endo
Endo is a newtype wrapper that lets us use the fact that functions of type a -> a form a monoid.
Monoid b => a -> b, too, form a monoid – the same type can't have 2 instances 
of the same typeclass, and this instance is more important than the Endo one.) 
What monoid? Well, id is the identity element, and . is the binary operation, 
nothing interesting.
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = (`appEndo` []) . getConst . l (\x -> Const (Endo (x:)))

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []

{-# LANGUAGE
  MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  
instance T.Traversable t => Each (t a) (t b) a b where
  each = T.traverse

instance Traversable ((,) a) where
  traverse f (x, y) = (x, ) <$> f y



Getting and Setting (view, set, over) for lenses and traversal

-- (a -> r) -> s -> r
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- (a -> b) -> s -> r
type Setting s t a b = (a -> Identify b) -> s -> Identity t

view :: Getting a s a -> s -> a
over :: Setting s t a b -> (a -> b) -> s -> t
set  :: Setting s t a b -> b -> s -> t

type Traversal s t a b = Applicative f => (a -> f b) -> s -> f t
type SimpleTraversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

data Name = Name { first :: String, last :: String }

traverseName :: Applicative f => (String -> f String) -> Name -> f Name
traverseName inj (Name nf nl) = Name <$> inj nf <*> inj nl

traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traverse f = sequenceA . fmap f

set traverse 8 [1,2,3]

instance Traversable [] where
    {-# INLINE traverse #-} -- so that traverse can fuse
    traverse f = List.foldr cons_f (pure [])
      where cons_f x ys = liftA2 (:) (f x) ys
      
_head :: Traversal' [a] a
_head f []     = pure []
_head f (x:xs) = (:) <$> f x <*> pure xs

views :: (Profunctor p, MonadReader s m) => Optical p (->) (Const r) s s a a -> p a r -> m r
views l f = Reader.asks (getConst #. l (Const #. f))

elementOf
elementsOf


views 

views :: SimpleTraversal s a -> s -> a
views l f = getConst . l (Const . f)

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b
  default each :: (Traversable g, s ~ g a, t ~ g b) => Traversal s t a b
  each = traverse
  {-# INLINE each #-}
