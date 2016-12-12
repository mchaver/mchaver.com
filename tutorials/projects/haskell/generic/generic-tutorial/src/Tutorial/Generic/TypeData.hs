{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}



module Tutorial.Generic.TypeData where

import Data.Data
import Data.Int
import Data.Proxy
import GHC.Generics

--data Record = Record { recordId :: Int32, recordName :: ByteString }
--  deriving (Generic)

class Selectors rep where
  selectors :: Proxy rep -> [(String, TypeRep)]

instance Selectors f => Selectors (M1 D x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance Selectors f => Selectors (M1 C x f) where
  selectors _ = selectors (Proxy :: Proxy f)

instance (Selector s, Typeable t) => Selectors (M1 S s (K1 R t)) where
  selectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeOf (undefined :: t) ) ]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
  selectors _ = selectors (Proxy :: Proxy a) ++ selectors (Proxy :: Proxy b)

instance Selectors U1 where
  selectors _ = []




class Selectors2 rep where
  selectors2 :: Proxy rep -> [String]

-- pattern matching at the instances level

-- M1
-- D, C S
-- K1
-- U1 no tags
instance Selectors2 f => Selectors2 (M1 D x f) where
  selectors2 _ = selectors2 (Proxy :: Proxy f)

instance Selectors2 f => Selectors2 (M1 C x f) where
  selectors2 _ = selectors2 (Proxy :: Proxy f)

instance (Selector s, Selectors2 rep) => Selectors2 (M1 S s rep) where
  selectors2 _ =
    [ selName (undefined :: M1 S s rep ()) ] ++ selectors2 (Proxy :: Proxy rep)

instance Selectors2 (K1 R t) where
  selectors2 _ = []
--instance (Selectors2 a) => Selectors2 (K1 i a) where
--  selectors2 _ = [] ++ selectors2 (Proxy :: Proxy a)
  -- base type

instance (Selectors2 l, Selectors2 r) => Selectors2 (l :+: r) where
  selectors2 _ = selectors2 (Proxy :: Proxy l) ++ selectors2 (Proxy :: Proxy r)
    --(++) <$> (L1 <$> selectors2 (Proxy :: Proxy a)) <*> (R1 <$> selectors2 (Proxy :: Proxy b))

instance (Selectors2 a, Selectors2 b) => Selectors2 (a :*: b) where
  selectors2 _ = selectors2 (Proxy :: Proxy a) ++ selectors2 (Proxy :: Proxy b)

instance Selectors2 U1 where
  selectors2 _ = [] -- return empty type
  -- base type

--gSelectors2 :: (Generic a, Selectors2 (Rep a)) => Proxy a -> [String]
--gSelectors2 = (fmap . fmap) to <$> selectors2 (Proxy :: Proxy a)
--gSelectors2 :: (Generic a, Selectors2 (Rep a)) => Proxy a -> [String]
--gSelectors2 _ = selectors2 (Proxy :: Proxy (Rep a))

--  genericArbitraryWithConList :: (Generic a, GArbitrary (Rep a), GArbitraryWithConList (Rep a)) => Gen [(String,a)]
--  genericArbitraryWithConList = (fmap . fmap) to <$> garbitraryWithConList


{-
DefaultSignatures
class TypeName a where
  typename :: Proxy a -> String

  default typename :: (Generic a, GTypeName (Rep a)) => Proxy a -> String
  typename _proxy = gtypename (from (undefined :: a))

-- | Generic equivalent to `TypeName`.
class GTypeName f where
  gtypename :: f a -> String

--instance (Datatype c) => GTypeName (M1 i c f) where
--  gtypename m = datatypeName (undefined :: M1 i c f ())

instance (Datatype c) => GTypeName (M1 C c f) where
  gtypename m = datatypeName (undefined :: M1 C c f ())

instance (Datatype c) => GTypeName (M1 D c f) where
  gtypename m = datatypeName (undefined :: M1 D c f ())

instance (Datatype c) => GTypeName (M1 S c f) where
  gtypename m = datatypeName (undefined :: M1 S c f ())
-}
{- Inspect tree
data Sample = Sample1 Int | Sample2 Int String deriving (Eq,Show,Generic)
λ> :kind! Rep Sample ()
Rep Sample () :: *
= D1
    D1Sample
    (C1 C1_0Sample (S1 NoSelector (Rec0 Int))
     :+: C1
           C1_1Sample
           (S1 NoSelector (Rec0 Int) :*: S1 NoSelector (Rec0 String)))
    ()


:set -XDeriveGeneric
import GHC.Generics
data SumType = SumType1  Int
             | SumType2 String Int
             -- | SumType3  String [Int] Double
             -- | SumType4 String [String] [Int] Double
  deriving (Eq,Generic,Show)
:kind! Rep SumType ()
Rep SumType () :: *
= D1
    D1SumType
    (C1 C1_0SumType (S1 NoSelector (Rec0 Int))
     :+: C1
           C1_1SumType
           (S1 NoSelector (Rec0 String) :*: S1 NoSelector (Rec0 Int)))
    ()
-}
