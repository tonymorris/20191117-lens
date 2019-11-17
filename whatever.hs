{-# LANGUAGE RankNTypes #-}

-- Map
import Data.Map(Map)
-- e.g. Map.insert
import qualified Data.Map as Map(insert, lookup, delete)

data Address =
  Address
    Int -- street number
    String -- street name
  deriving (Eq, Show)

-- this will make Ed happy
modifyStreetNumber :: (Int -> Int) -> Address -> Address
modifyStreetNumber = \f -> \(Address sn sm) -> Address (f sn) sm

moveNextDoor = modifyStreetNumber (+1)

data Identity a = Identity a
  deriving (Eq, Show)

runIdentity :: Identity a -> a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap = \f -> \(Identity a) -> Identity (f a)

streetNumber :: Lens' Address Int
-- modifyStreetNumber2 = \f -> \(Address sn sm) -> (\i -> fmap (\sn' -> Address sn' sm) i) (f sn)
streetNumber = \f -> \(Address sn sm) -> fmap (\sn' -> Address sn' sm) (f sn)

modifyStreetNumber3 :: (Int -> Int) -> Address -> Address
modifyStreetNumber3 = \f -> \a ->
  -- runIdentity (streetNumber ((\i2i -> \n -> Identity (i2i n)) f) a)
  runIdentity (streetNumber (Identity . f) a)

modifyStreetNumber4 :: (Int -> Int) -> Address -> Address
modifyStreetNumber4 = modify streetNumber

data Person =
  Person
    String -- name
    Address
  deriving (Eq, Show)

-- address :: Functor f => (Address -> f Address) -> Person -> f Person
address :: Lens' Person Address
-- modifyPersonAddress :: (Address -> Identity Address) -> Person -> Identity Person
address = \f -> \(Person n a) -> fmap (\a' -> Person n a') (f a)

modifyPersonAddress3 :: (Address -> Address) -> Person -> Person
modifyPersonAddress3 =
  \a2a p -> runIdentity (address (Identity . a2a) p)

modifyPersonAddress4 :: (Address -> Address) -> Person -> Person
modifyPersonAddress4 = modify address

-- over
modify :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
modify k = \f a -> runIdentity (k (Identity . f) a)

-- modifyStreetNumber2 :: Functor f => (Int -> f Int) -> Address -> f Address
-- modifyPersonAddress :: Functor f => (Address -> f Address) -> Person -> f Person
-- personStreetNumber :: Functor f => (Int -> f Int) -> Person -> f Person
personStreetNumber :: Lens' Person Int
-- this will make Ed happy :)
-- address ::      Lens' Person Address
-- streetNumber :: Lens' Address Int
-- _ ::            Lens' Person Int
personStreetNumber = address.streetNumber

setPersonStreetNumber :: Int -> Person -> Person
setPersonStreetNumber = set personStreetNumber

set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set k = modify k . const

getPersonAddress :: Person -> Address
-- use address :: Functor f => (Address -> f Address) -> Person -> f Person
-- use address :: (Address -> Const a Address) -> Person -> Const a Person
-- getPersonAddress = \p -> runConst (address Const p)
getPersonAddress = runConst . address Const
-- Const a Person -> Address
-- Const Address Person -> Address

getAddressStreetNumber :: Address -> Int
getAddressStreetNumber = runConst . streetNumber Const

-- view
get :: ((a1 -> Const a1 b1) -> a2 -> Const c b2) -> a2 -> c
get k = runConst . k Const

data Const a b = Const a

runConst :: Const a b -> a
runConst (Const x) = x

instance Functor (Const a) where
  fmap = \_ -> \(Const x) -> Const x

type Lens s t a b =
  forall f. Functor f => (a -> f b) -> s -> f t

-- this should always return True
getsetLaw :: Eq s => Lens' s a -> s -> Bool
getsetLaw l s = set l (get l s) s == s

-- this should always return True
setgetLaw :: Eq a => Lens' s a -> s -> a -> Bool
setgetLaw l s a =
  get l (set l a s) == a

-- this should always return True
setsetLaw l s x1 x2 =
  set l x2 (set l x1 s) == set l x2 s

type Lens' s a =
  Lens s s a a

foo :: Functor f => (a -> f b) -> s -> f t
foo = error "Ed rant"

data OldLens s t a b = OldLens (s -> (a, b -> t))


-- A Person has exactly 1 Address (**and** some other stuff)
-- An Address has exactly 1 Int street number (**and** some other stuff)

-- Map.insert :: Ord k => k -> a -> Map k a -> Map k a 
-- Map.delete :: Ord k => k -> Map k a -> Map k a 
-- Map.lookup :: Ord k => k -> Map k a -> Maybe a 

at :: Ord k => k -> Lens' (Map k v) (Maybe v)
at = \k -> undefined

{-
blah ::
  Functor f =>
  ((Address -> f Address) -> (Person -> f Person))
  -> ((Int -> f Int) -> (Address -> f Address))
  -> (Int -> f Int) -> (Person -> f Person)


{-
eta-reduction
  \x -> f x
  f

composition
  \x -> f (g x)
  f . g
-}
-- blah = \f -> \g -> \i2fi -> \p -> f (g i2fi) p
-- blah = \f -> \g -> \i2fi -> f (g i2fi)
-- blah = \f -> \g -> f . g
-- blah = \f -> \g -> (.) f g
-- blah = \f -> (.) f
blah = (.)
-}




