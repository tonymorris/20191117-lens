{-# LANGUAGE RankNTypes #-}

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
-- modifyPersonStreetNumber :: Functor f => (Int -> f Int) -> Person -> f Person
modifyPersonStreetNumber :: Lens' Person Int
-- this will make Ed happy :)
-- address ::      Lens' Person Address
-- streetNumber :: Lens' Address Int
-- _ ::            Lens' Person Int
modifyPersonStreetNumber = address.streetNumber

setPersonStreetNumber :: Int -> Person -> Person
setPersonStreetNumber = modify modifyPersonStreetNumber . const

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

type Lens' s a =
  Lens s s a a

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




