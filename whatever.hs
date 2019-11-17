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

modifyStreetNumber2 :: Functor f => (Int -> f Int) -> Address -> f Address
-- modifyStreetNumber2 = \f -> \(Address sn sm) -> (\i -> fmap (\sn' -> Address sn' sm) i) (f sn)
modifyStreetNumber2 = \f -> \(Address sn sm) -> fmap (\sn' -> Address sn' sm) (f sn)

modifyStreetNumber3 :: (Int -> Int) -> Address -> Address
modifyStreetNumber3 = \f -> \a ->
  -- runIdentity (modifyStreetNumber2 ((\i2i -> \n -> Identity (i2i n)) f) a)
  runIdentity (modifyStreetNumber2 (Identity . f) a)

data Person =
  Person
    String -- name
    Address
  deriving (Eq, Show)

-- modifyPersonAddress :: Functor f => (Address -> f Address) -> Person -> f Person
modifyPersonAddress :: (Address -> Identity Address) -> Person -> Identity Person
modifyPersonAddress = _


