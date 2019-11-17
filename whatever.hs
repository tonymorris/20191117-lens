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

modifyStreetNumber4 :: (Int -> Int) -> Address -> Address
modifyStreetNumber4 = modify modifyStreetNumber2

data Person =
  Person
    String -- name
    Address
  deriving (Eq, Show)

modifyPersonAddress :: Functor f => (Address -> f Address) -> Person -> f Person
-- modifyPersonAddress :: (Address -> Identity Address) -> Person -> Identity Person
modifyPersonAddress = \f -> \(Person n a) -> fmap (\a' -> Person n a') (f a)

modifyPersonAddress3 :: (Address -> Address) -> Person -> Person
modifyPersonAddress3 =
  \a2a p -> runIdentity (modifyPersonAddress (Identity . a2a) p)

modifyPersonAddress4 :: (Address -> Address) -> Person -> Person
modifyPersonAddress4 = modify modifyPersonAddress

-- over
modify :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
modify k = \f a -> runIdentity (k (Identity . f) a)

-- modifyStreetNumber2 :: Functor f => (Int -> f Int) -> Address -> f Address
-- modifyPersonAddress :: Functor f => (Address -> f Address) -> Person -> f Person
modifyPersonStreetNumber :: Functor f => (Int -> f Int) -> Person -> f Person
-- this will make Ed happy :)
modifyPersonStreetNumber = blah modifyPersonAddress modifyStreetNumber2

blah ::
  Functor f =>
  ((Address -> f Address) -> Person -> f Person)
  -> ((Int -> f Int) -> Address -> f Address)
  -> (Int -> f Int)
  -> Person
  -> f Person
blah = _
