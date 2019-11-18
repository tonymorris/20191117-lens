{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

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

instance Applicative Identity where
  pure = \a -> Identity a
  Identity f <*> Identity a =
    Identity (f a)

instance Monoid a => Applicative (Const a) where
  pure = \_ -> Const mempty
  Const f <*> Const a =
    Const (f `mappend` a)

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

-- contains :: Ord a => a -> Lens' (Set a) Bool
-- set . contains :: Ord a => a -> Bool -> Set a -> Set a

at :: Ord k => k -> Lens' (Map k v) (Maybe v)
at = \k -> \p -> \m ->
      fmap
        {-
        (
          \case
            Nothing -> _
            Just v -> _
        )
        -}
        (maybe (Map.delete k m) (\v -> Map.insert k v m))
        (p (Map.lookup k m))

data Company =
  Company
    Person -- ceo
    Person -- cto
    Person -- cfo
    [Person] -- employees
    String -- name
  deriving (Eq, Show)

companyCeo ::
  Functor f => (Person -> f Person) -> Company -> f Company
companyCeo =
  undefined

companyEmployees ::
  Functor f => ([Person] -> f [Person]) -> Company -> f Company
companyEmployees =
  undefined

-- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
-- traverse :: Applicative f => (Person -> f Person) -> [Person] -> f [Person]

type Iso s t a b = forall p f. (Profunctor p, Functor f) => 
  p a (f b) -> p s (f t)

type Iso' s a =
  Iso s s a a

data Star f a b = Star (a -> f b)

instance Functor f => Profunctor (Star f) where
  dimap b2a c2d (Star a2fc) = Star $ \b -> fmap c2d $ a2fc (b2a b)

-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b


iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso s2a b2t pafb = dimap s2a (fmap b2t) pafb

-- unget :: Iso s t a b -> b -> t

type Traversal s t a b =
  forall f. Applicative f =>
  (a -> f b) -> s -> f t

type Traversal' s a =
  Traversal s s a a

companyPersons ::
  -- Applicative f => (Person -> f Person) -> Company -> f Company
  Traversal' Company Person
companyPersons =
  \f -> \(Company ceo cto cfo es n) ->
    Company <$>
      f ceo <*>
      f cto <*>
      pure cfo <*>
      traverse f es <*>
      pure n

moveAllPersonsNextDoor ::
  Company -> Company
moveAllPersonsNextDoor =
  modify (companyPersons.address.streetNumber) (+1)

testCompany ::
  Company
testCompany =
  Company
    (Person "Bob" (Address 23 "street st"))
    (Person "Mary" (Address 12 "unstreet st"))
    (Person "Fred" (Address 4 "stop st"))
    [
      Person "Bill" (Address 99 "start st")
    , Person "Alice" (Address 998 "dzfhg st")
    ]
    "ACME Industries"

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- (<$>) :: Functor f => (a -> b) -> f a -> f b

class Profunctor f where
  dimap ::
    (b -> a) -> (c -> d) -> f a c -> f b d

data Tagged a b = Tagged b
-- data PrettyParse a b = Crazy (a -> Doc) (Parser b)

class Profunctor p => Choice p where
  left :: p a b -> p (Either a c) (Either b c)

instance Choice (->) where
  left a2b (Left a) = Left (a2b a)
  left a2b (Right c) = Right c 

-- review :: Prism s t a b -> b -> t

class Profunctor p => Strong p where
  first :: p a b -> p (a, c) (b, c)

type Prism s t a b = forall p f. (Choice p, Applicative f) => 
  p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

prism :: (b -> t) -> (s -> Either a t) -> Prism s t a b
prism b2t seat pafb = dimap seat (\case
    Left fb -> fmap b2t fb
    Right t -> pure t 
  ) $ left pafb

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ \case
  Left a -> Left a
  Right c -> Right (Right c)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ \case
  Left c -> Right (Left c)
  Right a -> Left a 

-- (#) :: Prism s t a b -> b -> t

-- _Left._Right # 12
-- Left (Right 12)

doNothing :: Traversal s s a b
doNothing a2fb s = pure s 

idLens :: Iso s t s t
idLens = id


-- S_i = [i]   A_i = i
-- traverse :: Traversal [a] [b] a b
both :: Traversal (a,a) (b,b) a b
both a2b (a1,a2) = (,) <$> a2b a1 <*> a2b a2

-- data Match 

-- project :: Prism s t a b -> s -> Either t a

-- embed :: Prism s t a b -> b -> t

instance Functor f => Strong (Star f) where
  first (Star a2fb) = Star $ \(a,c) -> fmap (\b -> (b,c)) $ a2fb a

-- type Lens s t a b = forall p. Strong p => p a b -> p s t


instance Profunctor (->) where
  dimap b2a c2d a2c = c2d . a2c . b2a

leftMap :: Profunctor f => (b -> a) -> f a x -> f b x
leftMap f = dimap f id

data FancyCompany =
  FancyCompany
    Company
  deriving (Eq, Show)

fancyCompany :: Iso' FancyCompany Company 
fancyCompany =
  iso
    (\(FancyCompany x) -> x)
    FancyCompany

identity :: Iso (Identity a) (Identity b) a b
identity =
  iso
   runIdentity
   Identity

{-

Ed afternoon rant
-----------------

// b -> t, s -> Either t a
Prism     s t a b = (Choice p, Applicative f) => p a (f b) -> p s (f t)

// s -> a, s -> b -> t
Lens      s t a b = (p ~ (->)    , Functor f) => p a (f b) -> p s (f t)

// s -> a, b -> t
Iso       s t a b = (Profunctor p, Functor f) => p a (f b) -> p s (f t)


Traversal s t a b = (p ~ (->), Applicative f) => p a (f b) -> p s (f t)

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b

newtype Predicate a = Predicate (a -> Bool)

instance Contravariant Predicate where
  fmap b2a (Predicate a2bool) = Predicate $ \b -> a2bool $ b2a b

// s -> a
type Getter s a = (Functor f, Contravariant f) =>
  (a -> f a) -> s -> f s

// s -> [a]
type Fold s a = (Applicative f, Contravariant f) => 
  (a -> f a) -> s -> f s

-}


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




