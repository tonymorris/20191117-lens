{-# Language TemplateHaskell #-}
{-# Language RankNTypes #-}
{-# Language DeriveDataTypeable #-}
{-# Language LambdaCase #-}
import Control.Lens
import Data.Data
import Control.Monad.State
import Data.Map(Map)
-- e.g. Map.insert
import qualified Data.Map as Map


data Foo = Foo { _fooBar :: Int, _fooBaz :: Double }
  deriving Show

makeClassy ''Foo

data BigFoo = BigFoo { _bigFooSize :: Int, _bigFooFoo :: Foo }
  deriving Show
 
makeClassy ''BigFoo

instance HasFoo BigFoo where
  foo = bigFooFoo  

blah1 = runState (do pure ()) $ BigFoo 12 (Foo 34 56)

blah2 = runState (do fooBar += 2; fooBaz -= 2) $ BigFoo 12 (Foo 34 56)


data Json
  = Num Double
  | Str String
  | Array [Json]
  | Object (Map String Json)
  | Boolean Bool
  | Null
  deriving (Show,Data)

makePrisms ''Json

instance Plated Json where
  plate f (Array xs) = Array <$> traverse f xs
  plate f (Object xs) = Object <$> traverse f xs 
  plate f x  = pure x

-- makePrisms ''Json

key :: String -> Traversal' Json Json
key k = _Object.at k.traverse

bar :: Traversal' Json Json
bar = key "k1" . key "k2"
-- _Object.at "key".traverse._Object.at "key2".traverse

{- Ed ghci session

λ> :t (+=)
(+=)
  :: (Num a, Control.Monad.State.Class.MonadState s m) =>
     ASetter' s a -> a -> m ()

λ> :t toListOf
toListOf :: Getting (Data.Monoid.Endo [a]) s a -> s -> [a]

λ> :t toListOf traverse [1,2,3]
toListOf traverse [1,2,3] :: Num a => [a]

λ> toListOf traverse [1,2,3]
[1,2,3]

λ> toListOf traverse [1,2,3]

λ> :i Getting
type Getting r s a = (a -> Const r a) -> s -> Const r s
        -- Defined in ‘Control.Lens.Getter’

λ> :t filtered
filtered
  :: (Applicative f, Choice p) => (a -> Bool) -> Optic' p f a a

λ> :t traverse . filtered
traverse . filtered
  :: (Applicative (p a), Applicative f, Choice p, Traversable t) =>
     (a -> Bool) -> t (p a (f a)) -> p a (t (f a))

λ> :t over (traverse . filtered undefined)
over (traverse . filtered undefined)
  :: Traversable t => (a -> a) -> t a -> t a

λ> :t over (traverse . filtered even)
over (traverse . filtered even)
  :: (Integral a, Traversable t) => (a -> a) -> t a -> t a

λ> :t over (traverse . filtered even) (+1)
over (traverse . filtered even) (+1)
  :: (Integral a, Traversable t) => t a -> t a

λ> :t over (traverse . filtered even) (+1) [5..10]
over (traverse . filtered even) (+1) [5..10] :: Integral a => [a]

λ> over (traverse . filtered even) (+1) [5..10]
[5,7,7,9,9,11]

λ> -- over f . over g = over (f . g)

λ> -- over l f . over l g = over l (f . g)

λ> -- over l id = id

λ> over (traverse . filtered even) ((+1).(+1)) [5..10]
[5,8,7,10,9,12]

λ> over (traverse . filtered even) (+1) . over (traverse . filtered even) (+1) $ [5..10]
[5,7,7,9,9,11]

λ> Object (Map.singleton "key" $ Array [Array [Num 5, Num 6], Num 7])^..biplate :: [Double]
[5.0,6.0,7.0]

λ> :t rewrite
rewrite :: Plated a => (a -> Maybe a) -> a -> a

λ> :t transform
transform :: Plated a => (a -> a) -> a -> a

λ> -- rewrite $ \case Neg (Lit i) -> Just (Lit (negate i); _ -> Nothing

λ> :t rewriteOf
rewriteOf :: ASetter a b a b -> (b -> Maybe a) -> a -> b

-}

