module OnmyojiHelper.Base.Monad
  ( liftMaybe
  ) where

import           Control.Monad.Trans.Maybe
import           Data.Functor.Identity

liftMaybe :: Maybe a -> MaybeT Identity a
liftMaybe = MaybeT . Identity
