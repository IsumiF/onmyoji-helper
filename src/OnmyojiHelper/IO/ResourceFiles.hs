{-# LANGUAGE FlexibleInstances #-}

module OnmyojiHelper.IO.ResourceFiles
  ( MonadResourceFiles(..)
  , HasResourceDir(..)
  ) where

import           Control.Monad.Reader
import qualified Data.ByteString      as BS
import           System.FilePath      ((</>))

class Monad m => MonadResourceFiles m where
  readResourceFile :: FilePath -> m BS.ByteString

class HasResourceDir env where
  getResourceDir :: env -> FilePath

instance HasResourceDir env => MonadResourceFiles (ReaderT env IO) where
  readResourceFile file = do
    dir <- asks getResourceDir
    liftIO . BS.readFile $ dir </> file
