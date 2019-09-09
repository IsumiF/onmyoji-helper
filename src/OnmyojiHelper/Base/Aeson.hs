module OnmyojiHelper.Base.Aeson
  ( module Data.Aeson
  , removeNsOptions
  ) where

import           Data.Aeson

-- | Options that remove field namespace
removeNsOptions :: Options
removeNsOptions = defaultOptions { fieldLabelModifier = removeFieldNs }

-- | Remove the namespace part from a record field.
--
-- Example field names: @_user_name@ @_directory_uploaderName@.
-- The first underscore is mandantory.
removeFieldNs :: String -> String
removeFieldNs = tail . dropWhile (/= '_') . tail
