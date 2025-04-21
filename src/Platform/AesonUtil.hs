module Platform.AesonUtil where

import Data.Aeson.TH
import Data.MonoTraversable (lastMay)
import Data.Text (splitOn)
import Language.Haskell.TH.Syntax
import RIO (concat, fromMaybe, mapM, show, ($), (.), (<$>), (<>))
import RIO.Text (pack, toLower, unpack)
import Prelude (drop, length)

commonJSONDeriveMany :: [Name] -> Q [Dec]
commonJSONDeriveMany names =
  concat <$> mapM commonJSONDerive names

commonJSONDerive :: Name -> Q [Dec]
commonJSONDerive name =
  let lowerCaseFirst (y : ys) = unpack (toLower (pack [y])) <> ys
      lowerCaseFirst "" = ""
      structName = unpack $ fromMaybe "" . lastMay . splitOn (pack ".") . pack . show $ name
   in deriveJSON defaultOptions {fieldLabelModifier = lowerCaseFirst . drop (length structName)} name
