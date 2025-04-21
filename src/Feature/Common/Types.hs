module Feature.Common.Types where

import Platform.AesonUtil
import RIO

data Pagination = Pagination
  { paginationLimit :: Int,
    paginationOffset :: Int
  }
  deriving (Eq, Show)

type InputViolations = Map Text [Text]

newtype ErrorsWrapper a = ErrorsWrapper {errorsWrapperErrors :: a} deriving (Eq, Show)

$( commonJSONDeriveMany
     [ ''ErrorsWrapper
     ]
 )
