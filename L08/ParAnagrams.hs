module L08.ParAnagrams where

import Data.Char
import Data.List
import Data.Function
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
parAnagrams ::
  String
  -> FilePath
  -> IO [String]
parAnagrams name f =
  (flip (filter . flip S.member) (permutations name) . S.fromList . lines) `fmap` readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

