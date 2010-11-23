module L08.ParAnagrams where

import Data.Char
import Data.List
import Data.Function
import Control.Parallel.Strategies
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
parAnagrams ::
  Strategy String
  -> String
  -> FilePath
  -> IO [String]
parAnagrams s name f =
  (flip (parFilter s . flip S.member) (permutations name) . S.fromList . lines) `fmap` readFile f


parFilter ::
  Strategy a
  -> (a -> Bool)
  -> [a]
  -> [a]
parFilter s p x =
  filter p x `using` parList s

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

