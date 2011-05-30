module L08.FastAnagrams where

import Data.Char
import Data.List
import Data.Function
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  String
  -> FilePath
  -> IO [NoCaseString]
fastAnagrams name f =
    (flip (filter . flip S.member) (noCase (permutations name)) . S.fromList . noCase . lines) `fmap` readFile f
    where noCase = map NoCaseString

newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

toLower' :: NoCaseString -> [Char]
toLower' = (map toLower . ncString) 

instance Ord NoCaseString where
    compare = compare `on` toLower'

instance Eq NoCaseString where
  (==) = (==) `on` toLower'

instance Show NoCaseString where
  show = show . ncString
