import Data.Char
import Data.List
import Data.Function


{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO String
* lines :: String -> [String]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  String
  -> FilePath
  -> IO [String]
{-
anagrams name file = let contents = (readFile file)
                         rows = (fmap lines contents)
			 perms = (permutations name)
			 foo = fmap (intersectBy equalIgnoringCase perms) rows
                     in foo
-}
{-
anagrams name file = let contents = (readFile file)
			 perms = (permutations name)
                     in fmap (intersectBy equalIgnoringCase perms) (fmap lines contents)
-}
{-
anagrams name file = let contents = (readFile file)
			 perms = (permutations name)
                     in fmap ((intersectBy equalIgnoringCase perms) . lines) contents
-}
anagrams name = let perms = permutations name
                in fmap (intersectBy equalIgnoringCase perms . lines) . readFile

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  String
  -> String
  -> Bool
-- equalIgnoringCase s1 s2 = (fmap toLower s1) == (fmap toLower s2)
-- equalIgnoringCase = on (==) (fmap toLower)
equalIgnoringCase = (==) `on` fmap toLower
