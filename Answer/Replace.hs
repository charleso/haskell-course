#!/usr/bin/runhaskell

-- | Replaces the answers throughout the haskell source files and is intended to replace those
-- answers with a substitute expression that passes the compiler (e.g. /error "todo"/).
module Answer.Replace where

import Data.Char
import Data.List.Split
import System.FilePath
import System.Directory
import System.IO
import System.Environment

-- | A line of text usually in a list of lines.
type Line = String

-- | Replace the given line with a list of lines.
type Replacer = Line -> [Line]

-- | Runs the line replacer using the given input/output handles. Neither are closed.
withLines ::
  Replacer -- ^ The replacement function to run on each line.
  -> Handle -- ^ The input handle.
  -> Handle -- ^ The output handle.
  -> IO ()
withLines f i o =
  do j <- hGetContents i
     let z = unlines (f =<< lines j)
     hPutStr o z

-- | Runs the line replacer on a file by first creating a temporary file
-- then copying over the original.
withLinesInPlace ::
  Replacer -- ^ The replacement function to run on each line.
  -> FilePath -- ^ The file to run the replacement function on.
  -> IO ()
withLinesInPlace f z =
  do i <- openFile z ReadMode
     (p, o) <- openTempFile (dropFileName z) ('.' : takeFileName z)
     withLines f i o
     hClose o
     hClose i
     renameFile p z

-- | A line replacer that searches for a triple-dash (/---/) and replaces the line
-- with everything following. If there is nothing (or only whitespace) following, then
-- the line is deleted.
tripleDash ::
  Replacer
tripleDash line =
  case "---" `splitOn` line
    of [x]     -> [x]
       (_:z:t) -> let y = concat (z:t)
                  in if null (dropWhile isSpace y)
                       then []
                       else [y]
       []      -> error "invariant not met (splitOn returned [])"

-- | Runs the 'tripleDash' replacer on a file by first creating a temporary file
-- then copying over the original.
tripleDashInPlace ::
  FilePath -- ^ The file to run the replacement function on.
  -> IO ()
tripleDashInPlace =
  withLinesInPlace tripleDash

-- | Reads one or more command-line arguments (filenames) and runs 'tripleDashInPlace' on those files.
main :: IO ()
main =
  do a <- getArgs
     case a
       of [] -> print "Enter 1 or more file names to replace answers"
          _  -> mapM_  (\p -> tripleDashInPlace p >> putStrLn p) a
