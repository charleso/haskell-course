#!/usr/bin/runhaskell

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
  Replacer
  -> Handle
  -> Handle
  -> IO ()
withLines f i o =
  do j <- hGetContents i
     let z = unlines (f =<< lines j)
     hPutStr o z

withLinesInPlace ::
  Replacer
  -> FilePath
  -> IO ()
withLinesInPlace f z =
  do t <- getTemporaryDirectory
     i <- openFile z ReadMode
     (p, o) <- openTempFile t (takeFileName z)
     withLines f i o
     hClose o
     hClose i
     renameFile p z

tripleDash ::
  Replacer
tripleDash line =
  case "---" `splitOn` line
    of [x]     -> [x]
       (_:z:t) -> let y = concat (z:t)
                      v      = dropWhile isSpace y
                  in if null v
                       then []
                       else [y]
       []      -> error "invariant not met"

tripleDashInPlace ::
  FilePath
  -> IO ()
tripleDashInPlace =
  withLinesInPlace tripleDash

main :: IO ()
main =
  do a <- getArgs
     case a
       of [] -> print "Enter 1 or more file names to replace answers"
          _  -> mapM_  (\p -> tripleDashInPlace p >> putStrLn p) a

