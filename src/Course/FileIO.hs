{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = run =<< filepath
  where filepath :: IO FilePath
        filepath = headOr (error "No file argument found.") <$> getArgs

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> IO ()
run filepath = printFiles =<< files
  where file :: IO (FilePath, Chars)
        file = getFile filepath

        filepaths :: IO (List FilePath)
        filepaths = (lines . snd) <$> file

        files :: IO (List (FilePath, Chars))
        files = getFiles =<< filepaths

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles filepaths = sequence $ map getFile filepaths

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile filepath = (\c -> pure (filepath, c)) =<< contents
  where contents :: IO Chars
        contents = readFile filepath

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles files = foldLeft (\a (f, c) -> (\_ -> printFile f c) =<< a
                            ) (return ()) files

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile filepath contents = (\_ -> putStrLn contents) =<< putStrLn ("========= " ++ filepath)
