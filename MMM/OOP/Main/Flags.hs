{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-|
Module      :  Flags
Description :  Command-line options
Licence     :  MIT

Maintainer  :  victor.cacciari@gmail.com
Stability   :  experimental
Portability :  portable
-}
module MMM.OOP.Main.Flags (
    Options(..)
  , getOpts
  , showHelp
  ) where
  
import System.Console.CmdArgs
import System.Environment

data Options
  = Compile { ocInput  :: FilePath
            , ocOutput :: Maybe FilePath
            , ocDump   :: Bool
            , ocOmmitLinePs :: Bool
            }
  | Help
  deriving (Show, Data, Typeable)
            
compile :: Options
compile = Compile
  { ocInput  = def &= args 
                   &= typ "FILE" 
                   
  , ocOutput = def &= explicit
                   &= name "output" 
                   &= typ "FILE" 
                   &= help "Output file. Will use module name by default."
                   
  , ocDump   = def &= explicit
                   &= name "dump-code" 
                   &= typ "BOOL" 
                   &= help "Outputs the code to stdout after simplification."
                   
  , ocOmmitLinePs = def &= explicit
                        &= name "ommit-line-pragmas"
                        &= typ "BOOL" 
                        &= help "Ommits line pragmas from the output file. Usefull for debugging the Haskell code."
  } &= help "OOP to MMM compiler options."
    &= name "comp"
  
mhelp :: Options
mhelp = Help &= help "Display Help"

mode :: Mode (CmdArgs Options)
mode = cmdArgsMode $ modes [compile &= auto, mhelp]
     &= help "OOP to MMM Compiler"
     &= summary "oop2mmm v0.1\nVictor Miraldo <victor.cacciari@gmail.com>\nUniversity of Minho\n\n"
     

showHelp :: IO ()
showHelp = do
  _ <- withArgs ["--help"] $ cmdArgsRun mode
  return ()
  
  
getOpts :: IO Options
getOpts = getArgs >>= doGetOpts
  where
    doGetOpts as
      | null as   = withArgs ["--help"] $ cmdArgsRun mode
      | otherwise = cmdArgsRun mode
