{-|
Module      :  Main
Description :  Main module
Licence     :  MIT

Maintainer  :  victor.cacciari@gmail.com
Stability   :  experimental
Portability :  portable

Main module
-}
module Main where

import qualified MMM.OOP.Main.Flags as Fl

import MMM.Core.All

import MMM.OOP.Language.Syntax
import MMM.OOP.Language.Parser
import MMM.OOP.Language.Utils
import MMM.OOP.ToMMM

import MMM.HsMMM

import MMM.Util.Pretty
import MMM.Util.Util

import System.IO


main :: IO ()
main = do
  opts <- Fl.getOpts
  case opts of
    Fl.Help -> Fl.showHelp
    Fl.Compile {} -> compile opts
    
--------------------------------------------------------------------------------
-- * Compilation

-- ghci interface

compileIn :: FilePath -> IO ()
compileIn f = compile (Fl.Compile f Nothing True True)

compileInOut :: FilePath -> FilePath -> IO ()
compileInOut fin fout = compile (Fl.Compile fin (Just fout) True True)

inExamples :: String -> FilePath
inExamples = ("MMM/Examples/" ++)

-- Compilation Routine

compile :: Fl.Options -> IO ()
compile opts
  = do
    case Fl.ocOutput opts of
      Nothing -> compileOut
      Just f  -> withFile f WriteMode compileOutTo
  where
    compileOutTo hout
      = oopParse (Fl.ocInput opts) >>= either (putStrLn . show) (runCompilation $ Just hout)
      
    compileOut 
      = oopParse (Fl.ocInput opts) >>= either (putStrLn . show) (runCompilation Nothing)

    runCompilation hout m
      = do
        defs' <- mapM (simplifyAll . classDef) (moduleDecls m)
        let m' = m { moduleDecls = map (\(c, d) -> c { classDef = d }) (zip (moduleDecls m) defs') }
        when (Fl.ocDump opts) $ hPrettyPutStrLn stdout m'
        hsmod <- runErrorT $ trModule m'
        case hsmod of
            Left err -> hPutStrLn stderr $ show err
            Right m  -> let mname = hsmoduleGetName m ++ ".hs"
                        in case hout of
                          Nothing -> withFile mname WriteMode $ \out -> 
                                     mapM_ (hPutStrLn out . show . pretty) (hsmodOpts opts m)
                          Just o  -> mapM_ (hPutStrLn o . show . pretty)   (hsmodOpts opts m)
            
    hsmodOpts o m
      = if Fl.ocOmmitLinePs o
        then filter (not . hsStmtIsLineP) m
        else m
    
    hPrettyPutStrLn h a
      = do hPutStrLn h $ replicate 45 '#'
           hPutStrLn h $ show $ pretty a
           hPutStrLn h $ replicate 45 '#'
        
    
