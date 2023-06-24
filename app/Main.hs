module Main where

import BookCypher
import System.Environment
import System.IO
import Data.Maybe (isJust,fromMaybe)
import System.Exit

main :: IO ()
main = getArgs >>= parse

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse ["-e",keyFile,inFile,outFile] = do [keyText,inFileText] <- getKeyAndInFile keyFile inFile
                                         outFileTextMaybe <- encryptText [keyText,inFileText]
                                         checkOutFileText outFileTextMaybe outFile
parse ["-d",keyFile,inFile,outFile] = do [keyText,inFileText] <- getKeyAndInFile keyFile inFile
                                         outFileTextMaybe <- decryptText [keyText,inFileText]
                                         checkOutFileText outFileTextMaybe outFile
parse _ = putStrLn "ERROR: Invalid arguments. Check the usage below." >> usage >> exit

getKeyAndInFile :: FilePath -> FilePath -> IO [String]
getKeyAndInFile keyFile inFile = sequence [readFile keyFile, readFile inFile]

encryptText :: [String] -> IO (Maybe String)
encryptText [keyText,inFileText] = getWordsEncrypt keyText inFileText

decryptText :: [String] -> IO (Maybe String)
decryptText [keyText,inFileText] = getWordsDecrypt keyText inFileText

checkOutFileText :: Maybe String -> FilePath -> IO ()
checkOutFileText outFileTextMaybe outFile =
  case isJust outFileTextMaybe of
    True -> do let outFileText = fromMaybe "" outFileTextMaybe
               writeFile outFile outFileText
               putStrLn "SUCCESS: Output file written."
    False -> putStrLn "ERROR: Check that all letters for the input text are present in the key text, and that no obscure characters are used in the input text."

usage   = putStrLn "Usage: bypher [-e|-d] [key file] [input file] [output file]"
version = putStrLn "Bypher 0.1"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)
  

  
