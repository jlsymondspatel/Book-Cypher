module BookCypher where

import Data.Char (toLower)
import Data.Tuple (swap)
import Data.Maybe (isJust,fromJust)
import Data.List (intercalate,isInfixOf,isPrefixOf)
import System.Random
import Data.List.Split as Split (splitOn,split,keepDelimsL,keepDelimsR,oneOf)
import Text.Read (readMaybe)

type InText = String
type InWord = String
type InWords = [String]
type InChar = Char
type OutWord = String
type OutWords = [String]
type OutWordWN = String
type OutWordLN = String
type OutChar = String
type OutCharCN = String
type OutCharLN = String
type OutText = String
type KeyWord = String
type KeyWords = [String]
type KeyLine = String
type KeyLines = [String]
type KeyText = String
type KeyChar = Char

getKeyTextAsLines :: KeyText -> KeyLines
getKeyTextAsLines keyText = lines keyText

getInTextAsWords :: InText -> InWords
getInTextAsWords inText = inTextWordsPunctBreaks
  where inTextLower = map toLower inText
        inTextWords = words inTextLower :: InWords
        inTextWordsPunctBreaks = filter (not . null) . concat . map (Split.split (keepDelimsR $ oneOf ",.'!?0123456789")) $ concat . map (Split.split (keepDelimsL $ oneOf ",.'!?;:0123456789")) $ inTextWords

-- if input list is empty then output is also an empty list
getKeyTextIndexedWords :: KeyLines -> [(Int,[(KeyWord,Int)])]
getKeyTextIndexedWords keyLines = zip [1..] (map (\kwds -> zip kwds [1..]) keyWordssFiltered)
  where keyLinesLower = (map . map) toLower keyLines
        keyWordss = map words keyLinesLower :: [[[KeyChar]]]
        keyWordssFiltered = (map . map) (filter (\x -> elem x (['a'..'z']++['-','/']))) $ keyWordss

getKeyTextIndexedChars :: KeyLines -> [(Int,[(KeyChar,Int)])]
getKeyTextIndexedChars keyLines = zip [1..] (map (\chars -> zip chars [1..]) keyLinesLowerNoSpaces)
  where keyLinesLower = (map . map) toLower keyLines
        keyLinesLowerNoSpaces = map concat . map words $ keyLinesLower


findWordIndFromLine :: InWord -> [(KeyWord,Int)] -> Maybe OutWordWN
findWordIndFromLine inWord kwdAndIndTuples = fmap show $ lookup inWord kwdAndIndTuples

findCharIndFromLine :: InChar -> [(KeyChar,Int)] -> Maybe OutCharCN
findCharIndFromLine inChar kchAndIndTuples = fmap show $ lookup inChar kchAndIndTuples


getWordEncryptAsWord :: [(Int,[(KeyWord,Int)])] -> InWord -> IO (Maybe OutWord)
getWordEncryptAsWord kwlnsAndIndsTuples inWord
  | null indAndOutWordWNs = return Nothing
  | otherwise = do
      seedGen <- initStdGen
      let (randInd,_) = uniformR (0,n-1) seedGen
      let (outWordLNInt,Just outWordWN) = indAndOutWordWNs !! randInd
      return (Just $ (show outWordLNInt) ++ ".0" ++ outWordWN)
        where indAndOutWordWNs = filter (\(_,x) -> isJust x) . (map . fmap) (findWordIndFromLine inWord) $ kwlnsAndIndsTuples :: [(Int,Maybe OutWordWN)]
              n = length indAndOutWordWNs :: Int

getCharEncryptAsChars :: [(Int,[(KeyChar,Int)])] -> InChar -> IO (Maybe OutChar)
getCharEncryptAsChars kclnsAndIndsTuples inChar
  | null indAndOutCharCNs = return Nothing
  | otherwise = do
      seedGen <- initStdGen
      let (randInd,_) = uniformR (0,n-1) seedGen
      let (outCharLNInt,Just outCharCN) = indAndOutCharCNs !! randInd
      return $ Just $ (show outCharLNInt) ++ ".1" ++ outCharCN
        where indAndOutCharCNs = filter (\(_,x) -> isJust x) . (map . fmap) (findCharIndFromLine inChar) $ kclnsAndIndsTuples :: [(Int,Maybe OutCharCN)]
              n = length indAndOutCharCNs :: Int

getWordEncryptAsChars :: [(Int,[(KeyChar,Int)])] -> InWord -> IO (Maybe OutWord)
getWordEncryptAsChars kclnsAndIndsTuples inWord =
  do outCharsMaybeList <- outChars -- [Maybe OutChar]
     case (and . (map isJust)) outCharsMaybeList of
       False -> return Nothing
       _ -> return $ (fmap (intercalate "+") . sequence) outCharsMaybeList
       where outChars = sequence . map (getCharEncryptAsChars kclnsAndIndsTuples) $ inWord

getNumAndPunctEncrypt :: InWord -> Maybe OutWord
getNumAndPunctEncrypt inWord =
  case inWord of
    "," -> Just "0.01"
    "." -> Just "0.02"
    "'" -> Just "0.03"
    "!" -> Just "0.04"
    "?" -> Just "0.05"
    "0" -> Just "0.06"
    "1" -> Just "0.07"
    "2" -> Just "0.08"
    "3" -> Just "0.09"
    "4" -> Just "0.011"
    "5" -> Just "0.012"
    "6" -> Just "0.013"
    "7" -> Just "0.014"
    "8" -> Just "0.015"
    "9" -> Just "0.016"
    ";" -> Just "0.017"
    ":" -> Just "0.018"
    _ -> Nothing
  
getWordEncrypt :: [(Int,[(KeyWord,Int)])] -> [(Int,[(KeyChar,Int)])] -> InWord -> IO (Maybe OutWord)
getWordEncrypt kwlnsAndIndsTuples kclnsAndIndsTuples inWord =
  case (elem inWord ([",",".","'","!","?",";",":"]++(map (\x-> [x]) ['0'..'9']))) of
    True -> return $ getNumAndPunctEncrypt inWord
    _ -> do wordAsWord <- getWordEncryptAsWord kwlnsAndIndsTuples inWord
            wordAsChars <- getWordEncryptAsChars kclnsAndIndsTuples inWord
            if isJust wordAsWord == True
              then return wordAsWord
              else if isJust wordAsChars == True
                   then return wordAsChars
                   else return Nothing

getWordsEncrypt :: KeyText -> InText -> IO (Maybe OutText)
getWordsEncrypt keyText inText =
  do wordsEncryptMaybeList <- wordsEncryptIOMaybeList
     case (and . (map isJust)) wordsEncryptMaybeList of
       False -> return Nothing
       _ -> return $ fmap (intercalate ",") . sequence $ wordsEncryptMaybeList
       where keyLines = getKeyTextAsLines keyText
             inWords = getInTextAsWords inText
             keyIndWords = getKeyTextIndexedWords keyLines
             keyIndChars = getKeyTextIndexedChars keyLines
             wordsEncryptIOMaybeList = sequence . map (getWordEncrypt keyIndWords keyIndChars) $ inWords :: IO [Maybe OutWord]

getWordDecryptAsWord :: [(Int,[(KeyWord,Int)])] -> OutWord -> Maybe InWord
getWordDecryptAsWord kwlnsAndIndsTuples outWord
  | ((and . map isJust $ [lnMaybeInt,wnMaybeInt]) == False || line == Nothing) = Nothing
  | otherwise = word
    where [ln,wn] = splitOn ".0" outWord
          lnMaybeInt = readMaybe ln :: Maybe Int
          wnMaybeInt = readMaybe wn :: Maybe Int
          lnInt = fromJust lnMaybeInt
          wnInt = fromJust wnMaybeInt
          line = lookup lnInt kwlnsAndIndsTuples
          word = lookup wnInt . map swap . fromJust $ line
  
getCharDecryptAsChars :: [(Int,[(KeyChar,Int)])] -> OutChar -> Maybe InChar
getCharDecryptAsChars kclnsAndIndsTuples outChar
  | ((and . map isJust $ [lnMaybeInt,cnMaybeInt]) == False || line == Nothing) = Nothing
  | otherwise = char
    where [ln,cn] = splitOn ".1" outChar
          lnMaybeInt = readMaybe ln :: Maybe Int
          cnMaybeInt = readMaybe cn :: Maybe Int
          lnInt = fromJust lnMaybeInt
          cnInt = fromJust cnMaybeInt
          line = lookup lnInt kclnsAndIndsTuples
          char = lookup cnInt . map swap . fromJust $ line

getWordDecryptAsChars :: [(Int,[(KeyChar,Int)])] -> OutWord -> Maybe InWord
getWordDecryptAsChars kclnsAndIndsTuples outWord
  | (and . map isJust $ inChars) == False = Nothing
  | otherwise = sequence inChars
    where outChars = splitOn "+" outWord
          inChars = map (getCharDecryptAsChars kclnsAndIndsTuples) outChars :: [Maybe InChar]

getWordDecryptAsNumAndPunct :: OutWord -> Maybe InWord
getWordDecryptAsNumAndPunct outWord =
  case outWord of
    "0.01" -> Just ","
    "0.02" -> Just "."
    "0.03" -> Just "'"
    "0.04" -> Just "!"
    "0.05" -> Just "?"
    "0.06" -> Just "0"
    "0.07" -> Just "1"
    "0.08" -> Just "2"
    "0.09" -> Just "3"
    "0.011" -> Just "4"
    "0.012" -> Just "5"
    "0.013" -> Just "6"
    "0.014" -> Just "7"
    "0.015" -> Just "8"
    "0.016" -> Just "9"
    "0.017" -> Just ";"
    "0.018" -> Just ":"
    _ -> Nothing
  
getWordDecrypt :: [(Int,[(KeyWord,Int)])] -> [(Int,[(KeyChar,Int)])] -> OutWord -> (Maybe InWord)
getWordDecrypt kwlnsAndIndsTuples kclnsAndIndsTuples outWord
  | isPrefixOf "0." outWord = getWordDecryptAsNumAndPunct outWord
  | isInfixOf ".0" outWord = getWordDecryptAsWord kwlnsAndIndsTuples outWord
  | isInfixOf ".1" outWord = getWordDecryptAsChars kclnsAndIndsTuples outWord
  | otherwise = Nothing

getWordsDecrypt :: KeyText -> OutText -> IO (Maybe InText)
getWordsDecrypt keyText outText
  | (and . map isJust $ inWords) == False = return Nothing
  | otherwise = return $ fmap unwords . sequence $ inWords
  where keyLines = getKeyTextAsLines keyText
        outWords = splitOn "," outText
        keyIndWords = getKeyTextIndexedWords keyLines
        keyIndChars = getKeyTextIndexedChars keyLines
        inWords = map (getWordDecrypt keyIndWords keyIndChars) outWords :: [Maybe InWord]
