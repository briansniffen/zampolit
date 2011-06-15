{-# Language DeriveDataTypeable #-}

module Main where

import HSH hiding (space)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import System.Console.CmdArgs
import System.FilePath (takeFileName)
import System.Directory (getCurrentDirectory)

import System.Locale
import System.IO
import Data.Time
import Data.Time.Format

import Data.Function
import Data.List
import Data.Map (Map,(!))
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Utils

type Commit = String
type Author = String
type Date = UTCTime

data Cmd = Cmd {outfile :: FilePath,
                gameName :: String}
           deriving (Show,Data,Typeable)
                      

data CA = CA { commit :: Commit
             , date :: Date
             , author :: Author }

type Total = (Date,Author,Map Author Int)

foldNames "bts" = "brians"
foldNames "Paul Weaver" = "pweaver"
foldNames a = a

parseCA :: Parser [CA]
parseCA = many1 $ do
  string "commit "
  c <- count 40 hexDigit
  newline
  string "Author: "
  a' <- many1 (noneOf "<")
  let a = foldNames $ strip a'
  between (char '<') (char '>') $ many (noneOf ">")
  newline
  string "Date:   "
  d' <- many1 (noneOf "\n")
  let d = rTime d'
  newline
  return CA {commit = c, author = a, date = d}

rTime = readTime defaultTimeLocale "%a %b %e %T %Y %Z"
fTime = formatTime defaultTimeLocale "%s" 

wc ca = do
  runIO $ "git checkout " ++ commit ca
  s <- run $ "find . -name \\*tex -print0 -o -name \\*txt -print0 -o -name \\*yaml -print0" 
      -|- "xargs -0 wc -w"
      -|- "tail -1" -|- "grep -o '[0-9]\\+'"
  return $ read s

runningTotals :: [Total] -> (Author,Date,Int) -> [Total]
runningTotals totals (author,date,wc) = new:totals
  where
    new = (date,author,case totals of
                            [] -> f Map.empty
                            other -> f old)
    f = Map.insertWith (+) author wc
    old = (\(a,b,c)->c) $ head totals


printHeader :: FilePath -> String -> [Author] -> IO ()
printHeader output title authors = 
  withFile (output ++ ".gnuplot") WriteMode (\h -> do
    hPutStrLn h "set xdata time"
    hPutStrLn h "set timefmt \"%s\""
    hPutStrLn h "set format x \"%m/%d\""
    hPutStrLn h "set terminal postscript enhanced color size 10in,7.5in"
    hPutStrLn h $ "set output '| ps2pdf - " ++ output ++ ".pdf'"
    hPutStrLn h "set xlabel \"date\""
    hPutStrLn h "set ylabel \"words\""
    hPutStrLn h $ "set title \"" ++ title ++ " word counts by author\""
    hPutStrLn h "set datafile missing \"?\""
    hPutStr   h "plot "
    hPutStrLn h . intercalate ", " . map plotLine $ zip [2..] authors)
      where
        plotLine (n,a) = "'" ++ output ++ ".data' using 1:($" 
                         ++ show n ++ ") title \"" ++ a ++ "\""    

printRow :: Handle -> [Author] -> Total -> IO ()
printRow h authors (date,name,total) = do
  hPutStr   h $ fTime date
  hPutStr   h "\t"
  hPutStr   h $ take before $ cycle "?\t"
  hPutStr   h . show $ Map.findWithDefault 0 name total
  hPutStrLn h $ take after $ cycle "\t?"
    where
      i = fromJust $ elemIndex name authors
      before = 2*i
      after = 2*((length authors) - i)

main :: IO ()
main = do
  dir <- return . takeFileName =<< getCurrentDirectory
  let cmd = Cmd { outfile = def &= argPos 1 &= opt (dir ++ "-wc") &= typFile 
                , gameName = def &= argPos 0 &= opt dir &= typ "NAME"}
            &= program "zampolit"
            &= summary "zampolit v0.3, (c) Brian Snffen 2011"
  c <- cmdArgs cmd
  let title = gameName c
      output = outfile c
  cas <- run $ "git log" -|- "grep '^\\(commit\\|Auth\\|Date\\)'"
  case parse parseCA "" cas of 
    Left err -> print err
    Right cas -> do
      wcs <- mapM wc cas
      runIO $ "git checkout master"
      let totals = foldl runningTotals [] . reverse 
                 . zip3 (map author cas) (map date cas) 
                 . map (max 0) 
                 $ zipWith (-) wcs (tail wcs ++ [head $ reverse wcs])
          (_,_,mp) = head totals
          authors = sortBy (\x y -> compare (mp ! y) (mp ! x)) $ Map.keys mp
      printHeader output title authors
      withFile (output ++ ".data") WriteMode (\h ->
        mapM_ (printRow h authors) $ reverse totals)
      runIO $ "gnuplot " ++ output ++ ".gnuplot"
