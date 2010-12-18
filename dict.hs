import Data.List.Split (splitOn)
import System.Environment (getArgs)
import Control.Monad (join)
import Data.Char (toLower)
import Data.List (sort)
import System.Directory (getCurrentDirectory)
import System.Directory (doesFileExist)
import System.Environment.Executable (splitExecutablePath)

path :: IO String
path = splitExecutablePath >>= return . fst

-- Add filename to path
add_to_path :: String -> IO String
add_to_path filename = path >>= (\pfx -> return $ pfx ++ filename)

dictfile :: IO String
dictfile  = add_to_path "data/en_et.wbt"

indexfile :: IO String
indexfile = add_to_path "index_main"


main :: IO ()
main = do search         <- getArgs
          ifile          <- indexfile

          idata          <- (\exists -> if exists
                                           then readFile ifile
                                           else do putStrLn "Index not found. Indexing..."
                                                   index
                                                   readFile ifile
                            ) =<< (doesFileExist ifile)

          
          let idx        =  (read idata) :: [(Char,Int)]

          file_path      <- dictfile
          index_path     <- add_to_path "data/"
          wl             <- case lookup (head (head search)) idx of 
                              Just s -> readFile (index_path ++ "index" ++ (show s))
                              _      -> readFile file_path
          
          let dict       = [tupleize (splitOn "\t" x) | x <- lines wl]
          
          case search of
            [a]   -> output $ join $ lookup (Just a) dict
            _     -> putStrLn "Nothing found"
          
            where tupleize :: [a] -> (Maybe a, Maybe a)
                  tupleize [a,b]   = (Just a, Just b)
                  tupleize _       = (Nothing, Nothing)

                  output (Just a)  = putStrLn a 
                  output _         = putStrLn "Nothing"


-- Index dictionary for better berformance
-- Spit it into smaller files
-- TODO: split mapM oneliner into more smaller functions
index :: IO [()]
index = do file_path  <- dictfile
           file       <- readFile file_path
           -- file lines
           let flines = sort $ lines file
           let cnt    = counter flines [] [] 1
           ifile      <- indexfile
           index_path <- add_to_path "data/"

           writeFile ifile $ show $ reverse cnt

           mapM (\idx -> 
                  writeFile (location index_path idx) (unlines $ contents idx flines)
                ) cnt

             where contents idx flines = takeWhile (is_ch idx) $ dropWhile (contains idx) flines
                   contains idx line   = case line of
                                           (h:_) ->  h /= (fst idx)
                                           _     -> True

                   is_ch idx line      = not $ contains idx line
                   location path idx   = path ++ "index" ++ (show $ snd idx)

counter :: [[Char]] -> [(Char, Int)] -> [Char] -> Int -> [(Char,Int)]
counter [] acc _ _ = acc
counter (l:ls) acc last count = case l of
                                  (lh:_) -> if any (toLower lh==) last
                                               then counter ls acc last (count+1) 
                                               else counter ls ((toLower lh, count):acc) (toLower lh:last) (count+1)
                                  _      -> counter ls acc last (count+1)
