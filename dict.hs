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
main = do search  <- getArgs
          indexfile_path <- indexfile
          indexed <- doesFileExist indexfile_path

          idata <- case indexed of
                     True -> readFile indexfile_path
                     _    -> do putStrLn "Index not found. Indexing..."
                                index
                                readFile indexfile_path
          
          -- get index
          let idx =  (read idata) :: [(Char,Int)]

          -- open appropriate index file
          file_path <- dictfile
          index_path <- add_to_path "data/"
          wl <- case lookup (head (head search)) idx of 
                       Just s -> readFile (index_path ++ "index" ++ (show s))
                       _      -> readFile file_path
          
          let li = lines wl
          let dict = [tupleize (splitOn "\t" x) | x <- li]
          
          case search of
            [a]   -> output $ join $ lookup (Just a) dict

            _     -> putStrLn "Nothing"
          
          where tupleize :: [a] -> (Maybe a, Maybe a)
                tupleize [a,b]   = (Just a, Just b)
                tupleize _       = (Nothing, Nothing)

                output (Just a)  = putStrLn a 
                output _         = putStrLn "Nothing"


-- Index dictionary for better berformance
-- Spit it into smaller files
-- TODO: split mapM oneliner into more smaller functions
index :: IO ()
index = do file_path <- dictfile
           file   <- readFile file_path
           -- file lines
           let flines = sort $ lines file
           let cnt = counter flines [] [] 1
           indexfile_path <- indexfile
           writeFile indexfile_path $ show $ reverse cnt
           index_path <- add_to_path "data/"
           mapM (\idx -> writeFile (index_path ++ "index" ++ (show $ snd idx)) (unlines (contents idx flines))) cnt
           return ()
             where contents idx flines = takeWhile (is_ch idx) $ dropWhile (contains idx) flines
                   contains idx line = case line of
                                         (h:_) ->  h /= (fst idx)
                                         _     -> True

                   is_ch idx line = case line of
                                      (h:_) -> h == (fst idx)
                                      _     -> False
counter [] acc _ _ = acc
counter (l:ls) acc last count = 
  case l of
    (lh:_) -> if any ((toLower lh)==) last
                then counter ls acc last (count+1) 
                else counter ls (((toLower lh),count):acc) ((toLower lh):last) (count+1)
    _      -> counter ls acc last (count+1)
