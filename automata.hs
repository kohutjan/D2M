import System.IO
import System.Environment
import qualified Data.Map as Map

data Automata = Automata { states :: [Int],
                           initialState :: Int,
                           acceptStates :: [Int],
                           transitions :: [Map.Map Char Int]
                         } deriving (Show)

createAutomata :: String -> String -> String -> Automata
createAutomata states' initialState' acceptStates' =
        Automata { states = read $ '[':states' ++ [']'] :: [Int],
                   initialState = read initialState' :: Int,
                   acceptStates = read $ '[':acceptStates' ++ [']'] :: [Int],
                   transitions = [Map.fromList([('a', 0)])]}

load :: String -> IO ()
load path = do
            handle <- openFile path ReadMode
            states' <- hGetLine handle
            initialState' <- hGetLine handle
            acceptStates' <- hGetLine handle
            hClose handle
            print $ createAutomata states' initialState' acceptStates'


parseArgs :: [String] -> (String -> IO ())
parseArgs args
          | elem "-i" args = load
          | otherwise = error "No arguments"

main = do
  args <- getArgs
  (parseArgs args) (last args)
  contents <- readFile (last args)
  putStr contents
