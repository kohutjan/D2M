import System.IO
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

data Automata = Automata { states :: [Int],
                           initialState :: Int,
                           acceptStates :: [Int],
                           transitions :: Map.Map Int (Map.Map Char Int),
                           alphabet :: Set.Set Char
                         } deriving (Show)

completeState ::  Set.Set Char -> Int -> Map.Map Char Int -> Map.Map Char Int
completeState alphabet sinkState state = Map.union state (Map.fromList([(x, sinkState) | x <- Set.toList(alphabet), not $ elem x (Map.keys state)]))

completeAutomata :: Automata -> Automata
completeAutomata automata = Automata { states = (states automata) ++ [sinkState],
                                       initialState = initialState automata,
                                       acceptStates = acceptStates automata,
                                       transitions = Map.map (completeState (alphabet automata) sinkState) (transitions automata),
                                       alphabet = alphabet automata}
  where sinkState = maximum (states automata) + 1

getAlphabet :: [String] -> Set.Set Char -> Set.Set Char
getAlphabet [] alphabet = alphabet
getAlphabet (x:xs) alphabet = getAlphabet xs (Set.insert transitionChar alphabet)
  where transitionChar = x !! 2

getTransitions :: [String] -> Map.Map Int (Map.Map Char Int) -> Map.Map Int (Map.Map Char Int)
getTransitions [] transitions = transitions
getTransitions (x:xs) transitions = getTransitions xs (Map.insertWith Map.union startState transition transitions)
  where startState = Char.digitToInt $ head x :: Int
        transitionChar = x !! 2
        finalState = Char.digitToInt $ last x :: Int
        transition = Map.singleton transitionChar finalState

createAutomata :: [String] -> Automata
createAutomata (states':initialState':acceptStates':transitions') =
        Automata { states = read $ '[':states' ++ [']'] :: [Int],
                   initialState = read initialState' :: Int,
                   acceptStates = read $ '[':acceptStates' ++ [']'] :: [Int],
                   transitions = getTransitions transitions' Map.empty,
                   alphabet = getAlphabet transitions' Set.empty}

load :: String -> IO ()
load path = do
            content <- readFile path
            print $ completeAutomata (createAutomata (lines content))


parseArgs :: [String] -> (String -> IO ())
parseArgs args
          | elem "-i" args = load
          | otherwise = error "No arguments"

main = do
  args <- getArgs
  (parseArgs args) (last args)
  contents <- readFile (last args)
  putStr contents
