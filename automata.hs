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


getStateId :: Int -> Automata -> [Int] -> [Int]
getStateId state automata eqClasses = [eqClasses !! x | x -> (Map.elems transition)]
  where transitions' = transitions automata
        (Just transition) = Map.lookup state transitions



initEqClasses :: [Int] -> [Int] -> [Int]
getEqClasses states acceptStates = [if elem x acceptStates then 1 else 0 | x <- states]


getNextEqClasses :: Automata -> [Int] -> [Int]
getNextEqClasses automata eqClasses =
  where transitions' = transitions automata



getFinalEqClasses :: Automata -> [Int] -> [Int]
getFinalEqClasses automata eqClasses
  | nextEqClasses == eqClasses = eqClasses
  | otherwise = getFinalEqClasses automata nextEqClasses
  where nextEqClasses = getNextEqClasses automata eqClasses

createMinimizeAutomata :: Automata -> [Int] -> Automata
createMinimizeAutomata automata undisRelation =


minimizeAutomata :: Automata -> Automata
minimizeAutomata automata = createMinimizeAutomata automata (getFinalEqClasses automata eqClasses)
  where states' = states automata
        acceptStates' = acceptStates automata
        eqClasses = initEqClasses states' acceptStates'

completeState ::  Set.Set Char -> Int -> Map.Map Char Int -> Map.Map Char Int
completeState alphabet sinkState state = Map.union state (Map.fromList([(x, sinkState) | x <- Set.toList(alphabet), not $ elem x (Map.keys state)]))


completeAutomata :: Automata -> Automata
completeAutomata automata = Automata { states = (states automata) ++ [sinkState],
                                       initialState = initialState automata,
                                       acceptStates = acceptStates automata,
                                       transitions = Map.map (completeState (alphabet automata) sinkState) (transitions automata),
                                       alphabet = alphabet automata}
  where sinkState = maximum (states automata) + 1


getStates :: String -> [Int]
getStates states' = read $ '[':states' ++ [']'] :: [Int]


getTransitions :: [String] -> Map.Map Int (Map.Map Char Int)
getTransitions [] = Map.empty
getTransitions (x:xs) = Map.insertWith Map.union startState transition (getTransitions xs)
  where startState = Char.digitToInt $ head x :: Int
        transitionChar = x !! 2
        finalState = Char.digitToInt $ last x :: Int
        transition = Map.singleton transitionChar finalState


getAlphabet :: [String] -> Set.Set Char
getAlphabet [] = Set.empty
getAlphabet (x:xs) = Set.insert transitionChar (getAlphabet xs)
  where transitionChar = x !! 2


createAutomata :: [String] -> Automata
createAutomata (states':initialState':acceptStates':transitions') =
        Automata { states = getStates states',
                   initialState = read initialState' :: Int,
                   acceptStates = getStates acceptStates',
                   transitions = getTransitions transitions',
                   alphabet = getAlphabet transitions'}


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
