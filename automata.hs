import System.IO
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List


data Automata = Automata { states :: [Int],
                           initialState :: Int,
                           acceptStates :: [Int],
                           transitions :: Map.Map Int (Map.Map Char Int),
                           alphabet :: Set.Set Char
                          } deriving (Show)

unique :: [Int] -> [Int]
unique l = Set.toList $ Set.fromList l


getStateId :: Int -> Map.Map Int (Map.Map Char Int) -> [Int] -> [Int]
getStateId state transitions eqClasses = [eqClasses !! x | x <- (Map.elems transition)]
  where (Just transition) = Map.lookup state transitions


getStatesMap :: [Int] -> Map.Map Int (Map.Map Char Int) -> [Int] -> Map.Map [Int] [Int]
getStatesMap [] transitions eqClasses = Map.empty
getStatesMap (x:xs) transitions eqClasses = Map.insertWith (++) stateId [x] (getStatesMap xs transitions eqClasses)
  where stateId = getStateId x transitions eqClasses


initEqClasses :: [Int] -> [Int] -> [Int]
initEqClasses states acceptStates = [if elem x acceptStates then 1 else 0 | x <- states]


getEqClass :: Map.Map [Int] [Int] -> Int -> Int
getEqClass statesMap state = eqClass
  where eqStates = Map.elems statesMap
        (Just eqClass) = List.elemIndex (head [x | x <- eqStates, elem state x]) eqStates


getNextEqClasses :: Automata -> [Int] -> [Int]
getNextEqClasses automata eqClasses = map (getEqClass statesMap) states'
  where states' = states automata
        transitions' = transitions automata
        statesMap = getStatesMap states' transitions' eqClasses


getFinalEqClasses :: Automata -> [Int] -> [Int]
getFinalEqClasses automata eqClasses
  | nextEqClasses == eqClasses = eqClasses
  | otherwise = getFinalEqClasses automata nextEqClasses
  where nextEqClasses = getNextEqClasses automata eqClasses


getFinalStates :: [Int] -> [Int]
getFinalStates eqClasses = unique eqClasses


getFinalInitialState :: Int -> [Int] -> Int
getFinalInitialState initialState eqClasses = eqClasses !! initialState


getFinalAcceptStates :: [Int] -> [Int] -> [Int]
getFinalAcceptStates acceptStates eqClasses = unique [eqClasses !! x | x <- acceptStates]


getFinalTransitions :: Map.Map Int (Map.Map Char Int) -> [Int] -> [Int] -> Map.Map Int (Map.Map Char Int)
getFinalTransitions transitions [] eqClasses = Map.empty
getFinalTransitions transitions (x:xs) eqClasses = Map.insert x transition' (getFinalTransitions transitions xs eqClasses)
  where (Just transition) = Map.lookup x transitions
        transition' = Map.fromList [(fst x, eqClasses !! (snd x)) | x <- Map.toList transition]


createMinimizeAutomata :: Automata -> [Int] -> Automata
createMinimizeAutomata automata eqClasses = Automata { states = finalStates,
                                                       initialState = finalInitialState,
                                                       acceptStates = finalAcceptStates,
                                                       transitions = finalTransitios,
                                                       alphabet = alphabet automata}
  where finalStates = getFinalStates eqClasses
        finalInitialState = getFinalInitialState (initialState automata) eqClasses
        finalAcceptStates = getFinalAcceptStates (acceptStates automata) eqClasses
        finalTransitios = getFinalTransitions (transitions automata) finalStates eqClasses


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
            print $ minimizeAutomata (completeAutomata (createAutomata (lines content)))


parseArgs :: [String] -> (String -> IO ())
parseArgs args
          | elem "-i" args = load
          | otherwise = error "No arguments"


main = do
  args <- getArgs
  (parseArgs args) (last args)
  contents <- readFile (last args)
  putStr contents
