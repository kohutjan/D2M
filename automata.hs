import System.IO
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe


data Automata = Automata { states :: [Int],
                           initialState :: Int,
                           acceptStates :: [Int],
                           transitions :: Map.Map Int (Map.Map Char Int),
                           alphabet :: Set.Set Char
                          } deriving (Show)

main = do
       args <- getArgs
       let filePath = last args
       let proccesFunc = getProcessFunc args
       loadAutomata filePath proccesFunc


getProcessFunc :: [String] -> ([String] -> Automata)
getProcessFunc args
              | elem "-i" args = createAutomata
              | elem "-t" args = minimizeAutomata . completeAutomata . createAutomata
              | otherwise = error "No arguments"


loadAutomata :: String -> ([String] -> Automata) -> IO ()
loadAutomata path createAutomata = do
                                   content <- readFile path
                                   let fileLines = lines content
                                   let automata = createAutomata fileLines
                                   putStr $ showAutomata(automata)
                                   --putStr $ show(automata)


createAutomata :: [String] -> Automata
createAutomata (states':initialState':acceptStates':transitions') =
       Automata { states = readStates states',
                  initialState = read initialState' :: Int,
                  acceptStates = readStates acceptStates',
                  transitions = readTransitions transitions',
                  alphabet = readAlphabet transitions' }


readStates :: String -> [Int]
readStates states' = read $ '[':states' ++ [']'] :: [Int]


readTransitions :: [String] -> Map.Map Int (Map.Map Char Int)
readTransitions [] = Map.empty
readTransitions (line:fileLines) = Map.insertWith Map.union startState charTransition (readTransitions fileLines)
  where (Just firstCommaIndex) = List.elemIndex ',' line
        startState =  read $ take firstCommaIndex line :: Int
        transitionChar = line !! (firstCommaIndex + 1)
        finalState = read $ drop (firstCommaIndex + 3) line :: Int
        charTransition = Map.singleton transitionChar finalState


readAlphabet :: [String] -> Set.Set Char
readAlphabet [] = Set.empty
readAlphabet (line:fileLines) = Set.insert transitionChar (readAlphabet fileLines)
  where (Just firstCommaIndex) = List.elemIndex ',' line
        transitionChar = line !! (firstCommaIndex + 1)


completeAutomata :: Automata -> Automata
completeAutomata automata = if isComplete automata
                            then automata
                            else Automata { states = (states automata) ++ [sinkState],
                                            initialState = initialState automata,
                                            acceptStates = acceptStates automata,
                                            transitions = transitionsWithSink,
                                            alphabet = alphabet automata}
  where sinkState = maximum (states automata) + 1
        sinkStateTransition = Map.fromList([(transitionChar, sinkState) | transitionChar <- Set.toList(alphabet automata)])
        states' = states automata
        transitions' = transitions automata
        alphabet' = alphabet automata
        missingStartStates = [state | state <- states',
                                      not $ elem state (Map.keys transitions'),
                                      elem state $ concat $ (map Map.elems (Map.elems transitions'))]
        transitionsWithMissingStartStates = addStartStatesToTransitions missingStartStates transitions'
        completedStateTransitions = Map.map (completeStateTransition alphabet' sinkState) transitionsWithMissingStartStates
        transitionsWithSink = Map.insert sinkState sinkStateTransition completedStateTransitions


isComplete :: Automata -> Bool
isComplete automata = definedStartStates && definedStateTransitions
  where alphabetLength = length (alphabet automata)
        transitions' = transitions automata
        states' = states automata
        definedStartStates = (Map.size transitions') == (length states')
        stateTransitionsSizes = map Map.size (Map.elems transitions')
        definedStateTransitions = not $ elem False [stateTransitionSize == alphabetLength | stateTransitionSize <- stateTransitionsSizes]


addStartStatesToTransitions :: [Int] -> Map.Map Int (Map.Map Char Int) -> Map.Map Int (Map.Map Char Int)
addStartStatesToTransitions [] transitions = transitions
addStartStatesToTransitions (state:states) transitions = addStartStatesToTransitions states (Map.insert state Map.empty transitions)


completeStateTransition ::  Set.Set Char -> Int -> Map.Map Char Int -> Map.Map Char Int
completeStateTransition alphabet sinkState state = Map.union state missingCharTransitions
  where missingCharTransitions = Map.fromList([(transitionChar, sinkState) | transitionChar <- Set.toList(alphabet),
                                                                             not $ elem transitionChar (Map.keys state)])

minimizeAutomata :: Automata -> Automata
minimizeAutomata automata = createMinimizeAutomata automata finalEqClasses
  where states' = states automata
        acceptStates' = acceptStates automata
        initEqClasses = getInitEqClasses states' acceptStates'
        finalEqClasses = getFinalEqClasses automata initEqClasses


getInitEqClasses :: [Int] -> [Int] -> Map.Map Int Int
getInitEqClasses states acceptStates = Map.fromList([if elem x acceptStates
                                                     then (x, acceptStatesMin)
                                                     else (x, nonacceptStatesMin) | x <- states])
  where acceptStatesMin = minimum acceptStates
        nonacceptStatesMin = minimum [x | x <- states, not $ elem x acceptStates]


getFinalEqClasses :: Automata -> Map.Map Int Int -> Map.Map Int Int
getFinalEqClasses automata eqClasses
  | List.sort (Map.elems nextEqClasses) == List.sort (Map.elems eqClasses) = eqClasses
  | otherwise = getFinalEqClasses automata nextEqClasses
  where nextEqClasses = getNextEqClasses automata eqClasses


getNextEqClasses :: Automata -> Map.Map Int Int -> Map.Map Int Int
getNextEqClasses automata eqClasses = Map.fromList([(state, getEqClass statesTable state) | state <- states'])
  where states' = states automata
        transitions' = transitions automata
        statesTable = getStatesTable states' transitions' eqClasses


getStatesTable :: [Int] -> Map.Map Int (Map.Map Char Int) -> Map.Map Int Int -> Map.Map [Int] [Int]
getStatesTable [] transitions eqClasses = Map.empty
getStatesTable (state:states) transitions eqClasses = Map.insertWith (++) eqClassIdForState [state] (getStatesTable states transitions eqClasses)
  where eqClassIdForState = getEqClassIdForState state transitions eqClasses


getEqClassIdForState :: Int -> Map.Map Int (Map.Map Char Int) -> Map.Map Int Int -> [Int]
getEqClassIdForState state transitions eqClasses = actualEqClass : finalEqClassesForState
  where (Just stateTransition) = Map.lookup state transitions
        (Just actualEqClass) = Map.lookup state eqClasses
        finalEqClassesForState = Maybe.catMaybes [Map.lookup finalState eqClasses | finalState <- (Map.elems stateTransition)]


getEqClass :: Map.Map [Int] [Int] -> Int -> Int
getEqClass statesMap state = eqClass
  where eqStatesGroups = Map.elems statesMap
        eqClass = minimum (head [eqStates | eqStates <- eqStatesGroups, elem state eqStates])


createMinimizeAutomata :: Automata -> Map.Map Int Int -> Automata
createMinimizeAutomata automata eqClasses = Automata { states = finalStates,
                                                       initialState = finalInitialState,
                                                       acceptStates = finalAcceptStates,
                                                       transitions = finalTransitios,
                                                       alphabet = alphabet automata}
  where finalStates = getFinalStates eqClasses
        finalInitialState = getFinalInitialState (initialState automata) eqClasses
        finalAcceptStates = getFinalAcceptStates (acceptStates automata) eqClasses
        finalTransitios = getFinalTransitions (transitions automata) (states automata) eqClasses


getFinalStates :: Map.Map Int Int -> [Int]
getFinalStates eqClasses = unique $ Map.elems eqClasses


getFinalInitialState :: Int -> Map.Map Int Int -> Int
getFinalInitialState initialState eqClasses = finalInitialState
  where (Just finalInitialState) = Map.lookup initialState eqClasses


getFinalAcceptStates :: [Int] -> Map.Map Int Int -> [Int]
getFinalAcceptStates acceptStates eqClasses = unique $ Maybe.catMaybes [Map.lookup acceptState eqClasses | acceptState <- acceptStates]


getFinalTransitions :: Map.Map Int (Map.Map Char Int) -> [Int] -> Map.Map Int Int -> Map.Map Int (Map.Map Char Int)
getFinalTransitions transitions [] eqClasses = Map.empty
getFinalTransitions transitions (state:states) eqClasses = Map.insert eqState finalStateTransition (getFinalTransitions transitions states eqClasses)
  where (Just stateTransition) = Map.lookup state transitions
        finalStateTransition = Map.fromList $ [(fst charTransition, Maybe.fromJust $ Map.lookup (snd charTransition) eqClasses) | charTransition <- Map.toList stateTransition]
        (Just eqState) = Map.lookup state eqClasses


unique :: [Int] -> [Int]
unique l = Set.toList $ Set.fromList l


showAutomata :: Automata -> String
showAutomata automata = showStates ++ showInitialState ++ showAcceptStates ++ showTransitions
 where showStates = (tail (init (show (states automata)))) ++ "\n"
       showInitialState = (show (initialState automata)) ++ "\n"
       showAcceptStates = (tail (init (show (acceptStates automata)))) ++ "\n"
       showTransitions = getShowTransitions $ Map.toList (transitions automata)


getShowTransitions :: [(Int, Map.Map Char Int)] -> String
getShowTransitions [] = ""
getShowTransitions (transition:transitions) = getShowTransition (fst transition) (Map.toList(snd transition)) ++ getShowTransitions transitions


getShowTransition :: Int -> [(Char, Int)] -> String
getShowTransition state [] = ""
getShowTransition state (charTransition:charTransitions) = show state ++ "," ++ [fst charTransition] ++ "," ++ (show (snd charTransition)) ++ "\n" ++ (getShowTransition state charTransitions)
