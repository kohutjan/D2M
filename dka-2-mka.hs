{-|
  project: DKA-2-MKA
  login: xkohut08
  name: Jan Kohút
  date: 7. 4. 2018
-}

import System.IO
import System.Environment
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type State = Int
type TransitionChar = Char

{-|
  Transitions are represented as Map.Map State StateTransition.
  Every state has it's own StateTransition which represents the
  transitions from this state.
-}
type StateTransition = Map.Map TransitionChar State
type EqClass = Int

--EqClasses represents mapping of states to theirs current equivalence classes.
type EqClasses = Map.Map State EqClass

--Automata represetns finite automata
data Automata = Automata { states :: [State],
                           initialState :: State,
                           acceptStates :: [State],
                           transitions :: Map.Map State StateTransition,
                           alphabet :: Set.Set TransitionChar
                          } deriving (Show)


--Parse arguments
main = do
       args <- getArgs
       if (not ((elem "-i" args) || (elem "-t" args)))
         then error "Bad arguments"
         else return ()
       let options = ["-i", "-t"]
       --if last argument is not "-i" or "-t" store file path
       let filePath = if ((not $ null (last args)) && (not $ elem (last args) options))
                      then last args
                      else []
       let proccesFuncs = if null filePath
                            then getProcessFuncs args
                            else getProcessFuncs (init args)
       if null filePath
       then loadAutomatas getContents proccesFuncs
       else loadAutomatas (readFile filePath) proccesFuncs


getProcessFuncs :: [String] -> [([String] -> Automata)]
getProcessFuncs [] = []
getProcessFuncs (option:args) = (getProcessFunc option) : (getProcessFuncs args)


--Return process function for argument type
getProcessFunc :: String -> ([String] -> Automata)
getProcessFunc "-i" = createAutomata
getProcessFunc "-t" = minimizeAutomata . completeAutomata . createAutomata
getProcessFunc _ = error "Bad arguments"


loadAutomatas :: IO String -> [([String] -> Automata)] -> IO ()
loadAutomatas loadMode proccesFuncs = do
                                      content <- loadMode
                                      let fileLines = lines content
                                      let automatas = processAutomata proccesFuncs fileLines
                                      putStr (List.intercalate "\n" (map showAutomata automatas))


--Apply each process function on automata and return the result as list of strings.
processAutomata :: [([String] -> Automata)] -> [String] -> [Automata]
processAutomata [] _ = []
processAutomata (proccesFunc:proccesFuncs) fileLines = (proccesFunc fileLines) :
                                                       (processAutomata
                                                           proccesFuncs
                                                              fileLines)


--Read every part of automata string and create Automata
createAutomata :: [String] -> Automata
createAutomata (states':initialState':acceptStates':transitions') = if checkAutomata automata
                                                                      then automata
                                                                      else error "Bad semantic"
  where automata = Automata { states = readStates states',
                              initialState = readState initialState',
                              acceptStates = readStates acceptStates',
                              transitions = readTransitions transitions',
                              alphabet = readAlphabet transitions' }
createAutomata _ = error "Bad input"


--Read states and check validity
readStates :: String -> [State]
readStates states' = [if state < 0
                        then error "Negative state"
                        else state | state <- actualStates]
  where readsStates = reads $ ('[':states' ++ [']']) :: [([Int], String)]
        possibleStates = if null readsStates
                           then error "Bad states"
                           else readsStates
        actualStates = if (((null . fst . head)  possibleStates) ||
                           ((not . null . snd . head) possibleStates))
                         then error "Bad states"
                         else (fst . head) possibleStates


--Read state and check validity
readState :: String -> State
readState state = if actualState < 0
                    then error "Negative state"
                    else actualState
  where readsState = reads state :: [(Int, String)]
        possibleState = if null readsState
                          then error "Bad state"
                          else readsState
        actualState = if (null . snd . head) possibleState
                        then (fst . head) possibleState
                        else error "Bad state"


--Read transitions and check validity
readTransitions :: [String] -> Map.Map State StateTransition
readTransitions [] = Map.empty
readTransitions (line:fileLines) = Map.insertWith Map.union startState
                                   charTransition (readTransitions fileLines)
  where firstCommaIndex = maybe (error "Bad transitions") id
                                (List.elemIndex ',' line)
        secondCommaIndex = maybe (error "Bad transitions") id
                                 (List.elemIndex ',' (drop (firstCommaIndex + 1) line))
        startState =  readState $ take firstCommaIndex line
        transitionChar = readTransitionChar (take secondCommaIndex (drop (firstCommaIndex + 1) line))
        finalState = readState $ drop (firstCommaIndex + secondCommaIndex + 2) line
        charTransition = Map.singleton transitionChar finalState


--Read transition char (alphabet character) and check validity
readTransitionChar :: String -> TransitionChar
readTransitionChar transitionChar = if Char.isLower actualChar
                                      then actualChar
                                      else error "Bad alphabet"
  where actualChar = if length transitionChar == 1
                      then head transitionChar
                      else error "Bad alphabet"


--Read alphabet, doesn't have to check validity (already done in transitions).
readAlphabet :: [String] -> Set.Set TransitionChar
readAlphabet [] = Set.empty
readAlphabet (line:fileLines) = Set.insert transitionChar (readAlphabet fileLines)
  where (Just firstCommaIndex) = List.elemIndex ',' line
        transitionChar = line !! (firstCommaIndex + 1)


--Check if states in initial state, acceptance states and transitions corresponds with automata states.
checkAutomata :: Automata -> Bool
checkAutomata ( Automata states initialState acceptStates transitions _) =
        correctInitialState && correctAcceptStates && correctTransitions
  where correctInitialState = elem initialState states
        correctAcceptStates = foldl (&&) True [elem acceptState states |
                                               acceptState <- acceptStates]
        correctTransitions = checkTransitions transitions states


checkTransitions :: Map.Map State StateTransition -> [State] -> Bool
checkTransitions transitions states = correctStartStates && correctEndStates
  where correctStartStates = foldl (&&) True [elem startState states |
                                              startState <- Map.keys transitions]
        endStates = concat $ map Map.elems (Map.elems transitions)
        correctEndStates = foldl (&&) True [elem endState states |
                                            endState <- endStates]


--Complete automata if needed.
completeAutomata :: Automata -> Automata
completeAutomata automata = if isComplete automata
                            then automata
                            else Automata { states = (states automata) ++ [sinkState],
                                            initialState = initialState automata,
                                            acceptStates = acceptStates automata,
                                            transitions = transitionsWithSink,
                                            alphabet = alphabet automata}
  where sinkState = maximum (states automata) + 1
        sinkStateTransition = Map.fromList([(transitionChar, sinkState) |
                                            transitionChar <- Set.toList(alphabet automata)])
        states' = states automata
        transitions' = transitions automata
        alphabet' = alphabet automata
        missingStartStates = [state | state <- states',
                                      not $ elem state (Map.keys transitions'),
                                      elem state $ concat $ (map Map.elems (Map.elems transitions'))]
        transitionsWithMissingStartStates = addStartStatesToTransitions missingStartStates transitions'
        completedStateTransitions = Map.map (completeStateTransition alphabet' sinkState)
                                            transitionsWithMissingStartStates
        transitionsWithSink = Map.insert sinkState sinkStateTransition completedStateTransitions


isComplete :: Automata -> Bool
isComplete automata = definedStartStates && definedStateTransitions
  where alphabetLength = Set.size (alphabet automata)
        transitions' = transitions automata
        states' = states automata
        definedStartStates = (Map.size transitions') == (length states')
        stateTransitionsSizes = map Map.size (Map.elems transitions')
        definedStateTransitions = not $ elem False [stateTransitionSize == alphabetLength |
                                                    stateTransitionSize <- stateTransitionsSizes]


addStartStatesToTransitions :: [State] -> Map.Map State StateTransition -> Map.Map State StateTransition
addStartStatesToTransitions [] transitions = transitions
addStartStatesToTransitions (state:states) transitions =
    addStartStatesToTransitions states (Map.insert state Map.empty transitions)


completeStateTransition ::  Set.Set TransitionChar -> State -> StateTransition -> StateTransition
completeStateTransition alphabet sinkState state = Map.union state missingCharTransitions
  where missingCharTransitions = Map.fromList([(transitionChar, sinkState) |
                                              transitionChar <- Set.toList(alphabet),
                                              not $ elem transitionChar (Map.keys state)])


minimizeAutomata :: Automata -> Automata
minimizeAutomata automata = createMinimizeAutomata automata finalEqClasses
  where states' = states automata
        acceptStates' = acceptStates automata
        initEqClasses = getInitEqClasses states' acceptStates'
        finalEqClasses = getFinalEqClasses automata initEqClasses


--States are divided into two groups accept states and nonaccept states
getInitEqClasses :: [State] -> [State] -> EqClasses
getInitEqClasses states acceptStates = Map.fromList([if elem x acceptStates
                                                     then (x, acceptStatesMin)
                                                     else (x, nonacceptStatesMin) |
                                                           x <- states])
  where acceptStatesMin = minimum acceptStates
        nonacceptStatesMin = minimum [x | x <- states, not $ elem x acceptStates]


--If next equivalence classes are the same return them otherwise get next
getFinalEqClasses :: Automata -> EqClasses -> EqClasses
getFinalEqClasses automata eqClasses
  | List.sort (Map.elems nextEqClasses) == List.sort (Map.elems eqClasses) = eqClasses
  | otherwise = getFinalEqClasses automata nextEqClasses
  where nextEqClasses = getNextEqClasses automata eqClasses


getNextEqClasses :: Automata -> EqClasses -> EqClasses
getNextEqClasses automata eqClasses = Map.fromList([(state, getEqClass statesTable state) |
                                                    state <- states'])
  where states' = states automata
        transitions' = transitions automata
        statesTable = getStatesTable states' transitions' eqClasses


{-|
  Table represents new equivalence classes based on transitions between previous
  equivalence classes.
  Key is list of equivalence classes, first element is actual equivalence class
  (from previous iteration) and the others are equivalence class after transition.
  This list represents new equivalence class (equivalence class id).
  Elem is list of states that belong to equivalence class represented by key.
-}
getStatesTable :: [State] -> Map.Map State StateTransition -> EqClasses -> Map.Map [EqClass] [State]
getStatesTable [] transitions eqClasses = Map.empty
getStatesTable (state:states) transitions eqClasses =
    Map.insertWith (++) eqClassIdForState [state] (getStatesTable states transitions eqClasses)
  where eqClassIdForState = getEqClassIdForState state transitions eqClasses


--States are guaranteed to be found (no undefined states are present).
getEqClassIdForState :: State -> Map.Map State StateTransition -> EqClasses -> [EqClass]
getEqClassIdForState state transitions eqClasses = actualEqClass : finalEqClassesForState
  where (Just stateTransition) = Map.lookup state transitions
        (Just actualEqClass) = Map.lookup state eqClasses
        finalEqClassesForState = Maybe.catMaybes [Map.lookup finalState eqClasses |
                                                  finalState <- (Map.elems stateTransition)]


--Extract equivalence class from table.
getEqClass :: Map.Map [EqClass] [State] -> Int -> Int
getEqClass statesTable state = eqClass
  where eqStatesGroups = Map.elems statesTable
        --Equivalence class is represented by equivalent state with minimum value.
        eqClass = minimum (head [eqStates | eqStates <- eqStatesGroups, elem state eqStates])


--Create minimize automata using equivalence classes.
createMinimizeAutomata :: Automata -> EqClasses -> Automata
createMinimizeAutomata automata eqClasses = Automata { states = finalStates,
                                                       initialState = finalInitialState,
                                                       acceptStates = finalAcceptStates,
                                                       transitions = finalTransitios,
                                                       alphabet = alphabet automata}
  where finalStates = getFinalStates eqClasses
        finalInitialState = getFinalInitialState (initialState automata) eqClasses
        finalAcceptStates = getFinalAcceptStates (acceptStates automata) eqClasses
        finalTransitios = getFinalTransitions (transitions automata) (states automata) eqClasses


getFinalStates :: EqClasses -> [State]
getFinalStates eqClasses = unique $ Map.elems eqClasses


getFinalInitialState :: State -> EqClasses -> State
getFinalInitialState initialState eqClasses = finalInitialState
  where (Just finalInitialState) = Map.lookup initialState eqClasses


getFinalAcceptStates :: [State] -> EqClasses -> [State]
getFinalAcceptStates acceptStates eqClasses = unique $ Maybe.catMaybes [Map.lookup acceptState eqClasses |
                                                                        acceptState <- acceptStates]


getFinalTransitions :: Map.Map State StateTransition -> [State] -> EqClasses -> Map.Map State StateTransition
getFinalTransitions transitions [] eqClasses = Map.empty
getFinalTransitions transitions (state:states) eqClasses =
    Map.insert eqState finalStateTransition (getFinalTransitions transitions states eqClasses)
  where (Just stateTransition) = Map.lookup state transitions
        finalStateTransition = Map.fromList $ [(fst charTransition, Maybe.fromJust $
                                                Map.lookup (snd charTransition) eqClasses) |
                                                charTransition <- Map.toList stateTransition]
        (Just eqState) = Map.lookup state eqClasses


unique :: [Int] -> [Int]
unique l = Set.toList $ Set.fromList l


--Convert Automata to string representation
showAutomata :: Automata -> String
showAutomata automata = showStates ++ showInitialState ++ showAcceptStates ++ showTransitions
 where showStates = (tail (init (show (states automata)))) ++ "\n"
       showInitialState = (show (initialState automata)) ++ "\n"
       showAcceptStates = (tail (init (show (acceptStates automata)))) ++ "\n"
       showTransitions = getShowTransitions $ Map.toList (transitions automata)


getShowTransitions :: [(State, StateTransition)] -> String
getShowTransitions [] = ""
getShowTransitions (transition:transitions) =
    getShowTransition (fst transition) (Map.toList(snd transition)) ++ getShowTransitions transitions


getShowTransition :: State -> [(TransitionChar, State)] -> String
getShowTransition state [] = ""
getShowTransition state (charTransition:charTransitions) =
    show state ++ "," ++ [fst charTransition] ++ "," ++ (show (snd charTransition)) ++
         "\n" ++ (getShowTransition state charTransitions)
