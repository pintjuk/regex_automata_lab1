import Text.ParserCombinators.Parsec
import System.Environment
import Data.Maybe
import Data.List
import qualified Text.Regex.Base as Posix
import qualified Text.Regex.Posix as Posix 
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

data Regex      = Atom Char
                | Dot
                | Or Regex Regex
                | Follow Regex Regex
                | Star Regex
                | Plus Regex
                | Question Regex
                deriving (Show)

data AbsRegex   = Symbol Char
                | Any
                | Epsilon
                | Noset
                | Union AbsRegex AbsRegex
                | Concat AbsRegex AbsRegex
                | Repeat AbsRegex
                deriving (Show)

-- subset of Epsilon-NDA with one final state, with epsilon being represented with "Nothing" 
data ENDA = ENDA {  states :: [Integer],
                    transitions :: [(Integer, Maybe Char, Integer)],
                    start :: Integer,
                    final :: Integer,
                    langENDA :: [Char] }
            deriving (Show)

data FDA = FDA {    statesFDA :: [[Integer]],
                    transitonsFDA :: [([Integer], Char, [Integer])],
                    startFDA :: [Integer],
                    finalFDA :: [[Integer]],
                    langFDA :: [Char]}
            deriving (Show)
-- simplified fda 
data SFDA = SFDA {    statesSFDA :: [Integer],
                    transitonsSFDA :: [(Integer, Char, Integer)],
                    startSFDA :: Integer,
                    finalSFDA :: [Integer],
                    langSFDA :: [Char]}
            deriving (Show)

------------------------------ PARSER ----------------------------------
parseRegexAtom :: Parser Regex
parseRegexAtom = do
    x <- noneOf("()|.*+?\\")
    return $ Atom x

parseRegexAny :: Parser Regex
parseRegexAny = do
    char '.'
    return Dot


parseConcats :: Parser Regex
parseConcats = do 
            x <- many1 $ try parseStar <|>parseRegexAtom <|> parseRegexAny <|> do 
                char '('
                z <- parseRegexExp
                char ')'
                return z
            return ( case x of [i]    -> i
                               (i:ii) -> foldl (Follow) i ii )

parseUnion :: Parser Regex
parseUnion = do
            x <- sepBy1  parseConcats (char '|')
            return $ case x of [i]    -> i
                               (i:ii) -> foldl (Or) i ii
                               
parseStar :: Parser Regex
parseStar = do
            x <- parseRegexAtom <|> parseRegexAny <|> do
                char '('
                z <- parseRegexExp
                char ')'
                return z
            y <- oneOf "*+?"
            return $ case y of  '*' -> Star x 
                                '+' -> Plus x
                                '?' -> Question x
parseRegexExp :: Parser Regex
parseRegexExp = parseUnion
-------------------------------------------------------------------------------------------------
------------------------------------------- REGEX TO ABSREGEX-------------------------------------
regexToAbsRegex :: Regex -> [Char] -> AbsRegex
regexToAbsRegex  (Star x) z = Repeat $ regexToAbsRegex x z
regexToAbsRegex (Plus x) z = Concat  (regexToAbsRegex x z) (Repeat $ regexToAbsRegex x z)
regexToAbsRegex (Question x) z = Union Epsilon $ regexToAbsRegex x z
regexToAbsRegex (Atom x) _ = Symbol x
regexToAbsRegex (Dot) z =  Any {-case z of [x] -> Symbol x 
                                    (x:xs) -> foldl (\i ii -> Union i (Symbol ii))  (Symbol x) xs-} 
regexToAbsRegex (Or x y) z = Union  (regexToAbsRegex x z) (regexToAbsRegex y z)
regexToAbsRegex (Follow x y) z = Concat  (regexToAbsRegex x z) ( regexToAbsRegex y z)

-------------------------------------------REGEX to eNDA ------------------------------------------
--states1 = (*2)
--states2 =(+1) . states1
--transitions1 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer) 
--transitions1 (from, tran, to) = (states1 from, tran, states1 to)
--transitions2 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer)
--transitions2 (from, tran, to) = (states2 from, tran, states2 to)

regexToENDA :: [Char]->AbsRegex -> ENDA
regexToENDA lang Epsilon = ENDA [0, 1] [(0, Nothing, 1)] 0 1 lang
regexToENDA lang (Symbol x)  = ENDA [0, 1] [(0, Just x, 1)] 0 1 lang
regexToENDA lang Noset   = ENDA [0, 1] [] 0 1 lang
regexToENDA lang Any = ENDA [0, 1] t 0 1 lang
    where t= map (\x-> (0, Just x, 1) ) lang
regexToENDA lang (Union regex1 regex2) = 
    ENDA q d s f lang
    where
            states1 = id
            states2 = (+ (toInteger. length $states enda1))
            transitions1 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer) 
            transitions1 (from, tran, to) = (states1 from, tran, states1 to)
            transitions2 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer)
            transitions2 (from, tran, to) = (states2 from, tran, states2 to)
            enda1 = regexToENDA lang regex1
            enda2 = regexToENDA lang regex2
            q1 = states1 <$> states enda1
            q2 = states2 <$> states enda2
            d1 = transitions1 <$> transitions enda1
            d2 = transitions2 <$> transitions enda2
            s1 = states1 $ start enda1
            s2 = states2 $ start enda2
            f1 = states1 $ final enda1
            f2 = states2 $ final enda2
            q = s:f:( q1 ++ q2 ) 
            d = d1 ++ d2 ++ [(s, Nothing, s1), (s, Nothing,  s2), (f1, Nothing, f), (f2, Nothing, f)]
            f=(maximum $ q1 ++ q2) + 1
            s= f+1
{-regexToENDA lang (Union regex1 regex2) = 
    ENDA q d s f lang
    where
            states1 = id
            states2 x = if x == (final enda2) then
                            (final enda1)
                        else if x == (start enda2) then
                            (start enda1)
                        else ( x + (toInteger. length $states enda1))
            transitions1 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer) 
            transitions1 (from, tran, to) = (states1 from, tran, states1 to)
            transitions2 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer)
            transitions2 (from, tran, to) = (states2 from, tran, states2 to)
            enda1 = regexToENDA lang regex1
            enda2 = regexToENDA lang regex2
            q1 = states1 <$> states enda1
            q2 = states2 <$> states enda2
            d1 = transitions1 <$> transitions enda1
            d2 = transitions2 <$> transitions enda2
            s1 = states1 $ start enda1
            s2 = states2 $ start enda2
            f1 = states1 $ final enda1
            f2 = states2 $ final enda2
            q = nub $ s:f:( q1 ++ q2 ) 
            d = nub $ d1 ++ d2
            f= f1
            s= s1
           -}
regexToENDA lang (Concat regex1 regex2) = 
    ENDA q d s1 f2 lang
    where   
            states1 = id
            states2 = (+ (toInteger. length $states enda1))
            transitions1 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer) 
            transitions1 (from, tran, to) = (states1 from, tran, states1 to)
            transitions2 :: (Integer, Maybe Char, Integer) -> (Integer, Maybe Char, Integer)
            transitions2 (from, tran, to) = (states2 from, tran, states2 to)
            enda1 =regexToENDA lang regex1
            enda2 = regexToENDA lang regex2
            q1 = states1 <$> states enda1
            q2 = states2 <$> states enda2
            d1 = transitions1 <$> transitions enda1
            d2 = transitions2 <$> transitions enda2
            s1 = states1 $ start enda1
            s2 = states2 $ start enda2
            f1 = states1 $ final enda1
            f2 = states2 $ final enda2
            q = q1 ++ q2
            d = (f1, Nothing, s2):d1 ++ d2



regexToENDA lang (Repeat regex1) = 
    ENDA q d s f lang
    where   
            enda1 = regexToENDA lang regex1
            q1 = states enda1
            d1 = transitions enda1
            s1 = start enda1
            f1 = final enda1
            q = s:f:(q1) 
            d = (s, Nothing, f):( f1, Nothing, s1):
                (s, Nothing, s1):( f1, Nothing, f):
                ( d1)
            f = ( maximum q1) + 1
            s = f+1
-------------------------------- ENDA to FDA ------------------------------------------------------
eClosure :: [(Integer, Maybe Char, Integer)] -> Integer -> [Integer]
eClosure trans state 
    | nextEpsilon == [] = [state]
    | otherwise         = state:(foldl (\x y -> x ++ (eClosure (trans\\nextTrans) y) ) [] nextEpsilon )
    where   isEpsilon (_, x, _) = (x==Nothing)
            fromeHere (x, _, _) = (x==state)
            getState  (_, _, x) = x
            nextEpsilon    = (map getState) . (filter isEpsilon ) $ nextTrans
            nextTrans = filter fromeHere trans     

eNDAtoFDA :: ENDA -> FDA
{-eNDAtoFDA enda = eNDAtoFDA' enda (FDA [startFDA] [] startFDA [] (langENDA enda))
    where   startFDA= eClosure ( transitions enda ) (start enda)
            eNDAtoFDA' (ENDA stENDA trENDA sENDA fENDA lENDA) (FDA stFDA trFDA sFDA fFDA lFDA) 
                | (allNextTransitions \\ trFDA) == [] = (FDA stFDA trFDA sFDA (filter (elem fENDA) stFDA) lFDA)
                | otherwise =  eNDAtoFDA' (ENDA stENDA trENDA sENDA fENDA lENDA) nextFDA 
                where   nextFDA = FDA (nub $ stFDA ++ allNextStates) (nub $ trFDA ++ allNextTransitions) sFDA fFDA lFDA
                        subsetAllTransToNext :: [Integer] -> Char -> ([Integer], Char, [Integer])
                        subsetAllTransToNext state letter = toTrans $ foldl (++) [] $ getNextState <$> filter isTransToNext trENDA
                            where   toTrans ns = (state, letter,ns)
                                    getNextState (_,_, x) = eClosure trENDA x
                                    isTransToNext (x, y, _) = and [y== (Just letter), elem x state]
                        allNextTransitions :: [([Integer], Char, [Integer])]
                        allNextTransitions = subsetAllTransToNext <$> stFDA <*> lENDA
                        allNextStates = (\(_, _, x) -> x) <$> allNextTransitions-}

eNDAtoFDA enda = eNDAtoFDA' enda (FDA [startFDA] [] startFDA [] (langENDA enda)) [startFDA]
    where   startFDA= eClosure ( transitions enda ) (start enda)
            eNDAtoFDA' (ENDA stENDA trENDA sENDA fENDA lENDA) (FDA stFDA trFDA sFDA fFDA lFDA) activeStates 
                | activeStates == [] = (FDA stFDA trFDA sFDA (filter (elem fENDA) stFDA) lFDA)
                | otherwise = eNDAtoFDA' (ENDA stENDA trENDA sENDA fENDA lENDA) nextFDA (allNextStates \\ stFDA)
                where   nextFDA = FDA (nub $ stFDA ++ allNextStates) (nub $ trFDA ++ allNextTransitions) sFDA fFDA lFDA
                        subsetAllTransToNext :: [Integer] -> Char -> ([Integer], Char, [Integer])
                        subsetAllTransToNext state letter = toTrans $ foldl (++) [] $ getNextState <$> filter isTransToNext trENDA
                            where   toTrans ns = (state, letter,ns)
                                    getNextState (_,_, x) = eClosure trENDA x
                                    isTransToNext (x, y, _) = and [y== (Just letter), elem x state]
                        allNextTransitions :: [([Integer], Char, [Integer])]
                        allNextTransitions = nub $ subsetAllTransToNext <$> activeStates <*> lENDA
                        allNextStates = nub $ (\(_, _, x) -> x) <$> allNextTransitions

---------------------------------------------------------------------------------------------------

----------------------------------- Simplify ------------------------------------------------------

simplifyFDA::FDA -> SFDA
simplifyFDA (FDA q d s f lang) = SFDA (simpState <$> q) ((\(a, b, c)->(simpState a, b, simpState c)) <$> d) (simpState s) (simpState <$> f) lang
    where simpState = (\(_, b)->b) . (\x-> (filter (\(a, _)->a==x) $ zip q (toInteger <$> [1..length q]))!!0)  

----------------------------------- Prity print ---------------------------------------------------
prityENDA ::  ENDA -> [Char]
prityENDA (ENDA st d s f lang) = heading  ++ body ++ "\n\n"
        where   body        = foldl (\a b -> a++"\n"++b ) ""  $ line <$> q
                heading     = (foldl (\x y-> x++[alphToChar y]++"\t") "\t" ( sort . nub $ alpha) ) ++ "\n"
                alpha = Nothing : (sort $ Just <$> lang)
                alphToChar = maybe 'ε' id 
                q= sort st 
                line :: Integer -> [Char]
                line state = foldl (\x y-> x ++ "\t" ++ y)  (startSym ++ show state ++ ":") $  foldl (\x y-> x ++ show y++",") "" <$> absline
                        where   absline :: [[Integer]]
                                absline = pure (\letter transes-> 
                                                    foldl (\bucket (_,thisletter,tostate) ->
                                                        bucket++(if thisletter==letter then [tostate] else [] ) 
                                                    ) [] transes
                                                )  
                                        <*> alpha 
                                        <*> [ filter (\(x,_,_)-> x==state) d]
                                startSym
                                        | and [state == s, state==f] = "->*"
                                        | state == s = " ->"
                                        | state == f = "  *"
                                        | otherwise = "   "

dotENDA :: ENDA -> IO ()
dotENDA (ENDA st d s f lang) = do 
                        putStrLn "aoeu"
                        putStrLn "aoeu"


prityFDA :: SFDA -> [Char]
prityFDA (SFDA q d s f alpha) = heading  ++ body ++ "\n\n"  
        where   body        = foldl (\a b -> a++"\n"++b ) ""  $ line <$> q
                heading     = (foldl (\x y-> x++[y]++"\t") "\t" ( sort . nub $ alpha) ) ++ "\n"
                line :: Integer -> [Char]
                line state  = foldl (\x (_,_,a) -> x++(show a)++"\t") (startSym ++ show state ++ ":\t") $ sortBy (\(_,a,_) (_,b,_)-> compare a b ) $ filter (\(x,_,_)-> x==state) d
                    where   startSym
                                    | and [state == s, elem state f] = "->*"
                                    | state == s = " ->"
                                    | elem state f = "  *"
                                    | otherwise = "   "

---------------------------------------------------------------------------------------------------
------------------------------------ MINIMIZE FDA -------------------------------------------------

removeUnreachable ::  SFDA -> SFDA
removeUnreachable (SFDA q d s f lang) = SFDA reachableStates reachableTrans s (reachableStates\\(reachableStates\\f)) lang
    where   reachableStates = reachableS d s
            reachableTrans = filter (\(x, _, _)->elem x reachableStates) d

reachableS :: [(Integer, Char, Integer)] -> Integer -> [Integer]
reachableS trans state 
    | nextS == [] = [state]
    | otherwise         = state: nub (foldl (\x y -> x ++ (reachableS (trans\\nextTrans) y) ) [] nextS )
    where   fromeHere (x, _, _) = (x==state)
            getState  (_, _, x) = x
            nextS    = map getState nextTrans
            nextTrans = filter fromeHere trans
            
            
minimize :: SFDA -> SFDA
minimize = minimize' {-. removeUnreachable-}

minimize' :: SFDA -> SFDA
minimize' (SFDA q d s f lang) =(SFDA clases minTransitions (toClassMin s) (nub $ toClassMin <$> f) lang) 
        where   minTransitions =nub $ renameTrans <$> d
                renameTrans (from, sym, to)=(toClassMin from, sym , toClassMin to )
                toClassMin a = filter (sequal a) clases !! 0
                clases = sort $ nub $ minimum <$> (\x-> filter (sequal x) q) <$> q
                clases'= sort $ nub $ (\x-> filter (sequal x) q) <$> q
                sequal a b = not $ Set.member (a, b) table
                table :: Set.Set (Integer, Integer)
                table = tablefilling initTable 0
                tablefilling:: Set.Set (Integer, Integer) -> Integer -> Set.Set (Integer, Integer) 
                tablefilling table itercount
                    | newtable==table   = table
                    | otherwise         = tablefilling  newtable (itercount+1) 
                    where   newtable = foldl insertThis table allDistingwishable
                            insertThis ctable (a, b) =  Set.union ctable $ Set.fromList $ (,) <$> a <*> b
                            allDistingwishable :: [([Integer], [Integer])]
                            allDistingwishable =  (\sym (a, b)-> ( Map.findWithDefault [] (a, sym) d',  Map.findWithDefault [] (b, sym) d')) <$> lang <*> (Set.toList table) 
                initTable :: Set.Set (Integer, Integer) 
                initTable =  Set.fromList $ filter (\(x, y)-> initdist x y) $ (,) <$> q <*> q
                initdist a b = (elem a f) /= (elem b f)
                d'::Map.Map (Integer, Char) [Integer]
                d'=foldl (\ accum (from, by, to)-> 
                            Map.insert (to, by) (nub $ from:( Map.findWithDefault [] (to, by) accum)) accum 
                        ) Map.empty d
{-
 minimize' (SFDA q d s f lang) = (SFDA clases minTransitions (toClassMin s) (nub $ toClassMin <$> f) lang) 
        where   minTransitions = nub $ renameTrans <$> d
                renameTrans (from, sym, to)=(toClassMin from, sym , toClassMin to )
                toClassMin a = filter (sequal a) clases !! 0
                clases = sort $ nub $ minimum <$> (\x->trace ("aoeu") $ filter (sequal x) q) <$> q
                sequal a b = not $ table Map.! (a, b)
                table = tablefilling initTable 0
                tablefilling:: Map.Map (Integer, Integer) Bool -> Integer -> Map.Map (Integer, Integer) Bool 
                tablefilling table itercount
                    | newtable==table   = table
                    | otherwise         = trace ("tablefiling itter: " ++ show itercount) $ tablefilling  newtable (itercount+1) 
                    where   newtable = foldl insertThis table allDistingwishable
                            insertThis ctable (a, b) =  foldl (\tb tp -> Map.insert tp True tb) ctable $ mktuple <$> a <*> b
                            mktuple a b = (a, b)
                            allDistingwishable :: [([Integer], [Integer])]
                            allDistingwishable =  ( pure distPridecesors <*> q <*> q <*> lang )
                            allDistinguishedPairs = Map.partition (==True) 
                            distPridecesors :: Integer -> Integer -> Char-> ([Integer],[Integer])
                            distPridecesors a b sym
                                | table Map.! (a, b) == False = ([],[])
                                | otherwise = (fromstate <$> filter (isToHere a) d, fromstate <$> filter (isToHere b) d)
                                where   isToHere here (_,i,ii) = and [i==sym, ii==here]
                                        fromstate (i,_,_) =i
                initTable :: Map.Map (Integer, Integer) Bool
                initTable = trace ("created initial!!") $ Map.fromList $ (\x y->((x,y), initdist x y)) <$> q <*> q
                initdist a b = (elem a f) /= (elem b f)
                d'::Map.Map (Integer, Char) Integer
                d'= Map.fromList $ (\(from, on, to )->((from, on), to)) <$> d
 -}
---------------------------------------------------------------------------------------------------
-- EXICUTE ATOMATON -------------------------------------------------------------------------------


run :: SFDA -> [Char] -> Bool
run (SFDA q d s f lang) ward = run' ward s
    where run' [] curState = elem curState f
          run'  (strh:strRest)  curState = run' strRest nextState
                where nextState =  (\(_,_,nxts)->nxts)
                                $ (filter   (\(fr, sym, _)-> 
                                                and  [fr==curState, sym==strh] 
                                            ) d) !! 0

regStringToENDA:: [Char] -> [Char] -> Either ParseError ENDA 
regStringToENDA r z = either Left (\x-> Right $ regexToENDA z $ regexToAbsRegex x z) $ parse parseUnion "regex" r 
regStringToFDA:: [Char] -> [Char] -> Either ParseError SFDA  
regStringToFDA r z = either Left ( Right . simplifyFDA . eNDAtoFDA) $ regStringToENDA r z
printFDA::  Either ParseError SFDA -> IO () 
printFDA x= putStr $ either show (prityFDA) $ x
printENDA :: Either ParseError ENDA -> IO () 
printENDA x = putStr $ either show (prityENDA) x

match' :: [Char] -> [Char] -> [Char]-> Either ParseError Bool
match' lang pattern ward = either Left (\x-> Right $ run x ward) 
                       $ regStringToFDA pattern lang
z= "aoeuidhtn"
match = match' $ nub "åäöpyfgcrlaoeuidhtns.qjkxbm123456789"


---------------------------------------------------------------------------------------------------
readExpr :: String -> String
readExpr input = case parse parseUnion "regex" input of
    Left err -> "No match: " ++ show err
    Right res  -> "Found value:"++ show res

main :: IO ()
{-main = do 
        --(expr:_) <- getArgs
        l<-  getLine
       -- z<- getLine
        putStr $ either (\_-> "error!" ) ( prityFDA .minimize   . simplifyFDA .  eNDAtoFDA ) $ regStringToENDA l $ nub z
-}
replCS:: Char->[Char]->[Char]->[Char]
replCS chr rplstr str= foldl (\accum oldchar -> if oldchar== chr then accum++rplstr else accum++[oldchar]) "" str 
main = do
    (file:oargs) <- getArgs
    (lang:reg:corpus) <- lines <$> readFile file
    putStrLn $ "lang: " ++ lang
    putStrLn $ "regex: " ++ reg
    putStrLn $ "parsing result:\n" ++ ( readExpr reg )

    let enda = regStringToENDA (".*("++reg++").*") lang
    putStrLn $ "eNDA number of states: " ++  (show ( length.states <$> enda))
    --printENDA enda
    let eautomata' = simplifyFDA <$> eNDAtoFDA <$> enda
    putStrLn $ "FDA number of states before minimization: "++ (show (length.statesSFDA <$> eautomata')) 
    let eautomata = (
                        if (length oargs)==1 
                        then minimize                      
                        else id
                    )
                    <$> eautomata'
    if (length oargs)==1
    then putStrLn $ "FDA number of states after minimization: " ++ (show (length.statesSFDA <$> eautomata))
    else putStr ""
    
    putStrLn "running"
    putStrLn "\\aoeu"
--    printFDA eautomata'
--    printFDA eautomata 

    --printFDA $ eautomata 
    case eautomata of
        Left err -> putStrLn $ "No match: "++ show err
        Right automata -> sequence_ $ map (\line -> do 
            putStrLn line
            let myresult = run automata line
            let escapedReg = replCS '^' "\\^"
                           $ replCS ']' "\\]"
                           $ replCS '[' "\\[" reg

            let rightresult = (line Posix.=~ escapedReg ::Bool) 
            if myresult==rightresult 
                then putStrLn $ "result: "++(show myresult)++"\nPASSED!"
                else putStrLn $ "result: "++(show myresult) ++ "\nExpected:" ++ (show rightresult)++"\n******** FAILED! ********") corpus
            --) corpus
