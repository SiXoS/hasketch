module Server where

import Room(Room)
import qualified Room as R
import qualified JSONTypes as JS

import System.Directory(getDirectoryContents,doesFileExist)


import qualified Data.Map as M
import Data.Map(Map)

import qualified Data.Set as S
import Data.Set(Set)

import Text.Regex.PCRE

import Data.Time.Clock.POSIX(POSIXTime,getPOSIXTime)
import Data.List(delete)
import Data.Aeson(FromJSON,ToJSON,toJSON,Value,fromJSON,Result(..))
import Data.Maybe(Maybe(..),isJust,fromJust,isNothing)
import Data.Char(toLower)

import Control.Monad.Except

data Corridor = Corridor {
  rooms :: Map String Room
  ,inCorridor :: Set Int
  ,users :: Map Int String}
  deriving (Show)

emptyCorridor :: Corridor
emptyCorridor = Corridor M.empty S.empty M.empty       

type Server a = Corridor -> (Corridor,[Transmission a])
type ServerErr a = Corridor -> Err (Corridor,[Transmission a])

data Receiver = All [Int] | Sender | User Int
  deriving (Eq,Show)
type Transmission a = (Receiver, a)

data Err a = Bad String | Ok a | None

isGood :: Err a -> Bool
isGood (Ok _) = True
isGood _      = False

handleRequest :: Corridor -> Value -> Int -> ExceptT String IO (Corridor,[Transmission Value])
handleRequest corr val uid = case fromJSON val of
  Error str -> throwError str
  Success cm@(JS.ClientMessage cmd room dat) -> case cmd of
    "enterCorridor" -> return $ enterCorridor uid corr
    "setName" -> do
      nm <- checkExists dat
      fromErr' cmd $ setName uid nm corr
    _ -> if not $ M.member uid $ users corr
           then fromErr' cmd $ Bad "You are not logged in."
           else handleRequest' corr cm uid
  where
    fromErr' = fromErr corr

handleRequest' :: Corridor -> JS.ClientMessage -> Int -> ExceptT String IO (Corridor,[Transmission Value])
handleRequest' corr cm@(JS.ClientMessage cmd room dat) uid = case cmd of
  "leaveCorridor" -> return $ leaveCorridor uid corr
  "createRoom" -> do
    cr <- checkExists dat
    ws <- liftIO $ getWords (JS.wordList cr)
    if ws == Nothing
      then fromErr' cmd $ Bad "Word list doesn't exist."
      else fromErr' cmd $ createRoom uid cr (fromJust ws) corr
  "logOut" -> return $ logOut uid corr
  "getWordlists" -> do
    lsts <- liftIO getWordlists
    return $ (corr,[(Sender, lsts)])
  "hint" -> do
    rm <- checkExists room
    fromErr' cmd $ hint uid rm corr
  "newPresentator" -> do
    rm <- checkExists room
    dt <- checkExists dat :: ExceptT String IO String
    let frc = length dt > 0
    now <- liftIO getPOSIXTime
    fromErr' cmd $ newPresentator uid rm now frc corr
  "joinRoom" -> do
    jr <- checkExists dat
    now <- liftIO getPOSIXTime
    fromErr' cmd $ joinRoom uid jr now corr
  "leaveRoom" -> do
    rm <- checkExists room
    now <- liftIO getPOSIXTime
    fromErr' cmd $ leaveRoom uid rm now corr
  "chat" -> do
    rm <- maybe (return Nothing) (\r -> case fromJSON r of
                                      Success x -> return $ Just x
                                      Error e -> throwError e) room
    dt <- checkExists dat
    now <- liftIO getPOSIXTime
    fromErr' cmd $ chat uid rm dt now corr
  "draw" -> do
    rm <- checkExists room
    dt <- checkExists dat
    fromErr' cmd $ draw uid rm dt corr
  x -> throwError $ "Invalid command: " ++ x
  where
    fromErr' = fromErr corr

disconnect :: Int -> Corridor -> IO (Corridor,[Transmission Value])
disconnect uid corr = do
  now <- getPOSIXTime
  return $ disconnect' uid now corr

disconnect' :: Int -> POSIXTime -> Server Value
disconnect' uid now corr
  | userIsLoggedIn && rmFound = (delUs corr',tsm)
  | userIsLoggedIn = (remUCor $ delUs corr,[])
  | otherwise = (remUCor corr,[])
  where
    userIsLoggedIn = M.member uid $ users corr
    remUCor c = c{inCorridor = S.delete uid $ inCorridor c}
    delUs c = c{users = M.delete uid $ users c}
    mrm = filter (M.member uid . R.score) $ M.elems $ rooms corr
    rmFound = length mrm > 0
    rm = R.name (mrm !! 0)
    (corr',tsm) = (\(Ok x) -> x) $ leaveRoom uid rm now corr -- As we've already found the room, it can only return Ok

fromErr :: Corridor -> String -> Err (Corridor,[Transmission Value]) -> ExceptT String IO (Corridor,[Transmission Value])
fromErr corr _ (None) = return (corr,[])
fromErr corr cmd (Bad e) = return (corr,[(Sender,JS.error cmd e)])
fromErr _ _ (Ok x) = return x

checkExists :: FromJSON a => Maybe Value -> ExceptT String IO a
checkExists dat
  | dat == Nothing = throwError "Required param was nothing."
  | otherwise = case fromJSON $ fromJust dat of
                  Error e -> throwError e
                  Success str -> return str

enterCorridor :: Int -> Server Value
enterCorridor client serv = (serv{inCorridor = S.insert client (inCorridor serv)}
                            , [(Sender,JS.withData "enterCorridor" allrooms)])
  where
    allrooms = map roomToSum $ M.elems $ rooms serv
    roomToSum room = JS.RoomSum (R.name room) (R.maxUsers room) (M.size $ R.score room) (isJust $ R.password room)  

setName :: Int -> String -> ServerErr Value
setName usr uName corr 
  | M.member usr usrs = Bad "You are already logged in"
  | hasValue usrs uName = Bad "Username taken"
  | not matchesName = Bad "Invalid username"
  | otherwise = Ok (corr{users = M.insert usr uName usrs},[(Sender,JS.ack "setName")])
  where
    matchesName = match (makeRegexOpts compCaseless execBlank "^(\\w|\\d){4,12}$") uName :: Bool
    usrNameExists = any (strCaseComp uName) $ M.elems $ users corr
    usrs = users corr

leaveCorridor :: Int -> Server Value
leaveCorridor usr corr = (corr{users = M.delete usr (users corr)},[(Sender,JS.ack "leaveCorridor")])

createRoom :: Int -> JS.CreateRoom -> [String] -> ServerErr Value
createRoom uid (JS.CreateRoom nme timer maxUsr finTimer pass maxRounds wordList) wrds corr 
  | roomExists = Bad "Room name already exists"
  | otherwise = Ok (corr{rooms = M.insert nme room (rooms corr),inCorridor = corridorers}
                   ,[(Sender,JS.ack "createRoom"),(All $ S.toList corridorers, JS.withData "newRoom" roomSum)])
  where
    roomExists = any (strCaseComp nme) $ M.keys $ rooms corr
    troom = R.newRoom nme timer maxUsr finTimer pass maxRounds wordList wrds
    (R.Norm room) = R.welcome uid pass troom
    corridorers = S.delete uid $ inCorridor corr
    roomSum = JS.RoomSum nme maxUsr 1 (isJust pass)

logOut :: Int -> Server Value
logOut usr corr = (corr{users = M.delete usr (users corr)},[(Sender, JS.ack "logOut")])

getWordlists :: IO Value
getWordlists = do
  lists <- getDirectoryContents "wordlists"
  return $ JS.withData "getWordlists" $ filter (not.isDot) lists
  where
    isDot :: String -> Bool
    isDot "."  = True
    isDot ".." = True
    isDot _    = False

getWords :: String -> IO (Maybe [String])
getWords lst = do
  exsts <- doesFileExist $ "wordlists/" ++ lst
  if not exsts
    then return Nothing
    else readFile ("wordlists/" ++ lst) >>= return . Just . lines

hint :: Int -> String -> ServerErr Value
hint u rm corr
  | isNothing room = None
  | isNothing hnt = Bad "You can't give more hints."
  | otherwise = Ok $ (corr{rooms = M.insert (R.name jroom) jroom (rooms corr)}
                     ,[(All vstrs,JS.withData "hint" jhnt)])
  where
    room = M.lookup rm (rooms corr) >>= (\rm' -> if M.member u (R.score rm') then Just rm' else Nothing)
    hnt = R.hint $ fromJust room
    (jhnt,jroom) = fromJust hnt
    vstrs = M.keys $ R.score jroom

newPresentator :: Int -> String -> POSIXTime -> Bool -> ServerErr Value
newPresentator uid rm now skip corr
  | noCorrRoom || noNewPres = None
  | skip && someGuessedRight = Bad "You can't skip a word if someone has guessed correct."
  | otherwise = case happening of
    R.GameEnd -> Ok (corr',[(All (pres:vstrs), JS.ack "gameOver")])
    _         -> Ok (corr',[(All vstrs
                            ,JS.withData "presentatorChange" (fromJust $ M.lookup pres (users corr)))
                           ,(User pres, JS.withData "becamePresentator" (R.word rm'))] ++ rChange)
  where
    foundRoom = M.lookup rm (rooms corr)
    someGuessedRight = R.hasGuessedRight (fromJust foundRoom)
    noCorrRoom = isNothing $ foundRoom >>= M.lookup uid . R.score
    res = R.setNewPresentator (fromJust foundRoom) False now
    noNewPres = isNothing res
    (happening,rm') = fromJust res
    pres = R.presentator rm'
    vstrs = M.keys $ M.delete pres $ R.score rm'
    corr' = corr{rooms = M.insert rm rm' (rooms corr)}
    rChange = if happening == R.RoundChange then [(All vstrs,JS.ack "roundChange")] else []

joinRoom :: Int -> JS.JoinRoom -> POSIXTime ->  ServerErr Value
joinRoom uid (JS.JoinRoom rm pass) now corr
  | noRoom = Bad "That room doesn't exist"
  | alreadyInRoom = Bad "You are already in a room"
  | otherwise = case R.welcome uid pass rm' of
      R.WrongPass -> Bad "Wrong password."
      R.FullRoom -> Bad "The room is full."
      R.PresentatorChange rm'' -> let newRoom = snd $ fromJust $ R.setNewPresentator rm'' True now in
        Ok (newCorr newRoom 
           ,[(Sender,JS.withData "joinRoom" $ joinRoomData newRoom)
            ,(User $ R.presentator newRoom, JS.withData "becamePresentator" (R.word newRoom))
            ,(All $ M.keys $ M.delete (R.presentator newRoom) $ R.score newRoom, JS.withData "presentatorChange" (users corr M.! R.presentator newRoom))
            ,(All $ M.keys $ R.score rm', JS.withData "newUser" (users corr M.! uid))])
      R.Norm rm'' -> Ok (newCorr rm''
                        ,[(Sender,JS.withData "joinRoom" $ joinRoomData rm'')
                         ,(All $ M.keys $ M.delete uid $ R.score rm'', JS.withData "newUser" (users corr M.! uid))])
  where
    alreadyInRoom = any (M.member uid . R.score) $ M.elems $ rooms corr
    newCorr nrm = corr{inCorridor = S.delete uid (inCorridor corr)
                      ,rooms = M.insert rm nrm (rooms corr)}
    trm = M.lookup rm $ rooms corr
    noRoom = isNothing trm
    rm' = fromJust trm
    tt r = if R.hasGuessedRight r then JS.Finishing else JS.Normal
    since r = round $ (if R.hasGuessedRight r then R.guessedRightAt else R.newWordSet) r * 1000
    getScore r (u,scr) = JS.Score scr (users corr M.! u) (S.member u $ R.guessedRight r)
    scoreData r = map (getScore r) $ M.assocs $ R.score r
    joinRoomData r = JS.Room rm (users corr M.! R.presentator r) (scoreData r) (R.timerLength r)
                             (R.finTimerLength r)
                             (R.round r) (R.maxRounds rm') (tt r) (round (now*1000) - since r)

leaveRoom :: Int -> String -> POSIXTime -> ServerErr Value
leaveRoom uid rm now corr
  | noRoom = None
  | otherwise = case R.kickOut uid trm of
      (R.NoChange,rm') -> Ok (newCorr rm',[(All $ M.keys $ R.score rm'
                                           ,JS.withData "userLeave" (users corr M.! uid))
                                          ,(User uid,JS.ack "leaveRoom")])
      (R.DeleteRoom,_) -> Ok (corr{rooms = M.delete rm $ rooms corr
                                ,inCorridor = S.insert uid $ inCorridor corr}
                           ,[(All $ S.toList $ inCorridor corr,JS.withData "roomDeleted" rm)
                            ,(User uid, JS.ack "leaveRoom")])
      (R.NewPresentator,rm') -> let rm'' = snd $ fromJust $ R.setNewPresentator rm' True now in
                               Ok (newCorr rm''
                                  ,[(User uid,JS.ack "leaveRoom")
                                   ,(User $ R.presentator rm'', JS.withData "becamePresentator" (R.word rm''))
                                   ,(All $ M.keys $ R.score rm'', JS.withData "presentatorChange"
                                                                  (users corr M.! R.presentator rm''))
                                   ,(All $ M.keys $ M.delete (R.presentator rm'') $ R.score rm''
                                    ,JS.withData "userLeave" (users corr M.! uid))])
  where
    newCorr r = corr{inCorridor = S.insert uid $ inCorridor corr, rooms = M.insert rm r $ rooms corr}
    res = M.lookup rm $ rooms corr
    noRoom = isNothing res
    trm = fromJust res

chat :: Int -> Maybe String -> String -> POSIXTime -> ServerErr Value
chat uid rm txt now corr
  | rm == Nothing = if S.member uid $ inCorridor corr
                    then Ok (corr,[(All $ S.toList $ S.delete uid $ inCorridor corr ,JS.chat uName txt)])
                    else None
  | noCorrRoom = None
  | R.presentator trm' /= uid && gCorrect = Ok (corr{rooms = M.insert (R.name rm') rm' $ rooms corr}
                                              ,[(All $ M.keys $ R.score rm'
                                                ,JS.withData "guessedCorrect" $ JS.CorrectGuess uName scr)])
  | R.presentator trm' /= uid && almost = Ok (corr,[(Sender, JS.error "chat" "You are close!")])
  | otherwise = Ok (corr,[(All $ M.keys $ M.delete uid $ R.score trm', JS.chat uName txt)])
  where
    uName = users corr M.! uid
    tmrm = M.lookup (fromJust rm) (rooms corr)
    noCorrRoom = isNothing tmrm || not (M.member uid $ R.score $ fromJust tmrm)
    trm' = fromJust tmrm
    res = R.guess txt uid now trm'
    (gCorrect,dat,almost) = case res of
                             R.Wrong -> (False,undefined,False)
                             R.Almost -> (False,undefined,True)
                             R.Correct x -> (True,x,False)
    (scr,rm') = dat

draw :: Int -> String -> Value -> ServerErr Value
draw uid rm coords corr
  | noCorrRoom = None
  | R.presentator rm' == uid = Ok (corr,[(All sendTo, JS.withData "draw" coords)])
  | otherwise = Bad "You are not the presentator."
  where
    tmrm = M.lookup rm (rooms corr)
    noCorrRoom = isNothing tmrm || not (M.member uid $ R.score $ fromJust tmrm)
    rm' = fromJust tmrm
    sendTo = M.keys $ M.delete uid $ R.score rm'

hasValue :: Map k String -> String -> Bool
hasValue mp v = any (strCaseComp v) (M.elems mp)

strCaseComp :: String -> String -> Bool
strCaseComp s1 s2 = map toLower s1 == map toLower s2

findElemWith :: Eq a => (a -> Bool) -> [a] -> Maybe a
findElemWith f (x:xs) | f x = Just x
                      | otherwise = findElemWith f xs
findElemWith f _ = Nothing
