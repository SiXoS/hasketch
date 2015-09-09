module Room(
  Room(Room),
  score,name,presentator,word,timerLength,newWordSet,guessedRight,guessedRightAt,hasGuessedRight,maxUsers,
  finTimerLength,password,maxRounds,round,hasDrawn,gameEnded,hintsGiven,wordList,words,nextPresentator,
  newRoom,
  hint,
  guess, GuessResponse(..),
  welcome, WelcomeResponse(..),
  kickOut, LeftResponse(..),
  setNewPresentator, PresentatorResponse(..)
           ) where

import Prelude hiding(round, words)
import qualified Prelude as P

import System.Random(StdGen,randomR,mkStdGen)

import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock.POSIX(POSIXTime,getPOSIXTime)

type Sec = Int  -- Seconds

data Room = Room {
   score           :: Map Int Int
  ,name            :: String
  ,presentator     :: Int
  ,word            :: String
  ,timerLength     :: Sec
  ,newWordSet      :: POSIXTime
  ,guessedRight    :: Set Int
  ,hasGuessedRight :: Bool
  ,guessedRightAt  :: POSIXTime
  ,maxUsers        :: Int
  ,finTimerLength  :: Int
  ,password        :: Maybe String
  ,maxRounds       :: Int
  ,round           :: Int
  ,hasDrawn        :: Set Int
  ,gameEnded       :: Bool
  ,hintsGiven      :: Int
  ,wordList        :: String
  ,words           :: [String]
  ,nextPresentator :: Int
  }
  deriving (Show)

newRoom :: String -> Int -> Int -> Int -> Maybe String -> Int -> String -> [String] -> Room
newRoom name timer maxUsers finTimer pass maxRounds wordList words =
  Room {score = M.empty
       ,name = name
       ,presentator = 0
       ,word = ""
       ,timerLength = timer
       ,newWordSet = realToFrac 0
       ,guessedRight = S.empty
       ,hasGuessedRight = False
       ,guessedRightAt = realToFrac 0
       ,maxUsers = maxUsers
       ,finTimerLength = finTimer
       ,password = pass
       ,maxRounds = maxRounds
       ,round = 1
       ,hasDrawn = S.empty
       ,gameEnded = False
       ,hintsGiven = 0
       ,wordList = wordList
       ,words = words
       ,nextPresentator = 0}

hint :: Room -> Maybe (String,Room)
hint rm | hints >= 3 || hints == length (word rm) = Nothing
        | otherwise = Just $ (take hints (word rm) ++ concat ["_ " | x <- [hints..(length (word rm) - 1)]]
                       ,rm{hintsGiven = hints+1,score = M.adjust ((+)1) (presentator rm) (score rm)})
        where
          hints = hintsGiven rm

setNewWord :: Room -> Bool -> POSIXTime -> Maybe Room
setNewWord rm force now
  | force || correctGuessers == visitors-1 || (gameEnded rm) || now-(since-2) > realToFrac tLength = Just newRoom
  | otherwise = Nothing
  where
    correctGuessers = S.size $ guessedRight rm
    visitors = M.size $ score rm
    since = if hasGuessedRight rm then guessedRightAt rm else newWordSet rm
    tLength = if hasGuessedRight rm then finTimerLength rm else timerLength rm
    newWord = (words rm) !! fst (randomR (0,length (words rm) - 1) (getGenFromTime now))
    newRoom = rm{gameEnded = False, word = newWord, newWordSet = now
                ,guessedRight = S.empty,hasGuessedRight = False}

data PresentatorResponse = Normal | GameEnd | RoundChange
  deriving (Show,Eq)

setNewPresentator :: Room -> Bool -> POSIXTime -> Maybe (PresentatorResponse,Room)
setNewPresentator rm force now
  | noWord = Nothing
  | isJust user = Just (Normal, rm'{presentator = fromJust user
                                   ,hasDrawn = S.insert (fromJust user) (hasDrawn rm')
                                   ,nextPresentator = np+1})
  | round rm == maxRounds rm = Just (GameEnd, rm'{round = 1
                                                 ,hasDrawn = S.empty
                                                 ,gameEnded = True
                                                 ,score = M.map (const 0) (score rm')
                                                 ,nextPresentator = newPresI})
  | otherwise = Just $ (RoundChange, rm'{round = round rm'+1
                                        ,hasDrawn = S.insert newPres S.empty
                                        ,presentator = newPres
                                        ,nextPresentator = newPresI + 1})
  where
    mrm = setNewWord rm force now
    rm' = fromJust mrm
    noWord = isNothing $ mrm
    nextPresI = nextPresentator rm' `mod` M.size (score rm')
    nextPres = M.keys (score rm') !! nextPresI
    (np,user) = if S.member nextPres (hasDrawn rm')
                  then findUser 0 $ M.keys $ score rm'
                  else (nextPresI,Just nextPres)
    findUser i (u:us) = if S.member u (hasDrawn rm') then findUser (i+1) us else (i,Just u) 
    findUser i [] = (i,Nothing)
    newPresI = fst (randomR (0,M.size (score rm')-1) (getGenFromTime now))
    newPres = M.keys (score rm') !! newPresI

data WelcomeResponse = FullRoom | WrongPass | PresentatorChange Room | Norm Room 

welcome :: Int -> Maybe String -> Room -> WelcomeResponse
welcome uid pass rm
  | pass /= (password rm) = WrongPass
  | numVstrs >= (maxUsers rm) = FullRoom
  | numVstrs == 1 = PresentatorChange rm{nextPresentator = 0
                                        ,score = M.insert uid 0 (score rm)
                                        ,hasDrawn = S.empty}
  | otherwise = Norm rm{score = M.insert uid 0 (score rm)}
  where
    numVstrs = M.size $ score rm

data LeftResponse = NoChange | NewPresentator | DeleteRoom

kickOut :: Int -> Room -> (LeftResponse,Room)
kickOut uid rm
  | mmbrs == 0 = (DeleteRoom,rm')
  | uid == (presentator rm') && mmbrs == 1 = (NoChange,rm'{hasDrawn = S.empty})
  | uid == (presentator rm') = (NewPresentator,rm')
  | otherwise = (NoChange,rm')
  where
    rm' = rm{score = M.delete uid $ score rm}
    mmbrs = M.size $ score rm'

data GuessResponse = Wrong | Almost | Correct (Int,Room)

guess :: String -> Int -> POSIXTime -> Room -> GuessResponse
guess wrd uid now rm
  | now-since > tLength = Wrong
  | word rm == wrd = Correct (uscre,rm{score = M.adjust ((+)presscre) (presentator rm) $ M.insert uid uscre $ score rm
                                   ,guessedRight = S.insert uid $ guessedRight rm})
  | almostCorrect = Almost
  | otherwise = Wrong
  where
    since = if hasGuessedRight rm then guessedRightAt rm else newWordSet rm
    tLength = realToFrac (if hasGuessedRight rm then finTimerLength rm else timerLength rm)
    presscre = if S.size (guessedRight rm) == 0 then 4 else 2
    uscre = M.size (score rm) - S.size (guessedRight rm) + score rm M.! uid
    almostCorrect = seqAlign wrd (word rm) <= length wrd `div` 3

seqAlign :: String -> String -> Int
seqAlign [] bs = length bs * gapPenalty
seqAlign as [] = length as * gapPenalty
seqAlign (a:as) (b:bs) = minimum [matchPenalty a b + seqAlign as bs, gapPenalty + seqAlign (a:as) bs, gapPenalty + seqAlign as (b:bs)]

gapPenalty = 2

vowels = "aeoiyåäö"

matchPenalty :: Char -> Char -> Int
matchPenalty a b | a==b = sameCharPenalty
           | (a `elem` vowels) == (b `elem` vowels) = sameTypePenalty
           | otherwise = differentTypePenalty
  where
    sameCharPenalty = 0
    differentTypePenalty = 2
    sameTypePenalty = 1

-- | Gets a generator based on the microsecond of the timestamp
getGenFromTime :: POSIXTime -> StdGen
getGenFromTime = mkStdGen.P.round.((*)1000000).snd.properFraction
