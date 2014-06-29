import Control.Monad
import Control.Monad.Writer
import Data.Functor
import qualified Data.Foldable as Fld ( mapM_ )
import qualified Data.Traversable as Trv ( mapM )
import Data.Random.Extras (shuffle)
import Data.Random.Source.DevRandom
import Data.Random.RVar (runRVar)

type Damage = Int
data Action = Action {
  verb::String,
  damage::Damage}
instance Show Action where
  show = verb


shuffleAndDraw :: Int -> [Action] -> IO [Action]
shuffleAndDraw n deck = runRVar ((take n) <$>  shuffle deck) DevURandom

randomAction :: [Action] -> IO Action
randomAction as = head <$> shuffleAndDraw 1 as

data Character = Character { name :: String,
                             hpCurrently :: Int,
                             hpTotal :: Int,
                             ioAction :: ActionDeck -> IO Action,
                             standardDeck :: ActionDeck}

instance Eq Character where
  lhs == rhs = name lhs == name rhs

player :: Character
player = Character { name="Player",
                     hpCurrently=5,
                     hpTotal=5,
                     ioAction=(\options -> (getLine>>randomAction options)),
                     standardDeck=playerDeck}

monster :: Character
monster = Character { name="MonsterBeast",
                      hpCurrently=5,
                      hpTotal=5,
                      ioAction=(\options -> randomAction options),
                      standardDeck=monsterDeck}

type GameEndReason = String

data ActionTaken = ActionTaken
                   { subject :: Character, object :: Character, action :: Action }

data Event = ActionEvent ActionTaken
           | GameEndEvent GameEndReason

instance Show Event where
  show (ActionEvent ActionTaken {subject=attacker,action=attack,object=target}) =
    name attacker ++ " " ++ show attack ++ "s at " ++ name target
    ++ " for " ++ show (damage attack) ++ " damage."
  show (GameEndEvent reason) = "Game end condition met : " ++ reason;

type Choice = ( Character , ActionDeck )

data Game = Game { characters :: [Character], waitingFor :: Choice }

type ActionDeck = [Action]

playerDeck :: ActionDeck
playerDeck = [Action "poke" 1, Action "stab" 3, Action "slap" 1,
              Action "trip" 1, Action "claw" 1, Action "punch" 2,
              Action "kick" 3, Action "taunt" 0]

monsterDeck :: ActionDeck
monsterDeck = [Action "gurgle" 0, Action "bite" 2, Action "pounce" 2,
               Action "paw" 1, Action "howl" 0, Action "whimper" 0 ]

main :: IO ()
main = do
  putStrLn "Welcome, adventurer."
  putStrLn "You have enountered a foul beast!"
  playGame $ Game {characters = [monster, player],
                   waitingFor = (monster, monsterDeck)}

playGame :: Game -> IO ()
playGame startOfTurn = do
  showStats startOfTurn
  actionTaken <- getAction startOfTurn
  let (afterThisTurn,events) = resolveNextChoice startOfTurn actionTaken
  narrateEvents startOfTurn afterThisTurn events
  let gameOverEvents =filter isGameOverEvent events
  if not . null $ gameOverEvents
    then mapM_ (putStrLn . show) gameOverEvents
    else playGame afterThisTurn

getAction :: Game -> IO (ActionTaken)
getAction game = do
  let
    character = fst $ waitingFor game
    letCharacterChoose = ioAction character
    actions = snd $ waitingFor game
    target = head $ filter (/= character) $ characters game
  actionChoosen <- letCharacterChoose actions
  return ActionTaken { subject = character,
                       action = actionChoosen,
                       object = target}

isGameOverEvent :: Event -> Bool
isGameOverEvent (GameEndEvent _) = True
isGameOverEvent _ = False

checkGameOverRules :: Game -> [Event]
checkGameOverRules game = map (GameEndEvent . obituate) deadCharacters
  where
    characterIsDead character = hpCurrently character <= 0
    deadCharacters = filter characterIsDead (characters game)

obituate :: Character -> String
obituate character = name character ++ " has died."

showStats :: Game -> IO()
showStats game = do
  Fld.mapM_ (tellHpRemaining) (characters game)
  return ()

narrateEvents :: Game -> Game -> [Event] -> IO ()
narrateEvents start end events = do
  mapM_ (putStrLn . show) events
  putStrLn ""
  return ()

tellHpRemaining :: Character -> IO ()
tellHpRemaining x = do
  putStrLn $ name x ++ ": " ++ (show . hpCurrently) x ++ " remaining."

resolveNextChoice :: Game -> ActionTaken -> (Game, [Event])
resolveNextChoice startOfTurn actionTaken =
  (endOfTurn,events)
  where
    endOfTurn = startOfTurn {
      characters = updateCharacterHps actionTaken (characters startOfTurn),
      waitingFor = nextCharactersTurn startOfTurn }
    events = [ActionEvent actionTaken] ++ checkGameOverRules endOfTurn

nextCharactersTurn :: Game -> Choice
nextCharactersTurn game =
  (playerToGo, standardDeck playerToGo)
  where
    playerWentLast = fst $ waitingFor game
    playerToGo = head $
                 tail (dropWhile (/= playerWentLast) (characters game))
                 ++ characters game

updateCharacterHps :: ActionTaken -> [Character] -> [Character]
updateCharacterHps ActionTaken {action=hit, object=target} chars =
  map updateHp chars
  where
    dmg = damage hit
    updateHp character
      | character == target = applyDamage dmg character
      | otherwise = character

applyDamage :: Int->Character->Character
applyDamage dmg character =
  character { hpCurrently = hpCurrently character - dmg }
