import Control.Monad
import Data.Functor
import Data.Random.Extras (shuffle)
import Data.Random.Source.DevRandom
import Data.Random.RVar (runRVar)

data GameState = GameState {playerHp :: Int, monsterHp :: Int};

type Damage = Int
data Action = Action {
  verb::String,
  damage::Damage}
instance Show Action where
  show action = verb action


type ActionDeck = [Action]

shuffleAndDraw :: Int -> [Action] -> IO [Action]
shuffleAndDraw n deck = runRVar ((take n) <$>  shuffle deck) DevURandom

playerDeck :: ActionDeck
playerDeck = [Action "poke" 1, Action "bite" 1, Action "slap" 1, Action "trip" 1,
              Action "claw" 1, Action "punch" 2, Action "kick" 3, Action "taunt" 0]

monsterDeck :: ActionDeck
monsterDeck = [Action "gurgle" 0, Action "bite" 2, Action "pounce" 2,
               Action "paw" 1, Action "howl" 0, Action "whimper" 0 ]

main :: IO ()
main = do
  putStrLn "Welcome, adventurer."
  putStrLn "You have enountered a foul beast!"
  playGame $ GameState 5 5

playGame :: GameState -> IO ()
playGame initialState = do
  getLine
  afterMonsterGoes <- monsterTurn initialState
  when (playerHp initialState > playerHp afterMonsterGoes) $
    printPlayerStatus afterMonsterGoes
  if (playerHp afterMonsterGoes <= 0) then
    putStrLn "You are dead and soon forgotten."
    else do
      newState <- playerTurn afterMonsterGoes
      if (monsterHp newState <= 0)
        then putStrLn "You have defeated the monster!  Good bye, mighty hero"
        else playGame newState

monsterTurn :: GameState -> IO(GameState)
monsterTurn (GameState php mhp) = do
  monsterAction <- head <$> shuffleAndDraw 1 monsterDeck
  putStrLn $ "It " ++ show monsterAction ++ "s!"
  let newPhp = php - damage monsterAction
  return (GameState newPhp mhp)

playerTurn :: GameState -> IO(GameState)
playerTurn (GameState php mhp) = do
  playerAction <- head <$> shuffleAndDraw 1 playerDeck
  putStrLn $ "You " ++ (show playerAction) ++ " the monster."
  let newMhp = mhp - damage playerAction
  putStrLn $ "The monster has " ++ show newMhp ++ " hitpoints left."
  return (GameState php newMhp)

printPlayerStatus :: GameState -> IO()
printPlayerStatus state = do
  putStrLn $ "You have " ++ show hp ++ " hitpoints left"
  where hp = playerHp state
