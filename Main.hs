import Data.Functor
import Data.Random.Extras (shuffle)
import Data.Random.Source.DevRandom
import Data.Random.RVar (runRVar)

data GameState = EnemyHitpoints Int

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
  print "Welcome, adventurer."
  print "You have enountered a foul beast!"
  playGame $ EnemyHitpoints 5

playGame :: GameState -> IO ()
playGame (EnemyHitpoints enemyHitpoints) = do
  monsterAction <- head <$> shuffleAndDraw 1 monsterDeck
  print $ "It " ++ show monsterAction ++ "s!"
  playerHand <- shuffleAndDraw 1 playerDeck
  print playerHand
  print $ "The monster has " ++ show enemyHitpoints ++ " hitpoints left"
  getLine
  if (enemyHitpoints > 0)
    then playGame $ EnemyHitpoints (enemyHitpoints - damage (head playerHand));
    else print "You have defeated the monster!  Good bye, mighty hero"
