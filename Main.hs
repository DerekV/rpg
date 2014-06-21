import Data.Functor
import Data.Random.Extras (shuffle)
import Data.Random.Source.DevRandom
import Data.Random.RVar (runRVar)

data GameState = EnemyHitpoints Int

shuffleAndDraw :: Int -> [a] -> IO [a]
shuffleAndDraw n deck = runRVar ((take n) <$>  shuffle deck) DevURandom

main :: IO ()
main = do
  playGame $ EnemyHitpoints 5

playGame :: GameState -> IO ()
playGame (EnemyHitpoints enemyHitpoints) = do
  hand <- shuffleAndDraw 1 ["poke", "bite", "slap", "trip", "claw", "punch", "kick", "taunt"]
  print hand
  print $ "The monster has " ++ show enemyHitpoints ++ " hitpoints left"
  getLine
  if (enemyHitpoints > 0) then do
    playGame $ EnemyHitpoints (enemyHitpoints - 1);
    else
    print "You have defeated the monster!  Good bye, mighty hero"
