import Data.Functor
import Data.Random.Extras (shuffle)
import Data.Random.Source.DevRandom
--import Data.Random.Source (RandomSource)
--import Data.Random.Internal
import Data.Random.RVar (runRVar)

shuffleAndDraw :: Int -> [a] -> IO [a]
shuffleAndDraw n deck = runRVar ((take n) <$>  shuffle deck) DevURandom

main :: IO b
main = do
  hand <- shuffleAndDraw 3 ["poke", "bite", "slap", "trip", "claw", "punch", "kick", "taunt"]
  print hand
  getLine
  main
