import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor
import qualified Data.Foldable as Fld ( mapM_ )
import qualified Data.Traversable as Trv ( mapM )
import Data.Sequence as Sq (Seq, fromList, index, update, null, filter, length)
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
                             ioAction :: IO Action }

player :: Character
player = Character { name="Player",
                     hpCurrently=5,
                     hpTotal=5,
                     ioAction=(getLine>>randomAction playerDeck)}

monster :: Character
monster = Character { name="MonsterBeast",
                      hpCurrently=5,
                      hpTotal=5,
                      ioAction=randomAction monsterDeck}

data Game = Game { characters :: Seq Character, turn :: Int }

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
  playGame $ Game {characters= fromList [monster, player], turn=0}

playGame :: Game -> IO ()
playGame startOfTurn = do
  tellStartTurn startOfTurn
  afterThisTurn <- doTurn startOfTurn
  tellTurnEndResults startOfTurn afterThisTurn
  (gameIsOver,reasons) <- runWriterT $ checkGameOverRules afterThisTurn
  if gameIsOver
    then mapM_ (putStrLn) reasons
    else playGame (nextTurn afterThisTurn)

checkGameOverRules :: Game -> WriterT [String] IO Bool
checkGameOverRules game = do
  Trv.mapM (obituate) deadCharacters
  return $ not $ Sq.null deadCharacters
  where
    characterIsDead character = hpCurrently character <= 0
    deadCharacters = Sq.filter characterIsDead (characters game)

obituate :: Character -> WriterT [String] IO ()
obituate character = do
  tell $ [  name character ++ " has died." ]

nextTurn :: Game -> Game
nextTurn game = game { turn=mod (1 + turn game) (Sq.length $ characters game)}

getTurnTaker :: Game -> Character
getTurnTaker game = Sq.index (characters game) (turn game)

tellStartTurn :: Game -> IO()
tellStartTurn game = do
  let character = getTurnTaker game
  putStrLn $ "Start of " ++ name character ++ "'s turn"

tellTurnEndResults :: Game -> Game -> IO ()
tellTurnEndResults start end = do
  let character = getTurnTaker start
  putStrLn $ (name character) ++ "'s turn ends";
  Fld.mapM_ (tellHpRemaining) (characters end)
  putStrLn ""
  return ()

tellHpRemaining :: Character -> IO ()
tellHpRemaining x = do
  putStrLn $ name x ++ ": " ++ (show . hpCurrently) x ++ " remaining."

doTurn :: Game -> IO Game
doTurn startOfTurn = do
  let
    pos = turn startOfTurn
    cs = characters startOfTurn
    character = index cs  pos
    targetPos = mod (1+pos) (Sq.length cs)
    target = index cs targetPos
  action <- ioAction character
  putStrLn $ name character
    ++ " " ++ verb action ++ "s at " ++ name target
    ++ " for " ++ (show . damage) action ++ " damage.";
  return startOfTurn {characters = update targetPos
                                   (applyDamage (damage action) target) cs }

applyDamage :: Int->Character->Character
applyDamage dmg character =
  character { hpCurrently = hpCurrently character - dmg }
