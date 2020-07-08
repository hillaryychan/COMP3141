module TicTacToe where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.State
import System.Random (randomRIO) -- package 'random'
import Data.List
import Data.Maybe

-- The state of the game
data Game = Game
  { board :: [Tile]
  , player :: Tile
  }
  deriving (Show)

instance Arbitrary Game where
  arbitrary =
    do
      { player <- arbitrary
      ; board <- vector 9
      ; pure $ Game board player
      }

-- Tiles on the board
data Tile
  = Neither
  | Naught
  | Cross
  deriving (Eq)

instance Show Tile where
  show Neither = "•"
  show Naught  = "○"
  show Cross   = "✕"

instance Arbitrary Tile where
  arbitrary = do
    { choice <- arbitrary :: Gen Int
    ; pure $ case choice `mod` 3 of
        0 -> Neither
        1 -> Naught
        2 -> Cross
    }

-- A position on the board
type Position = (Int, Int)

-- A player of the game
--
-- A player may perform IO operations to determine their move
type Player = Game -> IO Position

-- A game of TicTacToe
ticTacToe :: Player -> Player -> IO ()
ticTacToe naughts crosses = gameLoop newGame
  where
    newGame :: Game
    newGame = Game (replicate 9 Neither) Naught

    gameLoop :: Game -> IO ()
    gameLoop game =
      do
        { showBoard game
          -- get possible moves from game
        ; if null $ possibleMoves $ board game
            then putStrLn "No more moves"
            else case winner (board game) of
              Neither -> playerMove game
              winner  -> putStrLn $ "Winner: " ++ show winner
        }
      where
        playerMove game = do
          -- get current player and get player move
          { (x, y) <- case player game of
              Naught -> naughts game
              Cross -> crosses game
          ; let
              isValid = (x, y) `elem` possibleMoves (board game)
              nextPlayer = case player game of
                Naught -> Cross
                Cross -> Naught
          ; if isValid
              then do
                { let position = (y * 3) + x
                ; let nextBoard = insert position game
                ; gameLoop $ Game nextBoard nextPlayer
                }
              else putStrLn $ "Invalid move " ++ show (x, y)
          }

        insert :: Int -> Game -> [Tile]
        insert position (Game board player)
          =  take position board
          ++ [player]
          ++ drop (position + 1) board

-- Show the game board
showBoard :: Game -> IO ()
showBoard (Game board player) =
  do
    { boardLoop 0 board
    ; putStrLn "\n"
    ; putStrLn $ "The current player is " ++ show player
    ; putChar '\n'
    }
  where
    boardLoop :: Int -> [Tile] -> IO ()
    boardLoop _ []     = pure ()
    boardLoop 3 ts     = do
      { putStrLn "\n-+-+-"
      ; boardLoop 0 ts
      }
    boardLoop x (t:ts) = do
      { if x /= 0 then putChar '|' else pure ()
      ; putStr (show t)
      ; boardLoop (x + 1) ts
      }

-- Determine the current winner
winner :: [Tile] -> Tile
winner = diagonals `or` rows `or` columns
  where
    diagonals :: [Tile] -> Tile
    diagonals ts
      | ts !! 0 == ts !! 4 && ts !! 4 == ts !! 8 = ts !! 4
      | ts !! 2 == ts !! 4 && ts !! 4 == ts !! 6 = ts !! 4
      | otherwise                                = Neither

    rows :: [Tile] -> Tile
    rows = row 0 `or` row 3 `or` row 6

    row :: Int -> [Tile] -> Tile
    row n ts
      | ts !! n == ts !! (n+1) && ts !! (n+1) == ts !! (n+2) = ts !! n
      | otherwise                                            = Neither

    columns :: [Tile] -> Tile
    columns = column 0 `or` column 1 `or` column 2

    column :: Int -> [Tile] -> Tile
    column n ts
      | ts !! n == ts !! (n+3) && ts !! (n+3) == ts !! (n+6) = ts !! n
      | otherwise                                            = Neither

    or :: ([Tile] -> Tile) -> ([Tile] -> Tile) -> [Tile] -> Tile
    or left right tiles =
      case left tiles of
        Neither -> right tiles
        winner  -> winner

-- Get the list of all available moves
possibleMoves :: [Tile] -> [Position]
possibleMoves = go (0, 0)
  where
    go (_, _) []     = []
    go (x, y) (t:ts) =
      let
        nextPos = if x == 2 then (0, y + 1) else (x + 1, y)
        rest = go nextPos ts
      in case t of
        Neither -> (x, y) : rest
        _       -> rest

-- An interactive player
interactive :: Player
interactive _ = do
  { putStr "What is your next move? "
  ; readLn
  }

-- An A.I. that will make random moves
randomAi :: Player
randomAi (Game board player) = do
  { let moves = possibleMoves board
  ; choice <- randInt (0, length moves)
  ; pure $ moves !! choice
  }

-- An A.I. that simulates games to determine the best move
--
-- We want the first move in the shortest sequence of moves that will
-- let us win.
simulationAi :: Player
simulationAi game =
  do
    { let best = findBest $ values game
    ; case best of
        Just move -> pure move
        -- Fall back to random if no good moves remain
        Nothing   -> randomAi game
    }
  where
    values :: Game -> [(Int, Position)]
    values = take 100 .  mapMaybe checkSim . possibleSims

    findBest :: [(Int, Position)] -> Maybe Position
    findBest []     = Nothing
    findBest (m:ms) = Just $ rec m ms
      where
        rec (v, m) [] = m
        rec (v, m) ((v', m'):ms)
          | v' < v    = rec (v', m') ms
          | otherwise = rec (v , m ) ms

    checkSim :: Simulation -> Maybe (Int, Position)
    checkSim sim = case simulationValue (player $ simGame sim) sim of
      Nothing    -> Nothing
      Just value -> Just (value, head $ simMoves sim)

data Simulation = Simulation
  { simGame :: Game -- Current board and player state
  , simMoves :: [Position] -- Sequence of remaining moves
  , simCount :: Int -- Number of moves played
  }

-- Generate the list of simulations for a given game
possibleSims :: Game -> [Simulation]
possibleSims game = map ((flip $ Simulation game) 0) $ permutations moves
  where
    moves = possibleMoves $ board game

-- Get the 'value' of a simulation, the number of moves for a given
-- player to win
simulationValue :: Tile -> Simulation -> Maybe Int
simulationValue player = evalState go
  where
    go :: State Simulation (Maybe Int)
    go = do
      { position <- nextMove
      ; game <- gets simGame
      ; case position of
          Just (x, y) -> do
            { let nextBoard = insert (x + y * 3) game
            ; if winner nextBoard == player
                then do
                  { count <- gets simCount
                  ; pure $ Just count
                  }
                else if winner nextBoard == Neither
                  then do
                    { modify (endTurn nextBoard)
                    ; go
                    }
                  else pure Nothing
            }
          Nothing -> pure Nothing
      }

    nextMove :: State Simulation (Maybe Position)
    nextMove = do
      { (Simulation game moves count) <- get
      ; case moves of
        (m:ms) -> do
          { put (Simulation game ms count)
          ; pure $ Just m
          }
        [] -> pure Nothing
      }

    insert :: Int -> Game -> [Tile]
    insert position (Game board player)
      =  take position board
      ++ [player]
      ++ drop (position + 1) board

    endTurn :: [Tile] -> Simulation -> Simulation
    endTurn newBoard (Simulation (Game _ player) moves c)
      | player == Naught = Simulation
        { simGame  = Game newBoard Cross
        , simMoves = moves
        , simCount = c + 1
        }
      | player == Cross  = Simulation
        { simGame  = Game newBoard Naught
        , simMoves = moves
        , simCount = c + 1
        }

-- An A.I. should never make an invalid move
prop_onlyValid :: Player -> Game -> PropertyM IO ()
prop_onlyValid player game = do
  { let moves = possibleMoves $ board game
  ; pre $ not $ null moves
  ; move <- run $ player game
  ; assert $ move `elem` moves
  }

-- Generate random integers in a range [min, max)
randInt :: (Int, Int) -> IO Int
randInt (min, max) = randomRIO (min, max - 1)
