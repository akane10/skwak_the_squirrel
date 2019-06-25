import qualified Data.List as L

data GameObject = Player | Acorn deriving (Eq, Show)

data Room = Room Description [GameObject] deriving (Show)

type Description = String
type Inventory = [GameObject]
type GameMap = [Room]
type GameState = (GameMap, Inventory)

initialState :: GameState
initialState = ([
                  Room "you are inside the tree" [Player]
                 ,Room "you are outside the tree" [Acorn]
                ] 
               , [])

main :: IO()
main = do
  putStrLn "Welcome to Skwak the Squirrel."
  putStrLn "you are a squirrel"
  gameLoop initialState

gameLoop :: GameState -> IO()
gameLoop (rooms, currentInv) = do
  let currentRoom = case findRoomWithPlayer rooms of
                    Just r -> r
                    Nothing -> error "somehow the player ended up outside the map"
      possibleCmds = validCommands currentRoom currentInv
  if playerWon (rooms, currentInv)
  then gameOverRestart
  else do
    describeWorld currentRoom currentInv possibleCmds
    takeActionAndThenLoop currentRoom currentInv possibleCmds rooms

findRoomWithPlayer :: [Room] -> Maybe Room
findRoomWithPlayer rooms = L.find (\(Room _ obs) -> any (== Player) obs) rooms

validCommands :: Room -> Inventory -> [String]
validCommands (Room _ gameObjs) invItems = ["go"] ++ takeCommandList ++ dropCommandList ++ ["quit"]
  where takeCommandList = if somethingToTake gameObjs
                          then ["take"]
                          else []
        dropCommandList = if length invItems > 0
                          then ["put"]
                          else []

somethingToTake :: [GameObject] -> Bool
somethingToTake = any (/= Player)

playerWon :: GameState -> Bool
playerWon (rooms, currentInv) = 
  any hasAcornAndInside rooms
  where hasAcornAndInside (Room desc objs) = desc == "you are inside the tree" && any (==Acorn) objs

gameOverRestart :: IO()
gameOverRestart = do 
  putStrLn "you won! you have successfully stored the acorn for winter, well done!"
  putStrLn "do you want to play again? y = yes"
  playAgain <- getLine
  if playAgain == "y"
  then gameLoop initialState
  else putStrLn "thanks for playing"

getCommand :: IO String
getCommand = do
  putStrLn "what do you want to do?"
  getLine

takeActionAndThenLoop :: Room -> Inventory -> [String] -> [Room] -> IO()
takeActionAndThenLoop currentRoom currentInv possibleCmds rooms = do
  command <- getCommand
  if any (== command) possibleCmds
  then case command of
         "go" -> do
           putStrLn "you go..."
           gameLoop $ movePlayer (rooms, currentInv)
         "take" -> do
           putStrLn "you take the acorn..."
           gameLoop $ moveAcornToInventory (rooms, currentInv)
         "put" -> do
           putStrLn "you put the acorn down..."
           gameLoop $ moveAcornFromInventory (rooms, currentInv)
         "quit" -> do
           putStrLn "you decide to give up. thanks for playing"
         _ -> do
           putStrLn "that is not a command"
           gameLoop (rooms, currentInv)
  else do
    putStrLn "command not possible here or that is not a command"
    gameLoop (rooms, currentInv)

movePlayer :: GameState -> GameState
movePlayer (rooms, inv) = (newRooms, inv)
                          where newRooms = map adjustRooms rooms
                                adjustRooms (Room d objs) = if any (== Player) objs
                                                            then (Room d (filter (/= Player) objs))
                                                            else (Room d (Player : objs))

moveAcornToInventory :: GameState -> GameState
moveAcornToInventory (rooms, inv) = (newRooms, newInv)
                                    where newInv = Acorn : inv
                                          newRooms = map adjustRooms rooms
                                          adjustRooms (Room d objs) = Room d (filter (/= Acorn) objs)

moveAcornFromInventory :: GameState -> GameState
moveAcornFromInventory (rooms, inv) = (newRooms, newInv)
  where newInv = filter (/=Acorn) inv
        newRooms = map adjustRooms rooms
        adjustRooms (Room d objs) = if any (== Player) objs
                                    then Room d (Acorn : objs)
                                    else Room d objs

describeWorld :: Room -> Inventory -> [String] -> IO()
describeWorld currentRoom currentInv possibleCmds = do
  putStrLn $ describeRoom currentRoom
  putStrLn $ describeInventory currentInv
  putStrLn $ describeCommands possibleCmds

describeRoom :: Room -> String
describeRoom (Room desc objs) = desc ++ if any (==Acorn) objs
                                        then " there is an acorn here"
                                        else ""

describeInventory :: Inventory -> String
describeInventory [] = "you are holding nothing"
describeInventory inv = "you are holding: " ++ (concat $ map show inv)

describeCommands :: [String] -> String
describeCommands commands = "Commands: " ++ (L.intercalate ", " commands)

