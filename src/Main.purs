module Main where

import Data.Maybe
import Data.Tuple
import Prelude

import Data.Array (foldM,foldMap,foldl,zipWith, reverse
                  , concat, (!!), (..), updateAt, alterAt, replicate)
import Graphics.Canvas
import Data.Traversable (sequence,for)
import Effect (Effect)
import Effect.Console (logShow, log)
import Math (sqrt)
import Data.Int (toNumber)
import Effect.Timer (setInterval)
import Effect.Ref
import Gui


boardWidth = 12  :: Int  -- only 10 can use    x0000x  -->  clow
boardHeight = 21 :: Int  -- only 20 can user   x0000x  -->  row
                         --                    xxxxxx


type Board =  Array (Array Boolean)
type Square = Array (Array Boolean)
type MoveSquare =
  { position :: Tuple Int Int
  , square :: Square
  }

type GameState =
  { board :: Board
  , moveSquare :: MoveSquare
  }

initGameState = {
     board: fromMaybe [] $ initBoard
   , moveSquare: bMoveSquare
                }

initBoard' :: Int -> Int -> Board
initBoard' rows clows = do
  i <- 1 .. rows
  [replicate clows false]

initBoard :: Maybe Board
initBoard =  foldM (updateBoardAt true) baseBoard (changeListA <> changeListB)
  where baseBoard = initBoard' boardHeight boardWidth

changeListA :: Array (Tuple Int Int)
changeListA = do
  i <- 0 .. (boardHeight -1 )
  j <- [0,(boardWidth -1)]
  pure $ Tuple i j

changeListB :: Array (Tuple Int Int)
changeListB = do
  j <- 0 .. (boardWidth -1)
  pure $ Tuple (boardHeight -1 ) j


updateBoardAt :: Boolean  -> Board -> Tuple Int Int -> Maybe Board
updateBoardAt bool b (Tuple r c)  = alterAt r t b
   where t a = updateAt c bool a

updateBoardAt' tuple bool board  = updateBoardAt bool board tuple

readBoardAt :: Tuple Int Int -> Board -> Maybe Boolean
readBoardAt (Tuple r c) board = do
  a <- board !! r
  b <- a !! c
  pure b

readBoardList :: Array (Tuple Int Int) -> Board -> Maybe (Array Boolean)
readBoardList list board =sequence $ map (\t -> readBoardAt t board ) list



lSquare :: Square
lSquare = [
  [true,false,false,false],
  [true,false,false,false],
  [true,false,false,false],
  [true,false,false,false] ]


zSquare :: Square
zSquare = [
  [false,true,false,false],
  [false,true,true,false],
  [false,false,true,false],
  [false,false,false,false] ]


bSquare :: Square
bSquare = [
  [false,false,false,false],
  [false,true,true,false],
  [false,true,true,false],
  [false,false,false,false] ]

-- 只需要确定是否碰撞，不需要告诉我具体在哪里碰撞
extendSquare :: Square -> Array Boolean
extendSquare square =concat $ reverse square

isAre :: Tuple Int Int -> Boolean
isAre (Tuple r c) = r > -1  && r < (boardHeight )
                 && c > -1  && c < (boardWidth )


extendTuple :: Tuple Int Int -> Array (Tuple Int Int)
extendTuple (Tuple r c) = do
  i <- r .. (r -3)
  j <- c .. (c +3)
  pure $ Tuple i j

t1 = extendTuple (Tuple 0 20)

sliceExtendBoard :: Tuple Int Int -> Board -> Array Boolean
sliceExtendBoard t0@(Tuple r c) board = map tread $ extendTuple t0
   where tread :: Tuple Int Int ->  Boolean
         tread t@(Tuple tr tc) = if isAre t
                                 then fromMaybe false $ readBoardAt t board
                                 else false

checkCollision :: Board -> MoveSquare -> Boolean
checkCollision board moveSquare =
  foldl (||) false $
  zipWith (&&) (sliceExtendBoard moveSquare.position board)
  (extendSquare moveSquare.square)

bMoveSquare = {position: (Tuple 9 5) , square: zSquare}


moveGameState' :: (Tuple Int Int -> Tuple Int Int ) -> GameState -> GameState
moveGameState' f gameState =
  let newPositon =f gameState.moveSquare.position
  in { board: gameState.board
     , moveSquare: { position: newPositon
                   , square: gameState.moveSquare.square
                   }
     }

moveGameState :: (Tuple Int Int -> Tuple Int Int ) -> GameState -> Maybe GameState
moveGameState f gameState =
  let newGameState = moveGameState' f gameState
      --- 这搞错了 newGameState 和 gameState ... 找了好久问题
  in if checkCollision newGameState.board newGameState.moveSquare
     then Nothing
     else Just newGameState

writeBoardAt :: MoveSquare ->  Board -> Tuple Int Int -> Board
writeBoardAt moveSquare board t@(Tuple r c) =
  let Tuple pr pc = moveSquare.position
      ntuple = Tuple (pr+r-3) (pc+c)
  in if isAre ntuple
  then let bv = fromMaybe false $ readBoardAt ntuple board
           bs = fromMaybe false $ readBoardAt t moveSquare.square
       in fromMaybe board $ updateBoardAt (bv || bs) board ntuple
  else board

mergeBoardAndMoveSquare :: Board -> MoveSquare -> Board
mergeBoardAndMoveSquare board moveSquare =
  foldl (writeBoardAt moveSquare) board allSquare


moveRight :: Tuple Int Int -> Tuple Int Int
moveRight (Tuple r c) = (Tuple r (c+1))

moveLeft :: Tuple Int Int -> Tuple Int Int
moveLeft (Tuple r c) = (Tuple r (c-1))


moveDown :: Tuple Int Int -> Tuple Int Int
moveDown (Tuple r c) = (Tuple (r+1) c)



allBoard :: Array (Tuple Int Int)
allBoard = do
  i <- 0 .. 20
  j <- 0 .. 11
  pure $ Tuple i j

drawARect' :: Context2D -> Board -> Tuple Int Int -> Effect Unit
drawARect' ctx board tuple = do
  let r = fromMaybe false $ readBoardAt tuple board
  drawARect ctx tuple r

drawBoard :: Context2D -> Board -> Effect Unit
drawBoard ctx b =  foldMap (drawARect' ctx b) allBoard


drawMoveSquare' :: Context2D -> MoveSquare -> Tuple Int Int -> Effect Unit
drawMoveSquare' ctx moveSquare tuple@(Tuple r c) = do
    let bool = fromMaybe false $ readBoardAt tuple moveSquare.square
        Tuple a b = moveSquare.position
    drawARect ctx (Tuple (a+r-3) (b+c)) bool

allSquare :: Array (Tuple Int Int)
allSquare = do
  i <- 0 .. 3
  j <- 0 .. 3
  pure $ Tuple i j



drawMoveSquare :: Context2D -> MoveSquare -> Effect Unit
drawMoveSquare ctx moveSquare = foldMap (drawMoveSquare' ctx moveSquare) allSquare

drawARect ::Context2D -> Tuple Int Int -> Boolean -> Effect Unit
drawARect ctx (Tuple r c) b = do
  if b
    then do
    setFillStyle ctx "#0000FF"
    fillRect ctx {height:25.0,width:25.0,x:toNumber c * 25.0,y: toNumber r * 25.0 + 100.0}
    else do
    setFillStyle ctx "#00FF00"
    fillRect ctx {height:25.0,width:25.0,x:toNumber c * 25.0,y: toNumber r * 25.0 + 100.0}
  setFillStyle ctx "#00FF00"



render :: Context2D -> (Ref GameState) -> Effect Unit
render ctx ref = do
  gameState <- read ref
  clearRect ctx {height:1200.0,width:800.0,x:0.0,y:0.0}
  drawBoard ctx gameState.board
  drawMoveSquare ctx gameState.moveSquare
  log "in render circle"


dealEvent :: (Ref GameState) -> Effect Unit
dealEvent ref = do
  gameState <- read ref
  case moveGameState moveDown gameState of
    --如何去修改递归记录的内部结构??
    Nothing -> write { board: mergeBoardAndMoveSquare gameState.board gameState.moveSquare
                     , moveSquare: {position: Tuple 0 4, square: zSquare}
                     } ref
    Just newGameState -> write newGameState ref
  -- write (fromMaybe gameState $ moveGameState moveDown gameState) ref
  log "in deal event circle"


work :: Context2D -> (Ref GameState) -> Effect Unit
work ctx ref = do
  dealEvent ref
  render ctx ref

main :: Effect Unit
main = do
  canvas <- getCanvasElementById "canvas"
  ref <- new initGameState
  case canvas of
    Nothing -> do
      log "nothing"
    Just canvas -> do
      ctx <- getContext2D canvas
      setFillStyle ctx "#0000FF"
      t <- setInterval 500 $ work ctx ref
      log "find context2d"
  log "Hello sailor!"
  log "nice"









