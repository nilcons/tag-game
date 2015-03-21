{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Control.Arrow
import Control.Lens
import Data.AffineSpace
import Data.Default
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.IO.Unsafe
import Control.Monad.Trans.State.Strict

import Keys
import Player
import Target

data World
  = World
    { _wPlayers :: [Player]
    , _wKeys :: !KeySet
    , _wSize :: !(Float,Float)
    , _wFogo :: !Int
    , _wGrace :: !Float
    , _wShowScores :: !Bool
    , _wRandom :: !StdGen
    , _wTarget :: !Target
    } deriving (Show)
makeLenses ''World

iniSize :: (Int,Int)
iniSize = (1200,800)

borderWidth :: Float
borderWidth = 10

graceT :: Float
graceT = 2

iniWorld :: IO World
iniWorld = do
  rnd <- newStdGen
  let world0 =
        World [player0, player1, player2] def (iniSize & both %~ fromIntegral) (-1) 0 True rnd def
  return $ placeTarget world0

player0 :: Player
player0 = def & pColor .~ blue & pPos .~ (300,300) & pAng .~ degToRad 225
              & pName .~ "Blue"

player1 :: Player
player1 = def & pColor .~ red & pPos .~ (-300,-300) & pAng .~ degToRad 45
              & pName .~ "Red"

player2 :: Player
player2 = def & pColor .~ green & pPos .~ (300,-300) & pAng .~ degToRad 135
              & pName .~ "Green"

_player3 :: Player
_player3 = def & pColor .~ yellow & pPos .~ (-300,300) & pAng .~ degToRad (-45)
              & pName .~ "Yellow"

drawWorld :: World -> Picture
drawWorld World{..} = Pictures $ [drawBorder _wSize] ++ scores
                      ++ [drawTarget _wTarget] ++ map drawPlayer _wPlayers ++ [ fogo ]
  where
    pos = if _wFogo < 0
          then _tPos _wTarget
          else _pPos $ _wPlayers !! _wFogo
    fogoColor = if _wGrace <= 0 then light yellow else greyN 0.7
    fogo = translateP pos $ color fogoColor $ circleSolid 7
    scores = if _wShowScores then _wPlayers & traversed %@~ drawScore _wSize else []

drawBorder :: (Float,Float) -> Picture
drawBorder (w,h) = color borderColor $ Pictures [ l, r, t, b ]
  where
    borderColor = dark $ dark violet
    bw = 2*borderWidth
    l = translate (-w/2) 0 $ rectangleSolid bw h
    r = translate (w/2) 0 $ rectangleSolid bw h
    b = translate 0 (-h/2) $ rectangleSolid w bw
    t = translate 0 (h/2) $ rectangleSolid w bw


eventHandler :: Event -> World -> World
eventHandler ev = wKeys %~ handleKeys ev >>> updPlayers >>> saveSize >>> toggleScore
  where
    updPlayers w@World{..} = w & wPlayers.traversed %@~ updateFromKeys _wKeys
    saveSize = case ev of
      EventResize sz -> wSize .~ (sz & both %~ fromIntegral)
      _ -> id
    toggleScore = case ev of
      EventKey (Char 't') Down _ _ -> wShowScores %~ not
      _ -> id

stepFunction :: Float -> World -> World
stepFunction dt w@World{..}
  = w & wPlayers.traverse %~ stepPlayer dt
      & wPlayers.traverse %~ bounceOffBorder _wSize borderWidth
      & wTarget %~ stepTarget dt
      & wTarget %~ bounceOffBorder' _wSize borderWidth
      & updateFogo
      & interactions
      & wGrace -~ dt
  where
    interactions = case _wTarget of
      Token{} -> placeGoalIfNeeded
      Goal{}  -> updateGoal

updateGoal :: World -> World
updateGoal w@World{..} = w & upd
  where
    c = _wFogo
    goalPos = _tPos _wTarget
    curPos = _pPos $ _wPlayers !! c
    upd = if magV (goalPos .-. curPos) < playerR + goalR && _wGrace < 0
          then placeTarget >>> wFogo .~ (-1) >>> wPlayers.ix c.pScore +~ 1
          else id

placeGoalIfNeeded :: World -> World
placeGoalIfNeeded = execState $ do
  need <- use $ wFogo . to (>=0)
  when need $ do
    sz <- use wSize
    let (w,h) = sz & both %~ (/2) & both -~ (borderWidth + 1.1*goalR)
    x <- wRandomR (-w,w)
    y <- wRandomR (-h,h)
    wTarget .= Goal (x,y)

wRandomR :: (Float,Float) -> State World Float
wRandomR = zoom wRandom . state . randomR

placeTarget :: World -> World
placeTarget = execState $ do
  sz <- use wSize
  let (w,h) = sz & both %~ (/2) & both -~ (borderWidth + 2*tokenR)
  x <- wRandomR (-w,w)
  y <- wRandomR (-h,h)
  vx <- wRandomR (-1.8,1.8)
  vy <- wRandomR (-1.8,1.8)
  wTarget .= Token (x,y) (vx,vy)

updateFogo :: World -> World
updateFogo w@World{..} = w & upd
  where
    c = _wFogo
    cur = if c < 0 then _tPos _wTarget else _pPos $ _wPlayers !! c
    hits = zip [0..] (map _pPos _wPlayers) & filter ((/=c) . fst) & filter (closeby . snd)
    closeby pos = magV (pos .-. cur) < 2 * playerR
    upd = if _wGrace < 0 then upd' else id
    upd' = case hits of
      [] -> id
      ((n,_):_) -> wFogo .~ n >>> wGrace .~ graceT -- >>> wPlayers.ix n.pScore +~ 1


main :: IO ()
main = do
  w0 <- iniWorld
  let disp = InWindow "Játék" iniSize (10,10)
  play disp white 50 w0 drawWorld eventHandler stepFunction
