{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Player where

import           Control.Arrow
import           Control.Lens
import           Data.AffineSpace
import           Data.Default
import           Data.Foldable (toList)
import           Data.Monoid (mempty)
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.VectorSpace
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Angle

import           Keys

type Tail = Seq (Point,Point)

data Player
  = Player
    { _pPos :: !Point
    , _pSpd :: !Point
    , _pAng :: !Float
    , _pRot :: !Float
    , _pColor :: !Color
    , _pAcc :: !Bool
    , _pName :: !String
    , _pScore :: !Float
    , _pTail :: !Tail
    } deriving (Show)
makeLenses ''Player

instance Default Player where
  def = Player (0,0) (0,0) 0 0 black False "" 0 mempty

playerR :: Float
playerR = 15

drawPlayer :: Player -> Picture
drawPlayer Player{..} = translateP _pPos $ rotate (-radToDeg _pAng) $ -- scale 3 3 $
                        Pictures $ body1 . body2 . flame $ []
  where
    body1 = (:) $ Color (dim $ dim _pColor) $ circleSolid playerR
    body2 = (:) $ Color (light _pColor) $ polygon [(30,0),(-21,-6),(-16,0),(-21,6)]
    flameP = [(-25,0), (-30,3), (-35,4), (-40,3), (-45,0)]
    flameP' = tail $ reverse $ tail $ map (second negate) flameP
    flame = if not _pAcc then id else (:) $ Color red $ polygon $ flameP ++ flameP'

drawTail :: Player -> Picture
drawTail Player{..} = Color col $ Pictures $ quads
  where
    col = light . light . light . dim . dim $ _pColor
    ps  = toList _pTail
    quads = zipWith f ps (tail ps)
    f (ph1, pt1) (ph2, pt2) = Polygon [ph1, ph2, pt2, pt1]

-- TODO(klao): factor out
translateP :: Point -> Picture -> Picture
translateP (x,y) = translate x y

updateFromKeys :: KeySet -> Int -> Player -> Player
updateFromKeys keys pId = pRot .~ rot >>> pAcc .~ acc
  where
    [bal, jobb, fel] = map (\k -> Set.member (PKey pId k) keys) [Bal, Jobb, Fel]
    rot = case (bal,jobb) of
      (True,False) -> 1
      (False,True) -> -1
      _ -> 0
    acc = not fel

addToTail :: Point -> (Float, Float) -> Tail -> Tail
addToTail p d t = Seq.take 400 $ (p',p'') <| t
  where
    p' = p .+^ 4 *^ d
    p'' = p .-^ 4 *^ d

stepPlayer :: Float -> Player -> Player
stepPlayer dt p@Player{..} = p & pAng +~ dt*3*_pRot & pPos .~ pos' & pSpd .~ spd'
                               & pTail %~ addToTail _pPos dir
  where
    pos' = _pPos .+^ _pSpd
    spd' = _pSpd ^+^ acc
    dir = unitVectorAtAngle _pAng
    acc = -0.01 *^ _pSpd ^+^ (if _pAcc then 0.02 else 0) *^ dir

bounceOffBorder :: (Float,Float) -> Float -> Player -> Player
bounceOffBorder sz bw p@Player{..} = p & horiz & vert
  where
    (w,h) = sz & both %~ (/2) & both -~ (bw + playerR)
    (x,y) = _pPos
    (vx, vy) = _pSpd
    horiz = if abs x > w && signum x == signum vx then pSpd._1 %~ negate else id
    vert  = if abs y > h && signum y == signum vy then pSpd._2 %~ negate else id

drawScore :: (Float,Float) -> Int -> Player -> Picture
drawScore sz k Player{..} = translate x y $ color c $ Pictures [ name, score ]
  where
    (w,h) = sz & both %~ (/2)
    x = w - 180
    y = h - 50 - 30 * fromIntegral k
    c = dark $ dim _pColor
    sc = scale 0.18 0.18
    name = sc $ Text _pName
    score = translate 80 0 $ sc $ Text (show $ floor _pScore)
