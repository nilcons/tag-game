{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Target where

import           Control.Arrow
import           Control.Lens
import           Data.AffineSpace
import           Data.Default
import qualified Data.Set as Set
import           Data.VectorSpace
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import           Graphics.Gloss.Geometry.Angle

import           Player

data Target
  = Token
    { _tPos :: !Point
    , _tSpd :: !Point
    }
  | Goal
    { _tPos :: !Point
    }
  deriving (Show)
makeLenses ''Target

instance Default Target where
  def = Goal (0,0)

tokenR, goalR, goalW :: Float
tokenR = playerR
goalR = 60
goalW = 5


drawTarget :: Target -> Picture
drawTarget Token{..} = translateP _tPos $ Color black $ circleSolid tokenR
drawTarget Goal{..} = translateP _tPos $ Color black $ thickCircle goalR goalW

stepTarget :: Float -> Target -> Target
stepTarget dt t = case t of
  Goal{} -> t
  Token{..} -> t & tPos %~ (.+^ _tSpd)

-- TODO(klao): refactor
bounceOffBorder' :: (Float,Float) -> Float -> Target -> Target
bounceOffBorder' sz bw t@Token{..} = t & horiz & vert
  where
    (w,h) = sz & both %~ (/2) & both -~ (bw + tokenR)
    (x,y) = _tPos
    (vx, vy) = _tSpd
    horiz = if abs x > w && signum x == signum vx then tSpd._1 %~ negate else id
    vert  = if abs y > h && signum y == signum vy then tSpd._2 %~ negate else id
bounceOffBorder' _ _ t = t
