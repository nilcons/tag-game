module Keys where

import Graphics.Gloss.Interface.Pure.Game
import Data.Set

data IKey = Bal | Jobb | Fel deriving (Eq,Ord,Enum,Show)

data PKey = PKey {-# UNPACK #-} !Int !IKey
            deriving (Eq,Ord,Show)

type KeySet = Set PKey

playerKey :: Key -> Maybe PKey
playerKey k = case k of
  Char 's' -> Just $ PKey 1 Bal
  Char 'a' -> Just $ PKey 1 Fel
  Char 'd' -> Just $ PKey 1 Jobb
  SpecialKey KeyLeft -> Just $ PKey 0 Bal
  SpecialKey KeyPageDown -> Just $ PKey 0 Fel
  SpecialKey KeyDown -> Just $ PKey 0 Jobb
  Char 'j' -> Just $ PKey 2 Bal
  Char 'b' -> Just $ PKey 2 Fel
  Char 'k' -> Just $ PKey 2 Jobb
  Char 'i' -> Just $ PKey 3 Bal
  Char 'o' -> Just $ PKey 3 Fel
  Char 'p' -> Just $ PKey 3 Jobb
  _ -> Nothing

handleKeys :: Event -> KeySet -> KeySet
handleKeys ev = case ev of
  EventKey key ud _ _ -> case playerKey key of
    Nothing -> id
    Just pkey -> case ud of
      Up -> delete pkey
      Down -> insert pkey
  _ -> id
