module Agenda.Drag
  ( computeDropMinuteIndex
  , indexToMinutes
  , indexToTimeLabel
  ) where

import Prelude

import Data.Int as Int
import Data.String as String

computeDropMinuteIndex :: Int -> Int -> Int
computeDropMinuteIndex cursorIndex offsetMinutes =
  let
    totalMinutes = max 0 ((cursorIndex * 5) - offsetMinutes)
    rawIndex = Int.quot totalMinutes 5
  in
    clamp 0 287 rawIndex

indexToMinutes :: Int -> Int
indexToMinutes idx =
  clamp 0 1439 (idx * 5)

indexToTimeLabel :: Int -> String
indexToTimeLabel idx =
  let
    totalMinutes = indexToMinutes idx
    hour = Int.quot totalMinutes 60
    minute = Int.rem totalMinutes 60
  in
    pad2 hour <> ":" <> pad2 minute

pad2 :: Int -> String
pad2 n =
  let
    raw = Int.toStringAs Int.decimal n
  in
    if String.length raw == 1 then "0" <> raw else raw
