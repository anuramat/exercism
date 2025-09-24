module Clock (addDelta, fromHourMin, toString) where

import Text.Printf (printf)

data Clock = Clock {hours :: Int, minutes :: Int}
  deriving (Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = addDelta h m $ Clock 0 0

toString :: Clock -> String
toString c = printf "%02d:%02d" (hours c) (minutes c)

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m c = Clock {hours = h', minutes = m'}
  where
    totalMinutes = m + minutes c + 60 * (h + hours c)
    m' = mod totalMinutes 60
    h' = mod (div totalMinutes 60) 24
