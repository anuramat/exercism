module Clock (addDelta, fromHourMin, toString) where
import Text.Printf (printf)

data Clock = Clock { hours :: Int, minutes :: Int }
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin h m = addDelta h m $ Clock 0 0

toString :: Clock -> String
toString c = printf "%02d:%02d" (hours c) (minutes c)

addDelta :: Int -> Int -> Clock -> Clock
addDelta h m c = Clock { hours = newh , minutes = newm } where
  totalMinutes = m + minutes c + 60 * ( h + hours c )
  newm = mod totalMinutes 60
  newh = mod (div totalMinutes 60) 24
