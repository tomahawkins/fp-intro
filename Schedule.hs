module Main (main) where

-- This is the entry point of the program.
main :: IO ()
main = do
  -- Read the catalog.
  f <- readFile "catalog.txt"
  -- Parse the catalog into a schedule list.
  let schedule = parseCatalog $ words f

  -- Print conflicts for MWF classes.
  putStr "Monday:    " >> print (map (conflictCount schedule M ) mwfTimes)
  putStr "Wednesday: " >> print (map (conflictCount schedule W ) mwfTimes)
  putStr "Friday:    " >> print (map (conflictCount schedule F ) mwfTimes)

  -- Print conflicts for TuTh classes.
  putStr "Tuesday:   " >> print (map (conflictCount schedule Tu) ttTimes)
  putStr "Thursday:  " >> print (map (conflictCount schedule Th) ttTimes)

-- A type to represent a day of the week.
data Day = M | Tu | W | Th | F deriving (Show, Eq)

-- A type to represent a time in hours and minutes.
data Time = Time Int Int deriving (Show, Eq)

-- Make Time an Ord so we can compare them.
instance Ord Time where
  compare (Time h0 m0) (Time h1 m1)
    | h0 == h1  = compare m0 m1
    | otherwise = compare h0 h1

-- A class schedule is a list of days with starting and ending times.
type ClassSchedule = ([Day], Time, Time)

-- Given a huge list of words, parse it into a list of ClassSchedules.
parseCatalog :: [String] -> [ClassSchedule]
parseCatalog a = case a of
  "Days:" : d : "Time:" : from : "-" : to : rest
    | d /= "TBA" -> (parseDays d, parseTime from, parseTime to) : parseCatalog rest
  _ : rest -> parseCatalog rest
  [] -> []

-- Given a string, parse it into a list of Days.
parseDays :: String -> [Day]
parseDays a = case a of
  'M' :       rest -> M  : parseDays rest
  'T' : 'u' : rest -> Tu : parseDays rest
  'W' :       rest -> W  : parseDays rest
  'T' : 'h' : rest -> Th : parseDays rest
  'F' :       rest -> F  : parseDays rest
  [] -> []
  _ -> error $ "Invalid days: " ++ a

-- Given a string, parse it into a Time.
parseTime :: String -> Time
parseTime a = Time hour min
  where
  hour' = read $ take 2 a
  hour  = if am || hour' == 12 then hour' else hour' + 12
  min   = read $ take 2 $ drop 3 a
  am    = "am" == drop 5 a

-- Given Clarions ClassSchedules, a day of the week, and a time range,
-- count how many class are at the same time.
conflictCount :: [ClassSchedule] -> Day -> (Time, Time) -> Int
conflictCount schedule day (a, b) = length
  [ ()
  | (days, a', b') <- schedule
  , elem day days
  , a < b' && b > a'
  ]

-- Time ranges for MWF classes.
mwfTimes :: [(Time, Time)]
mwfTimes = [ (Time a 0, Time a 50) | a <- [8 .. 16] ]

-- Time ranges for TuTh classes.
ttTimes :: [(Time, Time)]
ttTimes =
  [ (Time  8  0, Time  9 15)
  , (Time  9 30, Time 10 45)
  , (Time 11  0, Time 12 15)
  , (Time 12 30, Time 13 45)
  , (Time 14  0, Time 15 15)
  , (Time 15 30, Time 16 45)
  ]

