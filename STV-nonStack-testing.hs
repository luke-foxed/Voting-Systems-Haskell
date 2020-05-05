import Data.List(sort,maximumBy,elemIndex)
import Data.Ord(comparing)
import Data.Maybe
import Debug.Trace

-- temp hard code available seats 
weight :: Int
weight = 1000

numSeats :: Int
numSeats = 3

votes :: [[String]]
votes = [
    ["","","D. Abbott","E. Balls","A. Burbhm","D. Milliband","E. Milliband"],
    ["1","Ms D Abbott MP  ","1","*","*","*","*"],
    ["2","RtHon B W Ainsworth MP ","5","4","3","1","2"],
    ["3","RtHon D Alexander MP ","5","3","4","1","2"],
    ["4","Ms H Alexander MP ","*","*","1","2","3"],
    ["5","Ms R Ali MP     ","5","3","4","1","2"]
    ]

------------------------
-- SORTING & CLEANING --
------------------------

-- get candidates
candidates :: [String]
candidates = drop 2 (head votes)

-- ignore list items that contain empty strings
rmEmptyStrings :: [[String]] -> [[String]]
rmEmptyStrings = filter (not . null) . map (filter (not . null)) 

-- keep only the voting scores from each list
rmNames :: [[String]] -> [[String]]
rmNames (x:xs) = [ drop 2 x | x <- xs]

cleanVotes :: [[String]]
cleanVotes = rmNames (rmEmptyStrings votes)

votesLength :: Int
votesLength = length cleanVotes

quota :: Int
quota = (votesLength `div` (numSeats + 1)) + 1

-- go through each vote and apply zipCandidate
groupCandidateVotes :: [[(String, String)]]
groupCandidateVotes = map zipCandidate cleanVotes

-- zip candidate with specific vote
zipCandidate :: [String] -> [(String, String)]
zipCandidate = zip candidates

sortVotes :: [[(String, String)]]
sortVotes = map isort groupCandidateVotes

-- taken from notes 
isort :: Ord a => [(String, a)] -> [(String, a)]
isort [] = []
isort (x:xs) = insertion x (isort xs)

-- taken from notes and modified to apply on second string in tuple
insertion :: Ord a => (String, a) -> [(String, a)]  -> [(String, a)]
insertion x [] = [x]
insertion x (y:ys) 
                | snd x <= snd y = x : y : ys
                | otherwise = y: insertion x ys

-- return list of candidates sorted by vote number
extractVotes :: [[(String, String)]]
extractVotes = map rmEmptyVotes sortVotes

-- remove asterix and vote number which isn't needed 
rmEmptyVotes :: [(String, String)] -> [(String, String)]
rmEmptyVotes vote = [(fst x, snd x) | x <- vote, snd x /= "*"]

-- take from list until duplicate is encountered, modified from: https://stackoverflow.com/questions/28755554/taking-from-a-list-until-encountering-a-duplicate
takeUntilDuplicate :: Eq a => [(String, a)] -> [(String, a)]
takeUntilDuplicate = helper []
    where helper seen [] = seen
          helper seen (x:xs)
              | snd x `elem` map snd seen = init seen
              | otherwise = helper (seen ++ [x]) xs

rmVoteTally :: [(String, String)] -> [String]
rmVoteTally = map fst

readAsInt :: String -> Int
readAsInt s = read s :: Int

-- takeUntilGap :: [(String, String)] -> [(String, String)]
-- takeUntilGap = helper []
--     where helper seen [] = seen
--           helper seen (x:xs)
--               | not (null seen) && snd x `elem` [show (readAsInt(snd (last seen)) - 1 )] = init seen
--               | otherwise = helper (seen ++ [x]) xs

takeUntilGap :: [(String, String)] -> [(String, String)]
takeUntilGap = helper []
    where helper seen [] = seen
          helper seen (x:xs)
              | not (null seen) && readAsInt (snd x) - readAsInt(snd (last seen)) > 1 = seen
              | otherwise = helper (seen ++ [x]) xs

finalVotes :: [[String]]
finalVotes =  map rmVoteTally (map takeUntilGap (map takeUntilDuplicate extractVotes))
------------------------
--  ALTERNATIVE VOTE  --
------------------------

-- The following code is sourced from 'Programming in Haskell' by Graham Hutton.
-- The code is referenced from pages 86 to 88.

countOccurances :: Eq a => a -> [a] -> Int
countOccurances x = length . filter (== x)

getFirstPastPost :: Ord a => [a] -> [(Int, a)]
getFirstPastPost vs = sort [(countOccurances v vs, v) | v <- rmDuplicates vs]

rmEmptyBallots :: Eq a => [[a]] -> [[a]]
rmEmptyBallots = filter (/= [])

rmDuplicates :: Eq a => [a] -> [a]
rmDuplicates []     = []
rmDuplicates (x:xs) = x : filter (/= x) (rmDuplicates xs)

eliminateCandidate :: Eq a => a -> [[a]] -> [[a]]
eliminateCandidate x = map (filter (/= x))

rankCandidates :: Ord a => [[a]] -> [a]
rankCandidates = map snd . getFirstPastPost . map head

getWinner :: Ord a => [[a]] -> a
getWinner bs = case rankCandidates (rmEmptyBallots bs) of
                [c]    -> c
                (c:cs) -> getWinner (eliminateCandidate c bs)

startAlternativeVoting :: String
startAlternativeVoting = getWinner finalVotes

------------------------
--      ST VOTE       --
------------------------

-- -- take first vote of sorted list of votes
-- extractFirstVote:: [String] -> (String, [String])
-- extractFirstVote (x:xs) = (x, xs)

-- -- seperate first vote from other votes
-- seperateVote :: [(String, [String])]
-- seperateVote = map (extractFirstVote) extractVotes

-- -- return a list of each first vote
-- listFirstVote :: [String]
-- listFirstVote = [fst x | x <- seperateVote]

-- -- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
-- countFirstVote :: [String] -> [(String,Int)]
-- countFirstVote xs = isort (zip candidates (map (\x -> length (filter (== x) xs)) candidates))

-- -- get last candidate in sorted list
-- roundWinner :: Int -> (String, Int)
-- roundWinner index = (countFirstVote listFirstVote) !! (length (countFirstVote listFirstVote) - index)

-- -- if vote count exceeds quota, add surplus to next roundWinner
-- -- this needs to be recursive to change roundWinner value
-- getSurplus :: (String, Int) -> (String, Int)
-- getSurplus (x, y) | y > quota = (addSurplus quota (roundWinner 2))
--                   | otherwise = (x, y)

-- -- add surplus to candidate
-- addSurplus :: Int -> (String, Int) -> (String, Int)
-- addSurplus z (x, y) = (x, y + z) 

-- removeLoser :: [(String, Int)] -> [(String, Int)]
-- removeLoser xs = tail xs

------------------------
--     ST VOTE V2     --
------------------------

electedCandidates :: [(String, Int)]
electedCandidates = []

groupPref :: [String]
groupPref = map head finalVotes

-- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
votesRecieved :: [String] -> [(String,Int)]
votesRecieved xs = reverse (isort (zip candidates (map (\x -> length (filter (== x) xs)) candidates)))

removeCandidate :: [(String, Int)] -> (String, Int) -> [(String, Int)]
removeCandidate allCans can = filter (/=can) allCans

-- steps for recursion
-- 1. Pass in canididate votes recieved
-- 2. Take head of that list and work out if quota is matched 
-- 3. If so, add to elected list. If greater than quota, distribute votes
-- 4. Else, drop the tail of the list and repeat 

firstPref :: [(String, Int)]
firstPref = votesRecieved (map head finalVotes)

-- secondPref :: [(String, Int)]
-- secondPref = (votesRecieved (map (filter (/=[]) (map (drop 1) finalVotes))))

-- thirdPref :: (String, Int)
-- thirdPref = head (votesRecieved (map head (filter (/=[]) (map (drop 2) finalVotes))))

-- fourthPref :: (String, Int)
-- fourthPref = head (votesRecieved (map head (filter (/=[]) (map (drop 3) finalVotes))))

-- finalPrefs :: [(String, Int)]
-- finalPrefs = [firstPref] ++ [secondPref] ++ [thirdPref] ++ [fourthPref]

elected :: [(String, Int)]
elected = []

eliminated :: [(String, Int)]
eliminated = []

-- run election without weights, if quota is met then move to elected and remove from contention, else eliminate the tail 
startElection :: Int -> [(String, Int)] -> [(String, Int)] -> [(String, Int)] -> IO()
startElection numSeats elected eliminated [] = print elected
startElection 0 elected _ _ = print elected
startElection numSeats elected eliminated votes = 
    if snd (head votes) > quota then do
        let weightFactor = calculateWeightFactor (snd (head votes) - quota) (finalVotes) (fst (head votes))
        -- let surplus = calculateFinalSurplus (snd (head votes)) weightFactor
        let recalculatedVotes = recalculateAllVotes votes weightFactor

        putStrLn "\nWEIGHT FACTOR --> " 
        print weightFactor 

        putStrLn "\nRECALCUALTED VOTES --> " 
        print recalculatedVotes

        startElection (numSeats - 1) (elected ++ [head votes]) eliminated recalculatedVotes
    else do

        print $ (head votes , "IS NOT ELECTED")

        let weightFactor = calculateWeightFactor (snd (last votes)) (finalVotes) (fst (last votes))
        let surplus = calculateFinalSurplus (snd (last votes)) weightFactor
        let recalculatedVotes = distributeEliminated votes surplus
        startElection (numSeats) (elected) (eliminated ++ [(last votes)]) (recalculatedVotes)

calculateWeightFactor :: Int -> [[String]] -> String -> Double
calculateWeightFactor surplus votes can = realToFrac(surplus) / realToFrac(calculateTransferable votes can)

-- if there are no other candidates in a list AFTER the current candidate, they have no transferable votes
calculateTransferable :: [[String]] -> String -> Int
calculateTransferable votes can = length ([x | x <- votes, x /= [] && head x == can && length (tail x) == 0])

-- calculate how much votes the next candidate should have after redistributing the surplus
calculateFinalSurplus :: Int -> Double -> Double
calculateFinalSurplus votesRecieved weightFactor = (realToFrac(votesRecieved) / (weightFactor)) - realToFrac(votesRecieved)

distributeSurplus :: [(String, Int)] -> Double -> [(String, Int)]
distributeSurplus votes surplus = [(x,y+ round(surplus)) | (x,y ) <- [head (tail votes)]] ++ tail (tail votes)

distributeEliminated :: [(String, Int)] -> Double -> [(String, Int)]
distributeEliminated votes surplus = [(x,y+ round(surplus)) | (x,y ) <- [head votes]] ++ removeCandidate (tail votes) (last votes)

test1 = calculateWeightFactor (snd (last firstPref)) (finalVotes) (fst (last firstPref))
test2 = calculateFinalSurplus (snd (last firstPref)) test1
test3 = distributeEliminated firstPref test2

recalculateAllVotes :: [(String, Int)] -> Double -> [(String, Int)]
recalculateAllVotes votes weightFactor = [(x, round ((realToFrac y) / weightFactor)) | (x,y) <- tail votes]

-- to do --> fix calculating transferable votes
-- drop from list until candidate is head of that list
-- ensure the tail of this new list is not null

getIndex :: String -> [Int]
getIndex can = map (fromMaybe 0 . (can `elemIndex`)) finalVotes

-- test4 = map (getIndex "D. Milliband") finalVotes
reassembleVotes :: [Int] -> [[String]]-> [[String]]
reassembleVotes indexes votes = zipWith (drop) (indexes) (votes)

test4 = getIndex "D. Milliband"
test5 = reassembleVotes test4 finalVotes
test6 = calculateTransferable test5 "D. Milliband"

getSecondPref can votes = ([x | x <- votes, x /= [] && head x == can && length (tail x) == 0])