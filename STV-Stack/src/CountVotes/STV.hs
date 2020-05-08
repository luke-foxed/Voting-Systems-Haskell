module CountVotes.STV where

import CleanVotes.Clean
import Types

weight :: Double
weight = 1.0

quota :: SortedVotes -> Int -> Double
quota votes numSeats = realToFrac (length votes) / realToFrac (numSeats + 1)

elected :: [(String, Double)]
elected = []

eliminated :: [(String, Double)]
eliminated = []

-- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
votesRecieved :: Candidates -> [String] -> [VotesRecieved]
votesRecieved candidates xs = reverse (isort (zip candidates (map (\x -> realToFrac(length (filter (== x) xs))) candidates)))

-- get count of first preference votes for each candidate
firstPref :: SortedVotes -> Candidates -> [VotesRecieved]
firstPref votes cans = votesRecieved cans (map head votes)

-- remove candidate from list of first preferences
removeCandidate :: [VotesRecieved] -> VotesRecieved -> [VotesRecieved]
removeCandidate allCans can = filter (/=can) allCans

-- calculate updated weight factor using old weight with the transferable and surplus votes of candidate
calculateWeightFactor :: Double -> Double -> Double -> Double
calculateWeightFactor oldWeight surplus transferableVotes = oldWeight * (realToFrac surplus / (oldWeight * transferableVotes))

-- if there are no other candidates in a list AFTER the current candidate, they have no transferable votes
calculateTransferable :: SortedVotes -> String -> Int
calculateTransferable votes can = length ([x | x <- votes, x /= [] && head x == can && not (null (tail x))])

-- calculate exact number of votes from candidate x to go to candidate y (before applying weighting factor)
calculateSurplusPerCandidate :: SortedVotes -> String -> String -> Int
calculateSurplusPerCandidate votes currentCan nextCan = length ([x | x <- votes, tail x /= [] && head x == currentCan && head (tail x) == nextCan])

-- add list of surpluses to the respective candidates
applySurplus :: [Double] -> [VotesRecieved] -> [VotesRecieved]
applySurplus surpluses votes = zip (map fst votes) (zipWith (+) surpluses (map snd votes))

-- main function which returns the elected candidates
runElection :: Double -> Double -> Int -> [(String, Double)] -> [(String, Double)] -> [VotesRecieved] -> SortedVotes -> [(String, Double)]
runElection _ _ _ elected _ [] _ = elected
runElection _ _ 0 elected _ _ _  = elected
runElection
  calcQuota
  weight
  numSeats
  elected
  eliminated
  firstPrefs
  allVotes

  -- initial run, check if candidate equal or greater than quota - if so, elect them, distribute the surplus and re run the election
  | realToFrac (snd (head firstPrefs)) > calcQuota = do 
        let transferable = calculateTransferable allVotes (fst (head firstPrefs))
        let weightFactor = calculateWeightFactor weight (realToFrac (snd (head firstPrefs)) - calcQuota) (realToFrac transferable)
        let surplusForEach = [y | x <- filter (/= fst (head firstPrefs)) (map fst firstPrefs), let y = realToFrac (calculateSurplusPerCandidate allVotes (fst (head firstPrefs)) x)]
        let adjustedSurplus = map (* weightFactor) surplusForEach
        let updatedPrefs = applySurplus adjustedSurplus (tail firstPrefs)

        runElection calcQuota weightFactor (numSeats - 1) (elected ++ [head firstPrefs]) eliminated updatedPrefs allVotes

  -- if there is only one seat left and one candidate left, elect the candidate regardless
  | numSeats == 1 && length firstPrefs == 1 = 
        runElection calcQuota weight (numSeats - 1) (elected ++ [head firstPrefs]) [] [] []

  -- else eliminate the candidate with the least first preferences and distribute their votes
  | otherwise = do 
        let transferable = calculateTransferable allVotes (fst (last firstPrefs))
        let surplusForEach = [y | x <- filter (/= fst (last firstPrefs)) (map fst firstPrefs), let y = realToFrac (calculateSurplusPerCandidate allVotes (fst (last firstPrefs)) x)]
        let adjustedSurplus = map (* weight) surplusForEach
        let updatedPrefs = applySurplus adjustedSurplus (removeCandidate firstPrefs (last firstPrefs))
        
        runElection calcQuota weight numSeats elected (eliminated ++ [last firstPrefs]) updatedPrefs allVotes

-- function to communicate with driver to start the election
startElection :: Int -> Double -> [VotesRecieved] -> [[String]] -> [(String, Double)]
startElection seats calcQuota = runElection calcQuota weight seats elected eliminated


------------------------
--      OLD CODE      --
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

-- calculateSurplusPerCandidate :: [[String]] -> String -> String -> Int
-- calculateSurplusPerCandidate votes currentCan nextCan = length ([x | x <- votes, tail x /= [] && head x == currentCan && nextCan `elem` [head (tail x)]])

-- redistributeVotes :: [(String, Int)] -> Double -> Double -> [(String, Int)]
-- redistributeVotes votes transferableVotes weightFactor = [(x, y+ round(transferableVotes * weightFactor)) | (x,y ) <- tail votes]

-- test7 = realToFrac (calculateTransferable finalVotes "D. Milliband")
-- test8 = calculateWeightFactor weight 111 43.5 test7 finalVotes
-- test9 = redistributeVotes firstPref 40 test8
-- test10 = zipWith (zipWith (+)) [1,2,3,4] [("D. Abbott",2),("E. Balls",18),("A. Burbhm",17),("E. Milliband",40)]

-- test13 surpluses votes = [y | x <- surpluses, let y = map (applySurplus x) votes]

-- calculate how much votes the next candidate should have after redistributing the surplus
-- calculateFinalSurplus :: Int -> Double -> Double
-- calculateFinalSurplus votesRecieved weightFactor = (realToFrac(votesRecieved) / (weightFactor)) - realToFrac(votesRecieved)

-- distributeSurplus :: [(String, Int)] -> Double -> [(String, Int)]
-- distributeSurplus votes surplus = [(x,y+ round(surplus)) | (x,y ) <- [head (tail votes)]] ++ tail (tail votes)

-- distributeEliminated :: [(String, Int)] -> Double -> [(String, Int)]
-- distributeEliminated votes surplus = [(x,y+ round(surplus)) | (x,y ) <- [head votes]] ++ removeCandidate (tail votes) (last votes)

-- test1 = calculateWeightFactor (snd (last firstPref)) (finalVotes) (fst (last firstPref))
-- test2 = calculateFinalSurplus (snd (last firstPref)) test1
-- test3 = distributeEliminated firstPref test2

-- recalculateAllVotes :: [(String, Int)] -> Double -> [(String, Int)]
-- recalculateAllVotes votes weightFactor = [(x, round ((realToFrac y) / weightFactor)) | (x,y) <- tail votes]

-- to do --> fix calculating transferable votes
-- drop from list until candidate is head of that list
-- ensure the tail of this new list is not null

-- test4 = getIndex "D. Milliband" "E. Milliband"
-- test5 = filter (/=[]) (reassembleVotes test4 (filterVotes "D. Milliband" "E. Milliband" finalVotes))
-- test6 = calculateTransferable test5 "D. Milliband"

-- reassembleVotes :: [Int] -> [[String]]-> [[String]]
-- reassembleVotes indexes votes = zipWith (drop) (indexes) (votes)

-- getIndex :: String -> String -> [Int]
-- getIndex currentCan nextCan = map (fromMaybe 0 . (nextCan `elemIndex`)) (filterVotes currentCan nextCan finalVotes)