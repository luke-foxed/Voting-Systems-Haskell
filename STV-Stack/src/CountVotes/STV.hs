module CountVotes.STV where

import CleanVotes.Clean
import Types

weight :: Double
weight = 1.0

numSeats :: Int
numSeats = 4

quota :: SortedVotes -> Double
quota votes = ((realToFrac(length votes)) / realToFrac(numSeats + 1)) + 1

groupPref :: SortedVotes -> [String]
groupPref votes = map head votes

-- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
votesRecieved :: Candidates -> [String] -> [VotesRecieved]
votesRecieved candidates xs = reverse (isort (zip candidates (map (\x -> realToFrac(length (filter (== x) xs))) candidates)))

removeCandidate :: [VotesRecieved] -> VotesRecieved -> [VotesRecieved]
removeCandidate allCans can = filter (/=can) allCans

firstPref :: SortedVotes -> Candidates -> [VotesRecieved]
firstPref votes cans = votesRecieved cans (map head votes)

elected :: [(String, Double)]
elected = []

eliminated :: [(String, Double)]
eliminated = []

calculateWeightFactor :: Double -> Double -> Double -> Double -> [[String]] -> Double
calculateWeightFactor oldWeight votesRecieved surplus nonTransferableCount votes = oldWeight * (realToFrac(surplus) / (oldWeight * (votesRecieved - nonTransferableCount )))

-- if there are no other candidates in a list AFTER the current candidate, they have no transferable votes
calculateTotalNonTransferable :: [[String]] -> String -> Int
calculateTotalNonTransferable votes can = length ([x | x <- votes, x /= [] && head x == can && length (tail x) == 0])

-- work out transferable count for inputted candidate
calculateSurplusPerCandidate :: [[String]] -> String -> String -> Int
calculateSurplusPerCandidate votes currentCan nextCan = length ([x | x <- votes, tail x /= [] && head x == currentCan && head (tail x) == nextCan])

applySurplus :: [Double] -> [VotesRecieved] -> [VotesRecieved]
applySurplus surpluses votes = zip (map fst votes) (zipWith (+) surpluses (map snd votes))

-- run election without weights, if quota is met then move to elected and remove from contention, else eliminate the tail 
runElection :: Double -> Int -> [(String, Double)] -> [(String, Double)] -> [VotesRecieved] -> SortedVotes -> [(String, Double)]
runElection weight numSeats elected eliminated [] _ = elected
runElection weight 0 elected _ _ _ = elected
runElection weight numSeats elected eliminated firstPrefs allVotes = 
    if realToFrac(snd (head firstPrefs)) > 10 then do
        let nonTransferableVotes = calculateTotalNonTransferable allVotes (fst (head firstPrefs))
        let weightFactor = calculateWeightFactor weight (realToFrac (snd (head firstPrefs))) (realToFrac (snd (head firstPrefs)) - quota (allVotes)) (realToFrac nonTransferableVotes) allVotes
        let surplusForEach = [y | x <- filter (/= fst (head firstPrefs)) (map fst firstPrefs), let y = realToFrac (calculateSurplusPerCandidate allVotes (fst (head firstPrefs)) x)]
        let adjustedSurplus = map (*weightFactor) surplusForEach
        let updatedPrefs = applySurplus adjustedSurplus (tail firstPrefs)

        runElection weightFactor (numSeats - 1) (elected ++ [head firstPrefs]) eliminated updatedPrefs allVotes

    else

        runElection weight numSeats elected (eliminated ++ [last firstPrefs]) firstPrefs allVotes

startElection :: [VotesRecieved] -> [[String]] -> [(String, Double)]
startElection = runElection weight numSeats elected eliminated