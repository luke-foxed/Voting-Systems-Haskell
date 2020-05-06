module CountVotes.STV where

import CleanVotes.Clean
import Types

weight :: Double
weight = 1.0

numSeats :: Int
numSeats = 4

quota :: SortedVotes -> Double
quota votes = realToFrac (length votes) / realToFrac (numSeats + 1)

-- get tuple of candidate and vote (modified from https://codereview.stackexchange.com/questions/88720/return-list-with-numbers-of-color-occurrences-in-another-list)
votesRecieved :: Candidates -> [String] -> [VotesRecieved]
votesRecieved candidates xs = reverse (isort (zip candidates (map (\x -> realToFrac(length (filter (== x) xs))) candidates)))

-- get count of first preference votes for each candidate
firstPref :: SortedVotes -> Candidates -> [VotesRecieved]
firstPref votes cans = votesRecieved cans (map head votes)

elected :: [(String, Double)]
elected = []

eliminated :: [(String, Double)]
eliminated = []

-- calculate updated weight factor using old weight with the transferable and surplus votes of candidate
calculateWeightFactor :: Double -> Double -> Double -> Double
calculateWeightFactor oldWeight surplus transferableVotes = oldWeight * (realToFrac surplus / (oldWeight * transferableVotes))

-- if there are no other candidates in a list AFTER the current candidate, they have no transferable votes
calculateTransferable :: [[String]] -> String -> Int
calculateTransferable votes can = length ([x | x <- votes, x /= [] && head x == can && not (null (tail x))])

-- calculate exact number of votes from candidate x to go to candidate y (before applying weighting factor)
calculateSurplusPerCandidate :: [[String]] -> String -> String -> Int
calculateSurplusPerCandidate votes currentCan nextCan = length ([x | x <- votes, tail x /= [] && head x == currentCan && head (tail x) == nextCan])

applySurplus :: [Double] -> [VotesRecieved] -> [VotesRecieved]
applySurplus surpluses votes = zip (map fst votes) (zipWith (+) surpluses (map snd votes))

-- run election without weights, if quota is met then move to elected and remove from contention, else eliminate the tail 
runElection :: Double -> Int -> [(String, Double)] -> [(String, Double)] -> [VotesRecieved] -> SortedVotes -> [(String, Double)]
runElection weight numSeats elected eliminated [] _ = elected
runElection weight 0 elected _ _ _ = elected
runElection weight numSeats elected eliminated firstPrefs allVotes = 
    if realToFrac(snd (head firstPrefs)) > quota allVotes then do
        let transferable = calculateTransferable allVotes (fst (head firstPrefs))
        let weightFactor = calculateWeightFactor weight (realToFrac (snd (head firstPrefs)) - quota allVotes) (realToFrac transferable)
        let surplusForEach = [y | x <- filter (/= fst (head firstPrefs)) (map fst firstPrefs), let y = realToFrac (calculateSurplusPerCandidate allVotes (fst (head firstPrefs)) x)]
        let adjustedSurplus = map (*weightFactor) surplusForEach
        let updatedPrefs = applySurplus adjustedSurplus (tail firstPrefs)

        runElection weightFactor (numSeats - 1) (elected ++ [head firstPrefs]) eliminated updatedPrefs allVotes

    else
        -- need to distribute loosing votes here
        runElection weight numSeats elected (eliminated ++ [last firstPrefs]) firstPrefs allVotes

startElection :: [VotesRecieved] -> [[String]] -> [(String, Double)]
startElection = runElection weight numSeats elected eliminated