module AlternativeVote.Alternative where

import Data.List(sort)

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

startAlternativeVoting :: [[String]] -> String
startAlternativeVoting votes = getWinner votes