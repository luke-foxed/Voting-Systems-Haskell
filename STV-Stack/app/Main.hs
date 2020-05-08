module Main where

import CleanVotes.Clean
import CountVotes.STV
import CountVotes.Alternative

main :: IO ()
main = do

    csvData <- readFile "votes.csv"
    let formatted = parseRawVotes csvData
    let cleanedVotes = finalVotes formatted
    let cans = candidates formatted

    putStrLn "Please Select A Voting System:\n"
    putStrLn "1) Alternative Vote"
    putStrLn "2) Single Transferable Vote"
    choice <- getLine

    if choice == "1" then do 
        let altVoteWinner = startAlternativeVoting cleanedVotes
        print $ "Winner is: " ++ altVoteWinner

    else if choice =="2" then do

        putStrLn "Enter the number of seats:"
        seats <- getLine

        let calcQuota = quota cleanedVotes (readAsInt seats)
        let votingPrefs = firstPref cleanedVotes cans

        -- DETAILED OUTPUT 
        electionResults <- startDetailedElection (readAsInt seats) calcQuota votingPrefs cleanedVotes
        putStrLn electionResults 

        -- RETURN ONLY WINNERS
        -- let electionResults = startElection (readAsInt seats) calcQuota votingPrefs cleanedVotes
        -- print (show electionResults)

    else do
        putStrLn "\nPlease enter a valid option...\n"
        main
