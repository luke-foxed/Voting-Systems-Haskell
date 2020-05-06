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
        let votingPrefs = firstPref cleanedVotes cans
        let electionResults = startElection votingPrefs cleanedVotes
        
        print electionResults 

    else do
        putStrLn "\nPlease enter a valid option...\n"
        main