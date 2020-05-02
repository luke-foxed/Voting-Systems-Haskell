module Main where

import CleanVotes.Clean
import AlternativeVote.Alternative

main :: IO ()
main = do

    csvData <- readFile "votes.csv"
    let formatted = parseRawVotes csvData
    let cleanedVotes = finalVotes formatted

    putStrLn "Please Select A Voting System:\n"
    putStrLn "1) Alternative Vote"
    putStrLn "2) Single Transferable Vote"
    choice <- getLine

    if choice == "1" then do 
        let altVoteWinner = startAlternativeVoting cleanedVotes
        print ("Winner is: " ++ altVoteWinner)

    else if choice =="2" then putStrLn "Running STV"
    else putStrLn "Please enter a valid option..."