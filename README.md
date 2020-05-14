# Voting-Systems-Haskell
Haskell implementation of the following two voting systems:

- Alternative Votes
- Single Transferable Votes

In order to run this program, [Haskell](https://www.haskell.org/platform/) and [Stack](https://docs.haskellstack.org/en/stable/README/)
must first be installed.

Once installed, the program can be built via the following command:
`stack build && stack install && STV-Stack-exe.exe`

## Issues:

- The STV implementation incorrectly redistributes surplus votes.
