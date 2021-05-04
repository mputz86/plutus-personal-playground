
# Personal Playground

Playground for my own personal experiments.

Note: Heavily relies on source code in/from
- [plutus](https://github.com/input-output-hk/plutus) repository
- Especially the [use-cases](https://github.com/input-output-hk/plutus/tree/master/plutus-use-cases) project
- Some code from [plutus-pioneer-program](https://github.com/input-output-hk/plutus-pioneer-program) was copied for easy reference (in `src/WeekXX` folders like in original repo)


## Test Case: Parallel Auction

See [src/ParallelAuction.hs](src/ParallelAuction.hs) for the implementation.
Lacks probably still a lot of corner cases. Focus was rather to get a grip on all the data, the flow, a half-way nice style of coding, ... .

Idea is to try out if bidding in an auction can be done in parallel by using different UTxOs if multiple bids should be placed within the same slot.

TODOs
- Closing endpoint is not complete, since assumtion was that access to datum of input is given, but it is not.

Using it:
- Use [src/ParallelAuctionTrace.hs](src/ParallelAuctionTrace.hs) in `cabal repl`
- Run `testParallelBidding`, `testSequentialBidding` or `testSimpleBidding`


## Test Case: Payback

Somehow paying back an UTxO with a token attached resulted in the transaction submitter "payed the payback". Problem was the missing split of the value of the UTxO into the native (ADA) amount and the token before using the value.
