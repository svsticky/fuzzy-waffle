# KandidaatsBestuurGokken (KBG)
## Candidate Board Guessing (CBG)

KBG is a web application for guessing the members of the next (candidate) board.

## Requirements
- GHC >= 9.6.7
- Cabal >= 3.0
- PostgreSQL >= 16

## Setting up
Copy `sample.env` to `.env` and fill in the oauth details.

Then simply start the application:
```bash
cabal update
cabal run
```
