# Overview

This is a Haskell based API backend and a set of frontend pages using those API.
The purpose of this project is to host some Haskell code on Google cloud run and use it as a backend for a frontend application.

# APIs

1. `POST /api/hello` - Say hello
1. `POST /api/wordle` - Wordle game solver
1. `POST /api/scrabble` - Scrabble solver

# Frontend

1. /wordle/index.html - Wordle game solver frontend

# Tech stack

1. Servant (Haskell web framework)
1. Alpine.js (Frontend framework)
1. Google Cloud Run (Hosting)
