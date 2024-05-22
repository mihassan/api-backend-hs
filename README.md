# Overview

This is a Haskell based API backend and a set of frontend pages using those API.
The purpose of this project is to host some Haskell code on Google cloud run and use it as a backend for a frontend application.

# APIs

1. `GET /api/_healthz` - Health check
1. `GET /api/hello` - Say hello
1. `GET /api/wordle` - Wordle game solver

# Frontend

1. /wordle/index.html - Wordle game solver frontend

# Tech stack

1. Scotty (Haskell web framework)
1. Alpine.js (Frontend framework)
1. Google Cloud Run (Hosting)
