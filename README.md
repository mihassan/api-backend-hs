# Overview

**Status: [Draft/Prototype]**

This is a Haskell based API backend and a set of frontend pages using those API.
The project serves as a prototype for hosting Haskell code on Google Cloud Run to provide services for a web frontend.

# APIs

1. `POST /api/hello` - Simple greeting endpoint
1. `POST /api/wordle` - Wordle game solver engine
1. `POST /api/bananagram` - Bananagram solver engine

# Frontend

1. `/wordle/index.html` - Minimal UI for the Wordle solver
1. `/bananagram/index.html` - Minimal UI for the Bananagram solver

# Tech stack

1. Servant (Haskell web framework)
1. Alpine.js (Lightweight frontend reactivity)
1. Google Cloud Run (Serverless hosting)
