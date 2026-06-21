# API Backend Haskell Prototype

**Status: [Draft/Prototype]**

This is a Haskell based API backend and a set of frontend pages using those API.
The project serves as a prototype for hosting Haskell code on Google Cloud Run to provide services for a web frontend.

## Current Scope

- Haskell API backend using Servant.
- Minimal frontend pages for Wordle and Bananagram solver experiments.
- Prototype deployment shape for Google Cloud Run.

## APIs

1. `POST /api/hello` - Simple greeting endpoint
1. `POST /api/wordle` - Wordle game solver engine
1. `POST /api/bananagram` - Bananagram solver engine

## Frontend

1. `/wordle/index.html` - Minimal UI for the Wordle solver
1. `/bananagram/index.html` - Minimal UI for the Bananagram solver

## Quick Start

```bash
cabal build
cabal run
```

## Tech Stack

1. Servant (Haskell web framework)
1. Alpine.js (Lightweight frontend reactivity)
1. Google Cloud Run (Serverless hosting)

## Verification

```bash
cabal build
```

## Limitations

- This repository is a draft prototype and should not be presented as a production backend.
- No automated test suite is documented in the README.
- Solver APIs and frontend pages are experimental.
