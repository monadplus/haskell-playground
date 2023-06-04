-- https://gitlab.haskell.org/DavidEichmann/ghc-debug/-/blob/2682813867a7743b479259831db65985e1fe90c3/test/test-progs/LargeThunk.hs
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Function
import Data.List (foldl', maximumBy, permutations, sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Debug.Trace
import GHC.Generics (Generic)
import System.Environment (getArgs)

data MovieDB = MovieDB
  { users :: !(Map UserID User),
    movies :: !(Map MovieID Movie),
    ratings :: ![Rating]
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype UserID = UserID Int
  deriving newtype (Eq, Ord, Num, Enum)
  deriving stock (Generic)
  deriving anyclass (NFData)

data User = User
  { name :: !String,
    -- | How trusted this user is. In range [0,1]. Higher is more trustworthy.
    trust :: !Double
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

newtype MovieID = MovieID Int
  deriving newtype (Eq, Ord, Num, Enum)
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

type Movie = String

data Rating = Rating
  { -- | Who rated this movie.
    user :: !UserID,
    -- | Movie being rated.
    movie :: !MovieID,
    -- | Rating In range [1,10]. Higher is better.
    rating :: !Double
  }
  deriving stock (Generic)
  deriving anyclass (NFData)

-- $ ghc -eventlog -rtsopts -O2 LargeThunk

-- $ ./LargeThunk 100000 100000 30000000 +RTS -l -hT -i0.5 -RTS

main :: IO ()
main = do
  [nUsersStr, nMoviesStr, nRatingsStr] <- getArgs
  let nUsers = read nUsersStr
      nMovies = read nMoviesStr
      nRatings = read nRatingsStr

  putStrLn $
    "Creating the DB with size ("
      ++ show nUsers
      ++ " users,"
      ++ show nMovies
      ++ " movies,"
      ++ show nRatings
      ++ " ratings)"
  let !movieDB =
        let users =
              M.fromList $
                zip
                  [0 ..]
                  [ User name trust
                    | name <-
                        take nUsers $
                          cycle $
                            permutations "abcdefghijklmnopqrstuvwxyz"
                    | trust <- cycle [0.0, 0.1 .. 1.0]
                  ]

            movies =
              M.fromList $
                zip
                  [0 ..]
                  ( take nMovies $
                      fmap unwords $
                        cycle $
                          permutations ["The", "Cat", "In", "The", "Hat", "House", "Mouse", "Story", "Legend"]
                  )

            ratings =
              take nRatings $
                cycle $
                  zipWith3
                    Rating
                    (cycle (M.keys users))
                    (cycle (M.keys movies))
                    (cycle [1.0, 1.01 .. 9])
         in force $
              MovieDB
                { users = users,
                  movies = movies,
                  ratings = ratings
                }

  do
    let msg = "DB created."
    traceMarkerIO msg
    putStrLn msg
  -- putStrLn "Hit ENTER to continue."
  -- _ <- getLine

  -- A single user's favorite movie.
  do
    let (userIDA, userA) = M.assocs (users movieDB) !! 0
        favoriteMovieID =
          snd $
            maximumBy
              (compare `on` fst)
              [ (rating, movieID)
                | Rating userIDB movieID rating <- ratings movieDB,
                  userIDA == userIDB
              ]

    putStrLn $ "User '" ++ name userA ++ "''s favorite movie: " ++ (movies movieDB M.! favoriteMovieID)

  -- Top rated movies
  -- each rating is weighted by the user's trust score.
  do
    let -- (Movie ID, rating) sorted by rating.
        movieRatings :: [(MovieID, Double)]
        movieRatings =
          sortBy (compare `on` snd) $
            fmap (\(movieID, (weightedRating, weight)) -> (movieID, weightedRating / weight)) $
              filter (\(_, (_, weight)) -> weight /= 0) $
                M.assocs $
                  foldl' go M.empty (ratings movieDB)

        go :: Map MovieID (Double, Double) -> Rating -> Map MovieID (Double, Double)
        go weights (Rating userID movieID score) =
          M.alter
            ( \case
                Nothing -> Just (userTrust * score, userTrust)
                Just (weightedScore, weight) ->
                  -- let !weightedScore' = weightedScore + (userTrust * score)
                  --     !weight' = weight + userTrust
                  let weightedScore' = weightedScore + (userTrust * score)
                      weight' = weight + userTrust
                   in Just (weightedScore', weight')
            )
            movieID
            weights
          where
            -- !userTrust = trust ((users movieDB) M.! userID)
            userTrust = trust ((users movieDB) M.! userID)

    putStrLn "Top 10 movies:"
    putStrLn $ unlines $ fmap show $ take 10 $ reverse $ movieRatings

  traceMarkerIO "Top 10 movies done."
  -- putStrLn "Hit ENTER to continue."
  -- _ <- getLine

  touch movieDB

{-# NOINLINE touch #-}
touch :: MovieDB -> IO ()
touch movieDB = if null (ratings movieDB) then putStrLn "" else return ()
