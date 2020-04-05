{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Api (SestavrAPI)
import Data.Proxy (Proxy (..))
import Database.Persist.Sql (toSqlKey)
import Database.Persist.Types (Key)
import Model
import Network.HTTP.Client (parseRequest_)
import Servant.API (Raw)
import Servant.QuickCheck ((<%>), defaultArgs, not500, serverSatisfies, withServantServer)
import Servant.QuickCheck.Internal.HasGenRequest (HasGenRequest, genRequest)
import Server (mkTestServer)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck (Arbitrary, applyArbitrary2, applyArbitrary3, arbitrary)
import Test.QuickCheck.Instances.Text ()

sestavrApi :: Proxy SestavrAPI
sestavrApi = Proxy

main :: IO ()
main = hspec
  $ describe "my server"
  $ do
    it "follows best practices" $ do
      withServantServer sestavrApi mkTestServer $ \baseUrl ->
        serverSatisfies
          sestavrApi
          baseUrl
          defaultArgs
          ( not500
              --   <%> onlyJsonObjects
              --   <%> notAllowedContainsAllowHeader
              <%> mempty
          )

instance Arbitrary (Key Target) where
  arbitrary = toSqlKey <$> arbitrary

instance Arbitrary (Key Position) where
  arbitrary = toSqlKey <$> arbitrary

instance Arbitrary (Key Exercise) where
  arbitrary = toSqlKey <$> arbitrary

instance Arbitrary (Key Routine) where
  arbitrary = toSqlKey <$> arbitrary

instance Arbitrary Target where
  arbitrary = Target <$> arbitrary

instance Arbitrary Position where
  arbitrary = Position <$> arbitrary

instance Arbitrary ExerciseWithTargets where
  arbitrary =
    ExerciseWithTargets
      <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary RoutineWithExercises where
  arbitrary = applyArbitrary3 RoutineWithExercises

instance Arbitrary ExerciseInRoutine where
  arbitrary = applyArbitrary2 ExerciseInRoutine

instance Arbitrary DurationMinutes where
  arbitrary = DurationMinutes <$> arbitrary

instance HasGenRequest Raw where
  genRequest _ = (1, pure (\baseUrl -> parseRequest_ $ show baseUrl))
