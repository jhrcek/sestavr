{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( run,
  )
where

import Api (SestavrAPI, sestavrApi)
import Config (Config (..))
import Control.Exception.Safe (catch, throwM)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.FileEmbed (embedFile)
import Data.Foldable (for_)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist ((==.))
import Database.Persist.Class
  ( delete,
    deleteWhere,
    get,
    insert,
    insertEntity,
    insertMany_,
    replace,
    selectList,
  )
import Database.Persist.Sql (SqlPersistM, fromSqlKey)
import Database.Persist.Sqlite
  ( ConnectionPool,
    createSqlitePool,
    runMigration,
    runSqlPersistMPool,
    runSqlPool,
  )
import Database.Persist.Types (Entity, entityKey, entityVal)
import Database.Sqlite
  ( Error (ErrorConstraint),
    SqliteException,
    seError,
  )
import Model
  ( EntityField
      ( ExerciseTagExerciseId,
        RoutineExerciseRoutineId
      ),
    Exercise,
    ExerciseId,
    ExerciseTag (..),
    ExerciseWithTags,
    Lesson,
    LessonId,
    Position,
    PositionId,
    Routine,
    RoutineExercise (..),
    RoutineId,
    RoutineWithExercises,
    Tag,
    TagId,
    eirDuration,
    eirExerciseId,
    exerciseDescription,
    exerciseId,
    exerciseImage,
    exerciseTagExerciseId,
    exerciseTagTagId,
    fromExercise,
    fromRoutine,
    getDurationMinutes,
    migrateAll,
    routineExerciseRoutineId,
    routineId,
    rweExercises,
    tagIds,
    toExercise,
    toRoutine,
  )
import qualified Network.Wai.Handler.Warp as Warp
import Servant
  ( (:<|>) (..),
    Application,
    Handler,
    Server,
    err404,
    err409,
    errBody,
    serve,
    throwError,
  )
import Servant.Server.StaticFiles (serveDirectoryWebApp)
import System.Directory (listDirectory)

run :: Config -> IO ()
run config = do
  let port = configPort config
      dbFile = configDbFile config
      imagesDir = configImagesDir config
      poolSize = configConnectionPoolSize config

  pool <- runStderrLoggingT $ createSqlitePool (Text.pack dbFile) poolSize
  runSqlPool (runMigration migrateAll) pool

  verifyImages pool imagesDir

  let app = serveApp pool imagesDir

  putStrLn $
    unlines
      [ "Sestavr spušťen.",
        "Otevři http://localhost:" ++ show port ++ " ve webovém prohlížeči.",
        "Pro ukončení stiskni CTRL+C."
      ]
  Warp.run port app

serveApp :: ConnectionPool -> FilePath -> Application
serveApp pool imagesDir = serve sestavrApi $ apiServer pool imagesDir

apiServer :: ConnectionPool -> FilePath -> Server SestavrAPI
apiServer pool imagesDir =
  getIndex
    :<|> getElmApp
    -- Tag
    :<|> getTags
    :<|> createTag
    :<|> deleteTag
    :<|> updateTag
    -- Position
    :<|> getPositions
    :<|> createPosition
    :<|> getPosition
    :<|> deletePosition
    :<|> updatePosition
    -- Exercise
    :<|> getExercises
    :<|> createExercise
    :<|> updateExercise
    :<|> deleteExercise
    -- Routine
    :<|> getRoutines
    :<|> createRoutine
    :<|> updateRoutine
    :<|> deleteRoutine
    -- Lesson
    :<|> getLessons
    :<|> createLesson
    :<|> deleteLesson
    -- Static files
    :<|> serveImages
  where
    runPool :: MonadIO m => SqlPersistM a -> m a
    runPool action = liftIO $ runSqlPersistMPool action pool
    serveImages = serveDirectoryWebApp imagesDir
    --
    handleConstraintError :: Handler x -> LBS.ByteString -> Handler x
    handleConstraintError action err =
      action `catch` handler
      where
        handler :: SqliteException -> Handler x
        handler ex = case seError ex of
          ErrorConstraint -> throw409 ex err
          _ -> throwM ex
    -- TAG
    getTags :: Handler [Entity Tag]
    getTags = runPool $ selectList [] []
    --
    createTag :: Tag -> Handler (Entity Tag)
    createTag tag =
      runPool (insertEntity tag)
        `handleConstraintError` "Tag with this name already exists"
    --
    deleteTag :: TagId -> Handler ()
    deleteTag tagId =
      runPool (delete tagId)
        `handleConstraintError` "Tag can't be deleted, it is used in some Exercises"
    --
    updateTag :: TagId -> Tag -> Handler ()
    updateTag tagId tag =
      runPool (replace tagId tag)
        `handleConstraintError` "Tag with this name already exists"
    -- POSITION
    getPositions :: Handler [Entity Position]
    getPositions = runPool $ selectList [] []
    --
    getPosition :: PositionId -> Handler Position
    getPosition positionId = do
      mPosition <- runPool $ get positionId
      case mPosition of
        Nothing -> throwError $ err404 {errBody = "Pozice neexistuje : " <> LBS.pack (show positionId)}
        Just p -> pure p
    --
    createPosition :: Position -> Handler (Entity Position)
    createPosition position =
      runPool (insertEntity position)
        `handleConstraintError` "Position with this name already exists"
    --
    deletePosition :: PositionId -> Handler ()
    deletePosition positionId =
      runPool (delete positionId)
        `handleConstraintError` "Position can't be deleted, it is used in some Exercises"
    --
    updatePosition :: PositionId -> Position -> Handler ()
    updatePosition positionId position =
      runPool (replace positionId position)
        `handleConstraintError` "Position with this name already exists"
    -- EXERCISE
    getExercises :: Handler [ExerciseWithTags]
    getExercises = runPool $ do
      exerciseToTags <- selectList [] [] :: SqlPersistM [Entity ExerciseTag]
      exerciseEntities <- selectList [] [] :: SqlPersistM [Entity Exercise]
      let eidToTags :: Map.Map ExerciseId [TagId]
          eidToTags =
            Map.fromListWith (<>) $
              fmap
                ( \entity ->
                    let val = entityVal entity
                     in ( exerciseTagExerciseId val,
                          [exerciseTagTagId val]
                        )
                )
                exerciseToTags
      pure $
        fmap
          ( \exEntity ->
              let tags = Map.findWithDefault [] (entityKey exEntity) eidToTags
               in fromExercise exEntity tags
          )
          exerciseEntities
    --
    createExercise :: ExerciseWithTags -> Handler ExerciseWithTags
    createExercise exerciseWithTags =
      do
        let exercise = toExercise exerciseWithTags
            tids = tagIds exerciseWithTags
        runPool $ do
          eid <- insert exercise
          insertMany_ $ fmap (ExerciseTag eid) tids
          pure $ exerciseWithTags {exerciseId = eid}
        `handleConstraintError` "Exercise with this name already exists"
    --
    updateExercise :: ExerciseId -> ExerciseWithTags -> Handler ExerciseWithTags
    updateExercise eid exerciseWithTags =
      do
        let exercise = toExercise exerciseWithTags
            tids = tagIds exerciseWithTags
        runPool $ do
          replace eid exercise
          deleteWhere [ExerciseTagExerciseId ==. eid]
          insertMany_ $ fmap (ExerciseTag eid) tids
          pure exerciseWithTags
        `handleConstraintError` "Exercise with this name already exists"
    --
    deleteExercise :: ExerciseId -> Handler ()
    deleteExercise eid =
      runPool
        ( do
            deleteWhere [ExerciseTagExerciseId ==. eid]
            delete eid
        )
        `handleConstraintError` "This exercise can't be deleted, because it's used in some routine"
    -- ROUTINE
    getRoutines :: Handler [RoutineWithExercises]
    getRoutines = runPool $ do
      routinesToExercises <- selectList [] [] :: SqlPersistM [Entity RoutineExercise]
      routineEntities <- selectList [] [] :: SqlPersistM [Entity Routine]
      let ridToRooutineExercises :: Map.Map RoutineId [RoutineExercise]
          ridToRooutineExercises =
            Map.fromListWith (<>) $
              fmap
                ( \entity ->
                    let val = entityVal entity
                     in ( routineExerciseRoutineId val,
                          [val]
                        )
                )
                routinesToExercises
      pure $
        fmap
          ( \routineEntity ->
              let res = Map.findWithDefault [] (entityKey routineEntity) ridToRooutineExercises
               in fromRoutine routineEntity res
          )
          routineEntities
    --
    createRoutine :: RoutineWithExercises -> Handler RoutineWithExercises
    createRoutine rwe = do
      let routine = toRoutine rwe
          exs = rweExercises rwe
      runPool $ do
        rid <- insert routine
        insertMany_ $
          zipWith
            ( \e index ->
                RoutineExercise
                  { routineExerciseRoutineId = rid,
                    routineExerciseExerciseId = eirExerciseId e,
                    routineExerciseDurationMin = getDurationMinutes $ eirDuration e,
                    routineExerciseOrder = index
                  }
            )
            exs
            [0 ..]
        pure $ rwe {routineId = rid}
    --
    updateRoutine :: RoutineId -> RoutineWithExercises -> Handler RoutineWithExercises
    updateRoutine rid rwe =
      do
        let routine = toRoutine rwe
            exs = rweExercises rwe
        runPool $ do
          replace rid routine
          deleteWhere [RoutineExerciseRoutineId ==. rid]
          insertMany_ $
            zipWith
              ( \e index ->
                  RoutineExercise
                    { routineExerciseRoutineId = rid,
                      routineExerciseExerciseId = eirExerciseId e,
                      routineExerciseDurationMin = getDurationMinutes $ eirDuration e,
                      routineExerciseOrder = index
                    }
              )
              exs
              [0 ..]
          pure rwe
    --
    deleteRoutine :: RoutineId -> Handler ()
    deleteRoutine rid =
      runPool
        ( do
            deleteWhere [RoutineExerciseRoutineId ==. rid]
            delete rid
        )
        `handleConstraintError` "This routine can't be deleted, because it's used in some lesson"
    -- LESSON
    getLessons :: Handler [Entity Lesson]
    getLessons = runPool $ selectList [] []
    --
    createLesson :: Lesson -> Handler (Entity Lesson)
    createLesson lesson = runPool $ insertEntity lesson
    --
    deleteLesson :: LessonId -> Handler ()
    deleteLesson lessonId = runPool $ delete lessonId

throw409 :: SqliteException -> LBS.ByteString -> Handler a
throw409 e detail = throwError $ err409 {errBody = detail <> "; " <> LBS.pack (show e)}

getIndex :: Handler ByteString
getIndex = pure indexHtml

getElmApp :: Handler ByteString
getElmApp = pure elmApp

indexHtml :: ByteString
indexHtml = $(embedFile "client/dist/index.html")

elmApp :: ByteString
elmApp = $(embedFile "client/dist/main.js")

{- Print out warnings about
- images being referenced in Exercises but without corresponding file in the images directory
- image files in images directory, which are not linked from any exercise
-}
verifyImages :: ConnectionPool -> FilePath -> IO ()
verifyImages pool imagesDir = do
  imageFiles <- Set.fromList <$> listDirectory imagesDir
  exercises <- runSqlPersistMPool (selectList [] [] :: SqlPersistM [Entity Exercise]) pool
  let allImageReferences =
        fmap
          ( \exerciseEntity ->
              let exercise = entityVal exerciseEntity
               in (entityKey exerciseEntity, extractImageLinks exercise)
          )
          exercises
      missingFiles =
        fmap
          ( \(key, refs) -> (key, refs `Set.difference` imageFiles)
          )
          allImageReferences
      unusedImages = imageFiles `Set.difference` foldMap snd allImageReferences
  for_ missingFiles $ \(exKey, missing) ->
    unless (Set.null missing)
      $ putStrLn
      $ "Exercise " ++ show (fromSqlKey exKey) ++ " refers to nonexistent images: " ++ show missing
  for_ unusedImages $ \img ->
    putStrLn $ "Image " ++ img ++ " is not used in any exercise"

extractImageLinks :: Exercise -> Set FilePath
extractImageLinks exercise =
  ( case exerciseImage exercise of
      Just imgRef -> Set.insert (Text.unpack imgRef)
      Nothing -> id
  )
    $ exerciseDescriptionLinks (exerciseDescription exercise)
  where
    exerciseDescriptionLinks :: Text -> Set FilePath
    exerciseDescriptionLinks description =
      Set.fromList
        . List.map
          ( Text.unpack . Text.takeWhile (/= ')')
              . Text.drop 1
              . Text.dropWhile (/= '(')
          )
        . List.tail
        $ Text.splitOn "![" description
