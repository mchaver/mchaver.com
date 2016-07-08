{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

import qualified Data.Sequence as S

import           GHCJS.Types
import           GHCJS.VDOM (DOMNode)
import qualified GHCJS.VDOM.Event as EV

import           LiveVDom
import           LiveVDom.Components
import qualified LiveVDom.Types as T

import           Valentine

data Task = Task {
  taskDetails   :: JSString
, taskCompleted :: Bool
}

runTasks :: DOMNode -> IO ()
runTasks container = do
  newTaskDetailsMb <- spawnIO ""
  tasksMb  <- spawnIO $ S.empty
  -- _ <- forkIO $ getUsers interface usersMb auditViewsMb

  runDom container (return ()) $ displayTasks tasksMb newTaskDetailsMb


displayTasks :: STMMailbox (S.Seq Task)
             -> STMMailbox JSString
             -> LiveVDom
displayTasks tasksMb newTaskDetailsMb = [valentine|
<div>
  Hello from Valentine.
  <div>
    &{forEach tasksMb $ displayTask}
  <div>
    ${textBoxWith (\str -> sendMessage (snd newTaskDetailsMb) str) [] Nothing}
    ${T.addEvent (EV.click setTasksButtonOnClicked) allButtonHtml}
|]
  where
    -- setTaskDetails str = sendMessage (snd newTaskDetailsMb) str


    setTasksButtonOnClicked e = do
      newTaskDetails <- (recvIO $ fst newTaskDetailsMb)
      allTasks <- (recvIO $ fst tasksMb)
      runMessages $ sendMessage (snd tasksMb) (allTasks S.|> (Task newTaskDetails False))

    allButtonHtml = [valentine|
      <button>
        Add Task
    |]
    -- addTask str = sendMessage (snd tasksMb) ((fst tasksMb) S.|> (Task str False))
    --  $${textBoxWith (\str -> sendMessage (snd mb) (ExampleMailbox str) ) [] Nothing}
{-
 $${textBoxWith (\str -> sendMessage (snd mb) (ExampleMailbox str) ) [] Nothing}

usersAllOnClicked e = do
      allXs <- (recvIO $ fst usersMb)
      allOnClicked selectedUsersMb allXs e

    noneOnClicked selectedXsMb e = do
      EV.preventDefault e
      runMessages $ sendMessage (snd selectedXsMb) $ S.empty

    allButtonHtml = [valentine|
      <button class="btn btn-default btn-xs">
        <i class="fa fa-check">
          All
|]

addTask :: S.Seq Task -> JSString -> Message ()
addTask t s = do
  t S.|> s
  return ()
-}

displayTask :: Task -> (Maybe Task -> Message ()) -> LiveVDom
displayTask t _ = [valentine|
<div>
  <span>
    Task:
  <span>
    ^{taskDetails t}
|]


main :: IO ()
main = do
  container <- createContainer
  runTasks container
