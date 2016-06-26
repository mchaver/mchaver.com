{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Lib

import qualified Data.Sequence as S

import           GHCJS.VDOM (DOMNode)
import qualified GHCJS.VDOM.Event as EV

import           LiveVDom
import LiveVDom.Components

import Valentine

runTasks :: DOMNode -> IO ()
runTasks container = do
  tasksMb  <- spawnIO $ S.empty
  -- _ <- forkIO $ getUsers interface usersMb auditViewsMb

  runDom container (return ()) $ displayTasks tasksMb


displayTasks :: STMMailbox (S.Seq Int) -> LiveVDom
displayTasks _ = [valentine|
<div>
  Hello from Valentine.
|]

main :: IO ()
main = do
  container <- createContainer
  runTasks container
