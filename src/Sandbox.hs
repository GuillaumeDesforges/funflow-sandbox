{-# LANGUAGE Arrows #-}

module Sandbox where

import Control.Arrow
import Control.Funflow

someFunc :: IO ()
someFunc = putStrLn "someFunc"

someTask :: SimpleFlow () String
someTask = proc () -> do
    name <- stepIO (const $ putStrLn "Yolo ?" >> getLine) -< ()
    returnA -< name
