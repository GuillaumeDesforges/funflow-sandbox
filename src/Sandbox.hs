{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Arrows #-}

module Sandbox where

import Path ( absdir, Path, Abs, Dir )

import Control.Arrow ( returnA, (>>>) )
import Control.Funflow ( SimpleFlow, step, stepIO, docker )
import Control.Funflow.Exec.Simple ( withSimpleLocalRunner )
import Control.Funflow.External ( OutputCapture(NoOutputCapture, StdOutCapture), stringParam, Env(EnvInherit, EnvExplicit) )
import qualified Control.Funflow.External.Docker as Docker

import qualified Data.CAS.ContentStore as CS

import Common


-- Example 1 : we can make a Flow from a pure function using `step`
flow1 :: (Num a) => SimpleFlow a a
flow1 = step (+ 1)

example1 :: (Num a, Show a) => Example a a
example1 = Example {
    flow = flow1,
    description = "flow from a pure function",
    input = 0,
    success = logResult
}


-- Example 2 : a Flow can do IO using the `stepIO` function
flow2 :: SimpleFlow () ()
flow2 = stepIO (\() -> putStrLn "Hello! This is a message that was made in a flow!")

example2 :: Example () ()
example2 = Example {
    flow = flow2
,
    description = "simple flow that uses IO",
    input = (),
    success = const mempty
}


-- Example 3 : we can write more complex flows using the Arrow notation `proc`
flow3 :: SimpleFlow () String
flow3 = proc () -> do
    -- IO steps
    firstName <- stepIO (\() -> putStrLn "What's your first name ?" >> getLine) -< ()
    lastName <- stepIO (\() -> putStrLn "What's your last name ?" >> getLine) -< ()
    -- pure step
    fullName <- step $ (\(firstName, lastName) -> firstName ++ " " ++ lastName) -< (firstName, lastName)
    -- Some other IO steps before returning the result
    stepIO (\fullName -> putStrLn $ "Your full name is " ++ fullName) -< fullName
    -- return the result previously computed
    returnA -< fullName

example3 :: Example () String
example3 = Example {
    flow = flow3,
    description = "complex flow using the Arrow notation",
    input = (),
    success = logResult
}

-- Example 4 : we can compose flows
flow4 :: (Num a, Show a) => SimpleFlow a a
flow4 = flow1 >>> flow1 >>> flow1

example4 :: (Num a, Show a) => Example a a
example4 = Example {
    flow = flow4,
    description = "a composition of flows that increment the input three times",
    input = 0,
    success = \result -> logResult result
}

-- Example 5 : run a task in Docker
flow5 :: SimpleFlow () CS.Item
flow5 = docker $ const Docker.Config {
    Docker.image = "ubuntu",
    Docker.optImageID = Nothing,
    Docker.command = stringParam "pwd",
    -- could be NoOutputCapture
    Docker.stdout = NoOutputCapture,
    Docker.args = [],
    -- could be EnvExplicit
    Docker.env = EnvInherit
}

example5 :: Example () CS.Item
example5 = Example {
    flow = flow5,
    description = "a flow running a task in Docker",
    input = (),
    success = const mempty
}

--
-- Run the examples
--

runExamples :: IO ()
runExamples = do
    let path :: Path Abs Dir
        path = [absdir|/tmp/funflow|]
        
        runner :: FlowRunner a b
        runner flow input = withSimpleLocalRunner path $ \run -> run flow input
    
    putStrLn "=== Running examples of flows ===\n"
    -- testFlow runner example1
    -- testFlow runner example2
    -- testFlow runner example3
    -- testFlow runner example4
    testFlow runner example5

