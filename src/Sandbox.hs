{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Arrows #-}

module Sandbox where

import Data.Char (toUpper)
import GHC.Exception (SomeException)
import Path (absdir, Path, Abs, Dir)

import Control.Arrow (returnA, (>>>))
import Control.Funflow (SimpleFlow, step, stepIO)
import Control.Funflow.Exec.Simple (withSimpleLocalRunner)


-- Some types to help understand what we manipulate here
type FlowResult b = IO (Either SomeException b)
type FlowRunner a b = SimpleFlow a b -> a -> FlowResult b

data Example a b = Example {
    flow :: (SimpleFlow a b),
    description :: String,
    input :: a,
    -- Specific to our example here
    success :: b -> IO ()
}

--
-- Define some flows (the most interesting part !)
--

-- Flow from a pure function
flowFromFunction :: (Num a) => SimpleFlow a a
flowFromFunction = step (\x -> x + 1)

-- Flow can use IO using the `stepIO` function
flowUsingIO :: SimpleFlow () ()
flowUsingIO = stepIO (\() -> putStrLn "Hello! This is a message that was made in a flow!")

-- Flow using the arrow syntax
flowUsingArrowSyntax :: SimpleFlow () String
flowUsingArrowSyntax = proc () -> do
    -- IO steps
    firstName <- stepIO (\() -> putStrLn "What's your first name ?" >> getLine) -< ()
    lastName <- stepIO (\() -> putStrLn "What's your last name ?" >> getLine) -< ()
    -- pure step
    fullName <- step $ (\(firstName, lastName) -> firstName ++ " " ++ lastName) -< (firstName, lastName)
    -- Some other io step without a result to bind
    stepIO (\fullName -> putStrLn $ "Your full name is " ++ fullName) -< fullName
    -- return the result
    returnA -< fullName

-- Flow to demonstrate how to compose flows
flowFromComposition :: SimpleFlow () ()
flowFromComposition = flowUsingIO >>> flowUsingIO

--
-- Our examples to run
--

logResult :: (Show a) => a -> IO ()
logResult result = putStrLn $ "# At the end of the flow, we get the result: " ++ show result

exampleFlowFromFunction :: (Num a, Show a) => Example a a
exampleFlowFromFunction = Example {
    flow = flowFromFunction,
    description = "simple flow from a pure function",
    input = 0,
    success = \result -> logResult result
}

exampleFlowThatUsesIO :: Example () ()
exampleFlowThatUsesIO = Example {
    flow = flowUsingIO,
    description = "simple flow that uses IO",
    input = (),
    success = \_ -> mempty
}

exampleFlowUsingArrowSyntax :: Example () String
exampleFlowUsingArrowSyntax = Example {
    flow = flowUsingArrowSyntax,
    description = "flow that uses the Arrow syntax",
    input = (),
    success = \result -> logResult result
}

exampleFlowFromComposition :: Example () ()
exampleFlowFromComposition = Example {
    flow = flowFromComposition,
    description = "a composition of flows",
    input = (),
    success = \result -> logResult result
}

--
-- Run the examples
--

logErr :: Show a => a -> IO ()
logErr err = putStrLn $ "/!\\ Something went wrong:" ++ show err

logSuccess :: IO ()
logSuccess = putStrLn "# It worked !"

testFlow :: FlowRunner a b -> Example a b -> IO ()
testFlow run example = do
    putStrLn $ "# Running the example '" ++ description example ++ "'"
    result <- run (flow example) (input example)
    case result of
        Left error -> logErr error
        Right something -> logSuccess >> (success example) something

runExamples :: IO ()
runExamples = do
    let path :: Path Abs Dir
        path = [absdir|/tmp/funflow|]
        
        runner :: FlowRunner a b
        runner flow input = withSimpleLocalRunner path $ \run -> run flow input
    
    testFlow runner exampleFlowFromFunction
    testFlow runner exampleFlowThatUsesIO
    testFlow runner exampleFlowUsingArrowSyntax
    testFlow runner exampleFlowFromComposition
