module Common where

import GHC.Exception (SomeException)
import Control.Funflow (SimpleFlow)

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

-- Some util functions to run the flows
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
    putStrLn ""

-- Some util functions for the examples
logResult :: (Show a) => a -> IO ()
logResult result = putStrLn $ "# At the end of the flow, we get the result: " ++ show result
