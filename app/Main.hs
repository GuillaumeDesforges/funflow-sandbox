{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Funflow.Exec.Simple
import Path

import Sandbox

main :: IO ()
main = do
    let path = [absdir|/tmp/funflow|]
    res <- withSimpleLocalRunner path (\run -> run someTask ())
    case res of
        Left err -> putStrLn $ "Something went wrong:" ++ show err
        Right something -> putStrLn something