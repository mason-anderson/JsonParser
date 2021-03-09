module Main where

import System.Environment
import Data.Maybe

import JsonParser

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return $ snd <$> runParser parser input

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then
        putStrLn "needs one argument for filename"
    else do
        let filename = args !! 0
        json <- parseFile filename jsonValue
        putStrLn $ fromMaybe "failed to parse" $ show <$> json
    return ()
