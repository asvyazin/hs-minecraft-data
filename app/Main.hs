{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)

program :: (MonadIO m) => m ()
program = do
  liftIO $ putStrLn "Hello, Fused Effects!"

main :: IO ()
main = program
