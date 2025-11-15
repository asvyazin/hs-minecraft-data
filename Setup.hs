{-# LANGUAGE OverloadedStrings #-}

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup (BuildFlags (..))

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { buildHook = buildHook'
      }

buildHook' :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildHook' pkg_descr local_bld_info user_hooks bld_flags = do
  -- Run the data generation before building
  generateData

  -- Then run the normal build
  buildHook simpleUserHooks pkg_descr local_bld_info user_hooks bld_flags

generateData :: IO ()
generateData = do
  putStrLn "Generating Minecraft protocol data..."