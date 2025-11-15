{-# LANGUAGE OverloadedStrings #-}

import Distribution.PackageDescription
import Distribution.Simple (UserHooks, buildHook, defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup (BuildFlags (..))
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

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
  let decls = [DataDecl () (NewType ()) Nothing (DHead () (Ident () "VarInt")) [QualConDecl () Nothing Nothing (ConDecl () (Ident () "VarInt") [TyCon () (UnQual () (Ident () "Int"))])] [Deriving () Nothing [IRule () Nothing Nothing (IHCon () (UnQual () (Ident () "Show"))), IRule () Nothing Nothing (IHCon () (UnQual () (Ident () "Eq")))]]]
  let mod = Module () (Just (ModuleHead () (ModuleName () "Data.Minecraft.Types") Nothing Nothing)) [] [] decls
  print $ prettyPrint mod
  putStrLn "Generating Minecraft protocol data..."