#!/usr/bin/env runhaskell

import           Data.Monoid
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util



-- default build directory
buildDir = "_build"


-- main build
main :: IO () -- , shakeVerbosity=Diagnostic
main = (shakeArgs shakeOptions{shakeFiles=buildDir} ) execute
  where
   -- Build Target files and directories
   execute = wants >> rules
   wants = want ([packageExecutableFile
                , sandbox ] ++ (fmap (\mod -> buildDir </> mod </> gitHiddenFile) subModules))
   subModules = [ghcjsServantClient
               , ghcjsJSValCombinators
               , ffiQQ]

   ghcjsServantClient = "ghcjs-servant-client"
   ghcjsJSValCombinators = "ghcjs-jsval-combinators"
   gitHiddenFile = ".git"
   ffiQQ = "ghcjs-ffiqq"
   sandbox = ".cabal-sandbox" </> "add-source-timestamps"
   packageExecutableFile  =  "dist" </> "build" </> "test-frontend"  </> "test-frontend" <.> "jsexe" </> "all.js"

   rules =  packageExecutableFileRule <>
            ghcjsServantClientRule <>
            ghcjsJSValCombinatorsRule <>
            sandboxRule <>
            ffiQQRule <>
            cleanarg
            
   ghcjsServantClientRule = (buildDir </> ghcjsServantClient </> gitHiddenFile) %> \out -> do
     gitAddOneTime (buildDir </> ghcjsServantClient)  "https://github.com/plow-technologies/ghcjs-servant-client.git"
         
   ghcjsJSValCombinatorsRule = (buildDir </> ghcjsJSValCombinators </> gitHiddenFile) %> \out -> do
     gitAddOneTime (buildDir </> ghcjsJSValCombinators) "https://github.com/plow-technologies/ghcjs-jsval-combinators.git"
        
   ffiQQRule = (buildDir </> ffiQQ </> gitHiddenFile) %> \out -> do
     gitAddOneTime (buildDir </> ffiQQ) "https://github.com/ghcjs/ghcjs-ffiqq.git"

   
   packageExecutableFileRule = packageExecutableFile %> \out -> do
        need [ sandbox
             , buildDir </> ghcjsServantClient </> gitHiddenFile
             , buildDir </> ffiQQ </> gitHiddenFile
             ]
        () <- cmd "cabal update"
        () <- cmd "cabal install --ghcjs --reorder-goals --only-dependencies"
        () <- cmd "cabal configure --ghcjs"
        cmd "cabal build"

   sandboxRule = sandbox %> \_ -> do
       need $ (\p -> (buildDir </> p </> gitHiddenFile) ) <$> subModules
       _ <- cmd "cabal sandbox init" :: Action ()
       _ <- cmd "cabal sandbox add-source ../test-types" :: Action ()
       _ <- traverse sandboxAdd subModules
       return ()
     where
       sandboxAdd :: FilePath -> Action () 
       sandboxAdd m = cmd "cabal sandbox add-source" [buildDir </> m]
   -- Cleanup --------------------------------------------------

   cleanarg = phony "clean" $ do
       putNormal "cleaning files in build"
       putNormal "removing submodules ..."
       (cmd "git submodule deinit -f " ) `traverse` ((buildDir </> ) `fmap` subModules)   :: Action [()]
       (cmd "git rm -f") `traverse` ((buildDir </> ) `fmap` subModules)   :: Action [()]
       () <- cmd "rm -rf" [".git" </> "modules" </> buildDir]
       () <- cmd "rm -rf" [buildDir]
       () <- cmd "cabal clean"
       () <- cmd "cabal sandbox delete"
       return ()

-- | fp is the local file path of the git repo, gitpath is the remote (https://...) path
gitAddOneTime :: FilePath  -> FilePath  -> Action ()
gitAddOneTime fp gitPath  = do
    rslt <- doesFileExist (fp </> ".git" )
    if rslt
       then do
         putNormal $ fp ++ " exists"
         gitInitSubCmd
         gitUpdateSubCmd
       else do
         putNormal $ fp ++ "/.git " ++ " does not exist"
         dirExists <- doesDirectoryExist fp         
         if dirExists
            then removeDirectory >> gitAddSubCmd >> gitInitSubCmd >> gitUpdateSubCmd
            else do
              gitAddSubCmd
              gitInitSubCmd
              gitUpdateSubCmd
  where
     gitAddSubCmd :: Action ()
     gitAddSubCmd = command [(Cwd buildDir)] "git" ["submodule","add",gitPath]
     gitInitSubCmd = command_ [] "git" ["submodule","init" ]
     gitUpdateSubCmd = command_ [] "git" ["submodule","update" , fp]
     removeDirectory =   command_ [] "rm" ["-rf", fp]

