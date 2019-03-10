#lang pollen

◊h1{Exploring Shake}
◊h2{Having fun with an existing Makefile}
◊m-article{
  ◊p{I had to get a couple of things running to work on a system with several parts, but all that made up the "onboarding" was a giant Makefile that assumed that I would be working off OSX, which wasn't the case. Since I had to go through the trouble of reading the Makefile and extracting instructions on what to do, I decided to rewrite a working version with Shake...}
  ◊p{
    ◊m-code-haskell{
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Util

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles="_build" } $ do
  let wantedDirs = [
          "foo"
        , "bar"
        ]

  phony "setup" $ ensureDirsExists wantedDirs

  phony "docker" $ do
    let dockerConfig = ".docker-compose/config.yml"
    need [dockerConfig]
    serviceCheck <- isDockerServiceRunning service project dockerConfig
    if not serviceCheck
      then do
          putNormal "Running Docker instances..."
          cmd_ ("docker-compose --file " ++ dockerConfig ++ " --project-name foo up -d")
          delay 5000000
          injectToPath "~/.local/bin"
          ...
    }
  }
  ◊p{I also had fun writing a lot of helper/utility functions in Haskell, which was the main allure of doing this as opposed to writing some shell scripts!}
  ◊p{
    ◊m-code-haskell{
module Util where

ensureDirsExists :: [FilePath] -> Action ()
ensureDirsExists = liftIO . mapM_ (createDirectoryIfMissing False)

delay :: Int -> Action ()
delay n = liftIO (threadDelay n)

injectToPath :: String -> Action ()
injectToPath s = do
    path <- getEnv' "PATH"
    setEnv' "PATH" (path ++ ":" ++ s)

isDockerServiceRunning :: DockerService -> Project -> FilePath -> Action Bool
isDockerServiceRunning (DockerService serviceName) (Project project) dockerConfig = do
    (_, Just hOut, _, _) <- liftIO $ createProcess (
        proc "bash" [
            "-c"
          , "docker-compose --file " ++ dockerConfig ++ " --project-name " ++ project ++ " ps"]) { std_out = CreatePipe }
    output <- liftIO $ hGetContents hOut
    return $ length (filter (serviceName `isInfixOf`) . lines $ output) > 0
    ...
    }
  }
  ◊p{It was fun to write functions that would do the equivalent job of commands used in the shell. For example, ◊code{ln -sf foo/* .} for linking some development files; I ended up with the following}
  ◊p{
    ◊m-code-haskell{
createFileLinks :: FilePath -> FilePath -> IO ()
createFileLinks source destination = do
  files <- listDirectory source
  forM_ files $ \file -> do
    isFile <- doesFileExist (source </> file)
    isDir <- doesDirectoryExist (source </> file)
    when isFile $ do
      linkExists <- doesFileExist (destination </> file)
      unless linkExists $
        createFileLink (source </> file) (destination </> file)
    when isDir $ do
      createDirectoryIfMissing False (destination </> file)
      createFileLinks (source </> file) (destination </> file)
    }
  }
  ◊p{Isn't Haskell such a nice imperative language? :)}
}
◊m-back
