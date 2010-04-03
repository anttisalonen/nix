module Config
where

import System.Console.GetOpt

import Global
import Common

configOptions :: [(OptDescr (ConfigOptions -> ConfigOptions))]
configOptions = [
    Option ['n'] ["name"] (ReqArg setConfigName "name") "your name"
  ]

data ConfigOptions = ConfigOptions { configName :: String }

setConfigName :: String -> ConfigOptions -> ConfigOptions
setConfigName s c = c{configName = s}

defaultConfigOptions = ConfigOptions ""

handleConfig args = do
  (opts, _) <- doArgs configOptions defaultConfigOptions [(\o -> not (null (configName o)))] "config" args False
  putStrLn $ "Your name: " ++ (configName opts)
  setGlobalName (configName opts)


