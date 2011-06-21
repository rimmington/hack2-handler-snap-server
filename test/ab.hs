{-# LANGUAGE OverloadedStrings #-}

import Hack2
import Hack2.Contrib.Response (set_body_bytestring)
import qualified Hack2.Handler.SnapServer as SnapServer
import qualified Hack2.Handler.HappstackServer as HappstackServer

import Data.Default (def)
import Data.ByteString.Lazy.Char8 (pack)

import Air.Env hiding (def)
import Air.Extra
import Prelude ()
import Control.Monad (forever)

import Hack2.Contrib.Middleware.SimpleAccessLogger

app :: Application
app = simple_access_logger Nothing - \env -> 
  return $ 
    set_body_bytestring (pack - show env + "\n") -
      Response 200 [ ("Content-Type", "text/plain") ] def

main = do
  fork - do
    putStrLn - "snap server started at port 3001"
    SnapServer.runWithConfig def {SnapServer.port = 3001} app
  
  sleep 1
  
  fork - do
    putStrLn - "happstack started at port 3000"
    HappstackServer.run app
  
  
  forever - do
    sleep 1