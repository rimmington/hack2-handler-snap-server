{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hack2.Handler.SnapServer 
(

  run
, runWithConfig
, runWithSnapServerConfig
, ServerConfig(..)
, hackAppToSnap

) where

import Prelude ()
import Air.Env hiding (def, Default)

import Hack2
import Data.Default (def, Default)
import qualified Data.CaseInsensitive as CaseInsensitive
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Data.Enumerator (Enumerator, Iteratee (..), ($$), joinI, run_, Enumeratee, Step, (=$), ($=))
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)

import qualified Snap.Core as Snap
import Snap.Types.Headers as Snap.Headers
import qualified Snap.Internal.Http.Types as SnapInternal

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Map (toAscList, fromAscList)

import Data.IORef (readIORef)
import qualified Snap.Http.Server as SnapServer

import System.Directory (createDirectory, doesDirectoryExist)
import Control.Monad (when)


-- backports from earlier hack2 utils

import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as Strict

import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL

import Data.Enumerator (run_, enumList, Enumerator, ($$))

fromEnumerator :: Monad m => Enumerator Strict.ByteString m Lazy.ByteString -> m Lazy.ByteString
fromEnumerator m = run_ - m $$ EB.consume

toEnumerator :: Monad m => Lazy.ByteString -> Enumerator Strict.ByteString m a
toEnumerator = enumList 1 < Lazy.toChunks

requestToEnv :: Snap.Request -> IO Env
requestToEnv request = do
  (Snap.SomeEnumerator some_enumerator) <- readIORef - request.SnapInternal.rqBody
  
  _requestBody <- fromEnumerator some_enumerator
  
  return - def
  
    {
      requestMethod  = request.Snap.rqMethod.snapMethodToHackMethod
    -- , scriptName     = request.SnapInternal.rqSnapletPath
    , pathInfo       = B.append "/" - request.Snap.rqPathInfo
    , queryString    = request.Snap.rqQueryString -- .B.dropWhile (is '?')
    , serverName     = request.Snap.rqServerName
    , serverPort     = request.Snap.rqServerPort
    , httpHeaders    = request.SnapInternal.rqHeaders.Snap.Headers.toList.map caseInsensitiveHeaderToHeader
    , hackUrlScheme  = if request.Snap.rqIsSecure then HTTPS else HTTP
    , hackInput      = _requestBody.l2s
    , hackHeaders    = 
      [
        ("RemoteHost", request.Snap.rqRemoteAddr)
      , ("RemotePort", request.Snap.rqRemotePort.show.pack)
      ]
    }
  

snapMethodToHackMethod :: Snap.Method -> RequestMethod
snapMethodToHackMethod Snap.GET     =     GET
snapMethodToHackMethod Snap.HEAD    =     HEAD
snapMethodToHackMethod Snap.POST    =     POST
snapMethodToHackMethod Snap.PUT     =     PUT
snapMethodToHackMethod Snap.DELETE  =     DELETE
snapMethodToHackMethod Snap.TRACE   =     TRACE
snapMethodToHackMethod Snap.OPTIONS =     OPTIONS
snapMethodToHackMethod Snap.CONNECT =     CONNECT


caseInsensitiveHeaderToHeader :: (CaseInsensitive.CI ByteString, ByteString) -> (ByteString, ByteString)
caseInsensitiveHeaderToHeader (x, y) = (x.CaseInsensitive.original, y) 

headerToCaseInsensitiveHeader ::  (ByteString, ByteString) -> (CaseInsensitive.CI ByteString, ByteString)
headerToCaseInsensitiveHeader (x, y) = (x.CaseInsensitive.mk, y) 

hackResponseToSnapResponse :: Response -> Snap.Response
hackResponseToSnapResponse response = 
  Snap.emptyResponse
    . Snap.setResponseCode (response.status)
    . (\r -> r { SnapInternal.rspHeaders = response.headers.map headerToCaseInsensitiveHeader.Snap.Headers.fromList })
    . Snap.setResponseBody (response.body.s2l.toEnumerator $= EL.map fromByteString)



-- ($=) :: Monad m
--      => Enumerator ao m (Step ai m b)
--      -> Enumeratee ao ai m b
--      -> Enumerator ai m b
-- ($=) = joinE


hackAppToSnap :: Application -> Snap.Snap ()
hackAppToSnap app = do
  request <- Snap.getRequest
  
  env <- io - requestToEnv request
  
  response <- io - app env
   
  let snap_response = hackResponseToSnapResponse response 
   
  Snap.putResponse snap_response

 

data ServerConfig = ServerConfig
  {
    port :: Int
  }
  deriving (Show, Eq)

instance Default ServerConfig where
  def = ServerConfig
    {
      port = 3000
    }


runWithSnapServerConfig :: SnapServer.Config Snap.Snap a -> Application -> IO ()
runWithSnapServerConfig snap_server_config app = do
  let snap = hackAppToSnap app :: Snap.Snap ()
  
  SnapServer.httpServe snap_server_config snap


runWithConfig :: ServerConfig -> Application -> IO ()
runWithConfig config app = do
  let snap_config = SnapServer.emptyConfig.SnapServer.setPort (config.port)
  
      snap_default_log_path = "log"
  
  log_directory_exist <- doesDirectoryExist snap_default_log_path
  when (not - log_directory_exist) -
    createDirectory snap_default_log_path
    
    
  runWithSnapServerConfig snap_config app

  
run :: Application -> IO ()
run = runWithConfig def

