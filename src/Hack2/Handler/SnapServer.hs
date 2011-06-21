{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hack2.Handler.SnapServer 
(

--   run
-- , runWithConfig
-- , ServerConfig(..)
-- , hackAppToWaiApp

) where

import Prelude ()
import Air.Env hiding (def, Default)

import qualified Network.Wai as Wai
import Hack2
import Data.Default (def, Default)
import qualified Data.CaseInsensitive as CaseInsensitive
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Data.Enumerator (Enumerator, Iteratee (..), ($$), joinI, run_, Enumeratee, Step, (=$), ($=))
import qualified Data.Enumerator.List as EL
import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)

import qualified Safe as Safe

import qualified Snap.Types as Snap
import qualified Snap.Internal.Http.Types as SnapInternal

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Map (toAscList, fromAscList)


{-

{  requestMethod  :: RequestMethod
,  scriptName     :: ByteString
,  pathInfo       :: ByteString
,  queryString    :: ByteString
,  serverName     :: ByteString
,  serverPort     :: Int
,  httpHeaders    :: [(ByteString, ByteString)]
,  hackVersion    :: (Int, Int, Int)
,  hackUrlScheme  :: HackUrlScheme
,  hackInput      :: HackEnumerator
,  hackErrors     :: HackErrors
,  hackHeaders    :: [(ByteString, ByteString)]

-}


requestToEnv :: Snap.Request -> Env
requestToEnv request = def
  {
    requestMethod  = request.Snap.rqMethod.snapMethodToHackMethod
  -- , scriptName     = request.SnapInternal.rqSnapletPath
  , pathInfo       = request.Snap.rqPathInfo
  , queryString    = request.Snap.rqQueryString -- .B.dropWhile (is '?')
  , serverName     = request.Snap.rqServerName
  , serverPort     = request.Snap.rqServerPort
  , httpHeaders    = request.SnapInternal.rqHeaders.toAscList.map_snd (listToMaybe > fromMaybe B.empty) .map caseInsensitiveHeaderToHeader
  , hackUrlScheme  = if request.Snap.rqIsSecure then HTTPS else HTTP
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
    . (\r -> r { SnapInternal.rspHeaders = response.headers.map headerToCaseInsensitiveHeader. map_snd return .fromAscList })
    . Snap.setResponseBody (response.body.unHackEnumerator $= EL.map fromByteString)


-- ($=) :: Monad m
--      => Enumerator ao m (Step ai m b)
--      -> Enumeratee ao ai m b
--      -> Enumerator ai m b
-- ($=) = joinE

{-
hackAppToWaiApp :: Application -> Wai.Application
hackAppToWaiApp app request = do
   response <- io - app - requestToEnv request
   
   let wai_response_enumerator = hackResponseToWaiResponseEnumerator response 
   
   return - Wai.ResponseEnumerator wai_response_enumerator
   
  
  

hackResponseToWaiResponseEnumerator :: (forall a. Response -> Wai.ResponseEnumerator a)
hackResponseToWaiResponseEnumerator response f = 
  let s = response.status.statusToStatusHeader
      h = response.headers.map headerToCaseInsensitiveHeader
  
      -- wai response enumerator expect the callback (iteratee) to acts on builder.
      -- type ResponseEnumerator a =
      --  (H.Status -> H.ResponseHeaders -> Iteratee Builder IO a) -> IO a
  
      server_iteratee :: Iteratee Builder IO a
      server_iteratee = f s h

      
      -- in Builder, fromByteString :: S.ByteString -> Builder
      -- in Enumerator.List, map :: Monad m => (ao -> ai)
      --  -> Enumeratee ao ai m b
      
      -- type Enumeratee ao ai m b = Step ai m b -> Iteratee ao m (Step ai m b)
      -- builder_enumeratee :: Enumeratee ByteString Builder IO a
      builder_enumeratee :: Step Builder IO a -> Iteratee ByteString IO (Step Builder IO a)
      builder_enumeratee = EL.map fromByteString
      
      
      flattened_server_iteratee :: Iteratee ByteString IO a
      flattened_server_iteratee = joinI bytestring_to_builder_layered_iteratee
      
      
      flattened_server_iteratee :: Iteratee ByteString IO a
      flattened_server_iteratee =  builder_enumeratee =$ server_iteratee
      
      final_iteratee_taking_input_from_hack_enumerator :: Iteratee ByteString IO a
      final_iteratee_taking_input_from_hack_enumerator = response.body.unHackEnumerator $$ flattened_server_iteratee
      
      
  in
  run_ final_iteratee_taking_input_from_hack_enumerator


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


runWithConfig :: ServerConfig -> Application -> IO ()
runWithConfig config app = Warp.runSettings
  Warp.defaultSettings {Warp.settingsPort = config.port}
  (hackAppToWaiApp app)

run :: Application -> IO ()
run = runWithConfig def

-}
