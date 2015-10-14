module Hails.Graphics.UI.Gtk.Simplify.UpdateCheck where

-- External Imports
import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Data.Maybe
import Graphics.UI.View
import Graphics.UI.Gtk.GtkView
-- import Graphics.UI.Gtk.GenericView
import Hails.MVC.GenericCombinedEnvironment
import Network.HTTP
import Network.URI

-- Internal Imports
import Data.ExtraVersion
import Data.ReactiveValue
import Hails.MVC.Model.ProtectedModel.UpdatableModel

installHandlers :: (GtkGUI a, UpdatableBasicModel b,
                    UpdateNotifiableEvent c, ReactiveValueActivatable d)
                => CEnv a b c
                -> (ViewElementAccessorIO (GtkView a) d)
                -> IO ()
installHandlers cenv mF = do
  let vw = view cenv
  mn <- mF vw
  defaultActivation mn `reactiveValueOnCanRead` onViewAsync vw (condition cenv)

condition :: (GtkGUI a, UpdatableBasicModel b, UpdateNotifiableEvent c)
          => CEnv a b c
          -> IO ()
condition cenv = void $
  forkIO $ E.handle (constantlyHandle (return ())) $ do
    let pm = model cenv
    url <- getUpdateURI pm
    v   <- (netRead url) :: IO (Either String Version)
    case v of
      (Left  _) -> return ()
      (Right s) -> setMaxVersionAvail pm s

netRead :: Read a => String -> IO (Either String a)
netRead url = do
  v <- downloadURL url
  case v of
   (Left s)  -> return (Left s)
   (Right s) -> E.handle (constantlyHandle (return $ Left "Format error"))
                         (return $ Right $ read s)

-- FIXME: use anyway and create ignoringExceptions
constantlyHandle :: a -> E.SomeException -> a
constantlyHandle a _ = a

{- | Download a URL.  (Left errorMessage) if an error,
 - (Right doc) if success.
 -}
downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP request
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r ->
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               (3,_,_) -> -- A HTTP redirect
                 case findHeader HdrLocation r of
                   Nothing -> return $ Left (show r)
                   Just url' -> downloadURL url'
               _ -> return $ Left (show r)
    where request = Request {rqURI = uri,
                             rqMethod = GET,
                             rqHeaders = [],
                             rqBody = ""}
          uri = fromJust $ parseURI url
