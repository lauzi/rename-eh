{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Control.Lens hiding ((<~))
import Network.HTTP.Client (CookieJar, createCookieJar, destroyCookieJar)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wreq hiding ((:=), cookies)
import qualified Network.Wreq as Wreq (FormParam((:=)))
import qualified Network.Wreq.Session as Sess

import System.Console.Haskeline


cookies :: Lens' CookieJar [Cookie]
cookies f jar = createCookieJar <$> f (destroyCookieJar jar)


-- panda: "image/gif"
-- ex: "text/html; charset=UTF-8"


data LoginInfo = LoginInfo
  { username :: String
  , password :: String } deriving (Show, Eq)


main :: IO ()
main = do
  LoginInfo usr pwd <- runInputT defaultSettings $ do
    let
      keepGet :: InputT IO (Maybe a) -> InputT IO a
      keepGet m = m >>= \x ->
        case x of
          Just y -> return y
          Nothing -> keepGet m

    usr <- keepGet $ getInputLine "Account: "
    pwd <- keepGet $ getPassword (Just '*') "Password: "

    return $ LoginInfo usr pwd

  let
    loginUrl = "https://forums.e-hentai.org/index.php?act=Login&CODE=01"

    (<~) = (Wreq.:=) :: ByteString -> String -> FormParam
    formData =
      [ "CookieDate" <~ "1"
      , "b" <~ "d"
      , "bt" <~ "1-1"
      , "UserName" <~ usr
      , "PassWord" <~ pwd
      ]

  res <- post loginUrl formData

  let
    cookieJar = res ^. responseCookieJar
    newCookies = cookieJar & cookies . traverse . cookieDomain .~ "exhentai.org"

  Sess.withSessionControl (Just newCookies) tlsManagerSettings $ \sess -> do
    exhentai <- Sess.get sess "https://exhentai.org/"

    let contextType = exhentai ^. responseHeader "content-type"
    putStrLn $ if contextType == "image/gif" then "Sad panda" else "Glad panda"
