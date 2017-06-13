{-# LANGUAGE OverloadedStrings #-}

module ExPanda
  ( LoginInfo(..)
  , queryLoginInfo
  , exCookieJar
  , saveExJar
  , loadExJar
  , exSession
  , observePanda
  ) where

import           Control.Exception        (try)
import           Control.Lens             hiding ((<~))
import           Data.ByteString          (ByteString)
import           Network.HTTP.Client      (CookieJar, createCookieJar,
                                           destroyCookieJar)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.Wreq             hiding ((:=), cookies)
import qualified Network.Wreq             as Wreq (FormParam ((:=)))
import qualified Network.Wreq.Session     as S
import           Text.Read                (readMaybe)

import           System.Console.Haskeline


cookies :: Lens' CookieJar [Cookie]
cookies f jar = createCookieJar <$> f (destroyCookieJar jar)


data LoginInfo = LoginInfo
  { username :: String
  , password :: String } deriving (Show, Read, Eq)


queryLoginInfo :: IO LoginInfo
queryLoginInfo = runInputT defaultSettings $ do
  let
    keepGet :: InputT IO (Maybe a) -> InputT IO a
    keepGet m = m >>= \x ->
      case x of
        Just y  -> return y
        Nothing -> keepGet m

  usr <- keepGet $ getInputLine "Account: "
  pwd <- keepGet $ getPassword (Just '*') "Password: "

  return $ LoginInfo usr pwd


newtype ExJar = ExJar CookieJar deriving (Show, Read)

exJar :: CookieJar -> ExJar
exJar = ExJar . (cookies . traverse . cookieDomain .~ "exhentai.org")

saveExJar :: FilePath -> ExJar -> IO ()
saveExJar path jar = writeFile path (show jar)

loadExJar :: FilePath -> IO (Maybe ExJar)
loadExJar path = do
  eitherContent <- try $ readFile path :: IO (Either IOError String)
  case eitherContent of
    Left _        -> return Nothing
    Right content -> return $ readMaybe content


exCookieJar :: LoginInfo -> IO (Maybe ExJar)
exCookieJar (LoginInfo usr pwd) = do
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

  let jar = res ^. responseCookieJar

  return $ if jar ^. cookies . to null
    then Nothing
    else Just $ exJar jar


data Panda = GladPanda | SadPanda deriving (Show, Eq)

instance Monoid Panda where
  mempty = SadPanda

  mappend SadPanda SadPanda = SadPanda
  mappend _ _               = GladPanda


panda :: Response body -> Panda
panda = mconcat . map headerToPanda . (^.. responseHeader "content-type")
  where
    headerToPanda "image/gif" = SadPanda
    headerToPanda _           = GladPanda


exSession :: ExJar -> (S.Session -> IO a) -> IO a
exSession (ExJar jar) = S.withSessionControl (Just jar) tlsManagerSettings


observePanda :: ExJar -> IO Panda
observePanda jar = exSession jar $ \sess -> do
  exhentai <- S.get sess "https://exhentai.org/"

  return $ panda exhentai
