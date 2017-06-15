{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ExPanda
  (
    -- * Logging into ExHentai
    LoginInfo(..)
  , promptLoginInfo
  , exCookieJar
  , ExJar
  , saveExJar
  , loadExJar
  , exSession
  , happyPanda

    -- * Performing a search
  , ResultPage(..)
  , search
  , nextSearchPage
  , EhId(..)

    -- * Querying galleries
  , EhApiRequest(..)
  , queryApi
  , fetchGalleryPage
  , EhApiResponse(..)
  , EhGallery(..)
  , Bytes(..)

    -- * For debugging
  , searchHtml
  , parseGalleryPage
  ) where

import           Control.Exception          (try)
import           Data.Char                  (isDigit)
import           Data.List                  (intercalate)

import           Control.Lens               hiding (deep, (.=), (<~))
import           Data.Aeson
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Text                  (pack, unpack)
import           Data.Time.Calendar         (fromGregorianValid)
import           Data.Time.Clock            (UTCTime (..))
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Network.HTTP.Client
    (CookieJar, createCookieJar, destroyCookieJar)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.Wreq               hiding ((:=), cookies)
import qualified Network.Wreq               as Wreq (FormParam ((:=)))
import qualified Network.Wreq.Session       as S
import           System.Console.Haskeline
import           Text.HandsomeSoup          (css, parseHtml, (!))
import           Text.Read                  (readMaybe)
import           Text.Regex.Applicative
import           Text.XML.HXT.Core          hiding (when)


plzDon't :: String -> Maybe a -> a
plzDon't errMsg Nothing = error errMsg
plzDon't _ (Just x)     = x


cookies :: Lens' CookieJar [Cookie]
cookies f jar = createCookieJar <$> f (destroyCookieJar jar)


data LoginInfo = LoginInfo
  { username :: !String
  , password :: !String
  } deriving (Show, Read, Eq)


-- | Asks for login info. Doesn't really care if it's valid or not.
promptLoginInfo :: IO LoginInfo
promptLoginInfo = runInputT defaultSettings $ do
  let
    keepGet :: InputT IO (Maybe a) -> InputT IO a
    keepGet m = m >>= \x ->
      case x of
        Just y  -> return y
        Nothing -> keepGet m

  usr <- keepGet $ getInputLine "Account: "
  pwd <- keepGet $ getPassword (Just '*') "Password: "

  return $ LoginInfo usr pwd


-- | A 'CookieJar' that is /supposed/ to be able to view exhentai.
--
-- Its main use is to run an 'exSession'.
newtype ExJar = ExJar CookieJar deriving (Show, Read)

exJar :: CookieJar -> ExJar
exJar = ExJar . (cookies . traverse . cookieDomain .~ "exhentai.org")

saveExJar :: FilePath -> ExJar -> IO ()
saveExJar path jar = writeFile path (show jar)

loadExJar :: FilePath -> IO (Maybe ExJar)
loadExJar path = do
  eitherContent <- try $ readFile path :: IO (Either IOError String)
  return $ case eitherContent of
    Left _        -> Nothing
    Right content -> readMaybe content


-- | Tries to login to exhentai.
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


-- maybe put the session into a reader so we can hide the session
-- do we/i need to though?
exSession :: ExJar -> (S.Session -> IO a) -> IO a
exSession (ExJar jar) = S.withSessionControl (Just jar) tlsManagerSettings


panda :: Response body -> Bool
panda = all (not . isGif) . (^.. responseHeader "content-type")
  where isGif = (==) "image/gif"

-- | Checks if the exhentai homepage is accessible.
happyPanda :: ExJar -> IO Bool
happyPanda jar = exSession jar $ \sess ->
  panda <$> S.get sess "https://exhentai.org/"


data EhId = EhId
  { gid   :: !Int
  , token :: !String
  } deriving (Show, Eq)

instance ToJSON EhId where
  toJSON (EhId gid token) = toJSON [toJSON gid, toJSON token]

readEhId :: String -> EhId
readEhId s = plzDon't ("Fail to parse eh url " ++ s) $ s =~ ehIdParser

ehIdParser :: RE Char EhId
ehIdParser = EhId <$> (shit *> gid) <*> slashed notSlash <* notSlash
  where
    shit = some anySym <* string ".org/g/"
    gid = read <$> some (psym isDigit)
    slashed p = sym '/' *> p <* sym '/'
    notSlash = many (psym (/= '/'))

ehIdUrl :: EhId -> String
ehIdUrl (EhId gid token) =
  intercalate "/" ["https://ehentai.org/g", show gid, token, ""]


data EhCategory = Doujinshi
                | Manga
                | ArtistCg
                | GameCg
                | Western
                | NonH
                | ImageSet
                | Cosplay
                | AsainPorn
                | Misc
                deriving (Show, Eq)

instance FromJSON EhCategory where
  parseJSON (String s) = return . readCategoryJson . unpack $ s
  parseJSON _          = fail "EhCategory JSON not a string"

readCategory, readCategoryJson :: String -> EhCategory
readCategory "doujinshi" = Doujinshi
readCategory "manga"     = Manga
readCategory "artistcg"  = ArtistCg
readCategory "gamecg"    = GameCg
readCategory "western"   = Western
readCategory "non-h"     = NonH
readCategory "imageset"  = ImageSet
readCategory "cosplay"   = Cosplay
readCategory "asainporn" = AsainPorn
readCategory _           = Misc

readCategoryJson "Doujinshi"      = Doujinshi
readCategoryJson "Manga"          = Manga
readCategoryJson "Artist CG Sets" = ArtistCg
readCategoryJson "Game CG Sets"   = GameCg
readCategoryJson "Western"        = Western
readCategoryJson "Non-H"          = NonH
readCategoryJson "Image Sets"     = ImageSet
readCategoryJson "Cosplay"        = Cosplay
readCategoryJson "Asain Porn"     = AsainPorn
readCategoryJson _                = Misc


data ResultPage = ResultPage
  { matches     :: ![(String, EhId)] -- ^ A list of @(title, id)@s
  , nextPageUrl :: !(Maybe String)   -- ^ The next search page
  } deriving (Show, Eq)

searchOpts :: String -> Options
searchOpts wat = defaults & params .~ pars
  where
    setTo1 = [ "f_doujinshi"
             , "f_manga"
             , "f_artistcg"
             , "f_gamecg"
             , "f_non-h"
             , "f_imageset"
             , "f_cosplay"
             , "f_asainporn"
             , "f_misc"]
    setTo0 = ["f_western"]
    cats = map (\p -> (p, "1")) setTo1 ++ map (\p -> (p, "0")) setTo0
    pars = cats ++ [ ("f_search", pack wat)
                   , ("f_apply", "Apply Filter")
                   , ("inline_set", "dm_l")]

search :: S.Session -> String -> IO ResultPage
search sess wat = do
  r <- S.getWith (searchOpts wat) sess "https://exhentai.org/"
  searchHtml . Char8.unpack $ r ^. responseBody

nextSearchPage :: S.Session -> ResultPage -> IO (Maybe ResultPage)
nextSearchPage _ (ResultPage _ Nothing) = return Nothing
nextSearchPage sess (ResultPage _ (Just url)) = do
  r <- S.get sess url
  page <- searchHtml . Char8.unpack $ r ^. responseBody
  return $ Just page


-- | A \"pure\" version of 'search' that take the webpage as input.
-- The 'IO' is only used to parse the HTML.
searchHtml :: String -> IO ResultPage
searchHtml html = do
  let
    doc = parseHtml html

    getHref = getAttrValue "href"
    getEhId = getHref >>> arr readEhId
    getMatches = css ".it5 a" >>> deep getText &&& getEhId

    getNothing = arr (const Nothing)
    getLink = deep (hasAttr "href") >>> getHref >>> arr Just
    maybeGetLink = ifA (hasAttr "class") getNothing getLink
    getMaybeLinks = css ".ptb td a" >>> maybeGetLink

  matches   <- runX $ doc >>> getMatches
  nextPages <- runX $ doc >>> getMaybeLinks

  let nextPageUrl = last nextPages
  return ResultPage {..}


newtype Bytes = Bytes Int deriving (Show, Eq)


data EhGallery = EhGallery
  { englishTitle  :: !String
  , japaneseTitle :: !String
  , galleryId     :: !EhId
  , category      :: !EhCategory
  , posted        :: !UTCTime
  , fileSize      :: !Bytes
  , pages         :: !Int
  , rating        :: !Double
  , tags          :: ![String]
  } deriving Show

epochToUtc :: Int -> UTCTime
epochToUtc = posixSecondsToUTCTime . realToFrac

instance FromJSON EhGallery where
  parseJSON = withObject "gallery" $ \v -> do
    englishTitle  <- v .: "title"
    japaneseTitle <- v .: "title_jpn"
    galleryId     <- EhId <$> v .: "gid" <*> v .: "token"
    category      <- v .: "category"
    posted        <- epochToUtc . read <$> v .: "posted"
    fileSize      <- Bytes <$> v .: "filesize"
    pages         <- read <$> v .: "filecount"
    rating        <- read <$> v .: "rating"
    tags          <- v .: "tags"
    return EhGallery {..}


newtype EhApiRequest = EhApiRequest [EhId] deriving Show

instance ToJSON EhApiRequest where
  toJSON (EhApiRequest ids) =
    object [ "method"    .= ("gdata" :: String)
           , "namespace" .= (1 :: Int)
           , "gidlist"   .= ids
           ]


newtype EhApiResponse = EhApiResponse [EhGallery] deriving Show

instance FromJSON EhApiResponse where
  parseJSON = withObject "response" $ \v ->
    EhApiResponse <$> v .: "gmetadata"


-- | The [API](https://ehwiki.org/wiki/API) allows
-- at most 25 entries per request, and usually
-- 5 seconds CD between 4 sequential requests.
--
-- The responsed gallaries are not guaranteed to be in order.
queryApi :: EhApiRequest -> IO [EhGallery]
queryApi req = do
  -- The reason of not using @asJSON =<< post ...@ is that
  -- the response content-type is fucking text/html; charset=UTF-8
  response <- post "https://api.e-hentai.org/api.php" (toJSON req)
  return $ case decode' (response ^. responseBody) of
    Nothing                 -> []
    Just (EhApiResponse gs) -> gs


-- | Info parsed from the gallery page are slightly less accurate
-- than the API, so don't use this unless hopeless or bored.
fetchGalleryPage :: S.Session -> EhId -> IO EhGallery
fetchGalleryPage sess ehid = do
  response <- S.get sess (ehIdUrl ehid)
  parseGalleryPage . Char8.unpack $ response ^. responseBody

parseDate :: String -> Maybe UTCTime
parseDate str = str =~ parser
  where
    secs h m = realToFrac $ (h * 60 + m) * 60
    tryRun = plzDon't "Can't parse date"
    toDay y m d = tryRun $ fromGregorianValid y m d
    makeTime y mon d h min' = UTCTime (toDay y mon d) (secs h min')

    -- we need 2 instances of the same shit cuz they have different types
    int = read <$> some (psym isDigit)
    integer' = read <$> some (psym isDigit) <* anySym
    int' = int <* anySym
    parser = makeTime <$> integer' <*> int' <*> int' <*> int' <*> int

parseSize :: String -> Bytes
parseSize str = Bytes . floor . (* mult unit) $ x
  where
    mult "GB" = 1024 ** 3
    mult "MB" = 1024 ** 2
    mult "KB" = 1024
    mult "B"  = 1
    mult _    = error "WTF"
    [xStr, unit] = words str
    x = read xStr :: Double

-- | A \"pure\" version of 'fetchGalleryPage'.
parseGalleryPage :: String -> IO EhGallery
parseGalleryPage html = do
  let
    doc = parseHtml html

    underscoreToSpace '_' = ' '
    underscoreToSpace c   = c
    fmtTag = map underscoreToSpace . drop 3

    getId = getAttrValue "href" >>> arr readEhId
    getCategory = getAttrValue "alt" >>> arr readCategory
    getRating = getText >>> arr (read . drop 9)
    getTags = (css ".gt" <+> css ".gtl") ! "id" >>> arr fmtTag

  [englishTitle]  <- runX $ doc //> css "#gn" //> getText
  [japaneseTitle] <- runX $ doc //> css "#gj" //> getText
  (galleryId : _) <- runX $ doc //> css ".ptds a" >>> getId
  [category]      <- runX $ doc //> css "#gdc img" >>> getCategory
  [rating]        <- runX $ doc //> css "#rating_label" //> getRating
  tags            <- runX $ doc //> css "#taglist" >>> getTags
  tableShit       <- runX $ doc //> css ".gdt2" //> getText

  let
    [dateS, _, _, _, sizeS, pagesS, _] = tableShit
    Just posted = parseDate dateS
    fileSize = parseSize sizeS
    pages = read . head . words $ pagesS

  return EhGallery {..}
