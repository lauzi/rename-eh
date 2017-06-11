import Control.Monad (when, unless, forM)
import System.Exit (exitFailure)
import Text.Printf

import ParsePath


data Test = Test
  { testFilePath :: String
  , testAuthor :: String
  , testTitle :: String
  } deriving (Eq, Show)


tests :: [Test]
tests = 
  [ Test
    "/Volumes/Randomos/[Bota Mochito] Gakkou de Ikou  ~WILD PARTY AFTER SCHOOL~"
    "Bota Mochito"
    "Gakkou de Ikou  ~WILD PARTY AFTER SCHOOL~"
  , Test
    "/Volumes/Randomos/[H-Manga] [Ekakibit] Chutorial  [Digital] - [えかきびと] チューとりある  [DL版] [Japanese]"
    "Ekakibit"
    "Chutorial"
  , Test
    "/Volumes/Randomos/[Nagare Ippon] Koukan Jyoken [Digital]"
    "Nagare Ippon"
    "Koukan Jyoken"
  , Test
    "/Volumes/Randomos/[Nanigawa Rui] Kyuuai Shoujo - Girl's hitting on me [Digital]"
    "Nanigawa Rui"
    "Kyuuai Shoujo - Girl's hitting on me"
  , Test
    "/Volumes/Randomos/[SAVAN] Toro Lover - Melty lover"
    "SAVAN"
    "Toro Lover - Melty lover"
  , Test
    "/Volumes/Randomos/[Zero no Mono] Wasurena 2"
    "Zero no Mono"
    "Wasurena 2"
  , Test
    "/Volumes/Randomos/(C83) [Jekyll and Hyde (Mizuki Makoto)] Izumi Chiaki no Engi suru Yuugure (WHITE ALBUM 2)"
    "Jekyll and Hyde (Mizuki Makoto)"
    "Izumi Chiaki no Engi suru Yuugure"
  , Test
    "/Volumes/Randomos/(C89) [Midorineko (Midori)] Mutsumigoto San (Touhou Project)"
    "Midorineko (Midori)"
    "Mutsumigoto San"
  , Test
    "/Volumes/Randomos/(C91) [Kodomo Beer (Yukibuster-Z)] Gojitsutan"
    "Kodomo Beer (Yukibuster-Z)"
    "Gojitsutan"
  , Test
    "/Volumes/Randomos/(COMIC1☆10) [Circle Ohigetan (Ohigetan)] Ohigebon-70 Teitoku no Ecchi  (Kantai Collection -KanColle-)"
    "Circle Ohigetan (Ohigetan)"
    "Ohigebon-70 Teitoku no Ecchi"
  , Test
    "/Volumes/Randomos/(COMIC1☆10) [Zetsubou Shiromuji (Shousan Bouzu)] Matomema  (Various)"
    "Zetsubou Shiromuji (Shousan Bouzu)"
    "Matomema"
  , Test
    "/Volumes/Randomos/(Jabjab Maidoari  2) [Romantic Sintai-Kensa (Nakamura B-ta)] Sorejaa Mofumofu Shichaimasu  (FLOWER KNIGHT GIRL)"
    "Romantic Sintai-Kensa (Nakamura B-ta)"
    "Sorejaa Mofumofu Shichaimasu"
  , Test
    "/Volumes/Randomos/(Reitaisai 11) [Midorineko (Midori)] Mutsumigoto (Touhou Project)"
    "Midorineko (Midori)"
    "Mutsumigoto"
  , Test
    "/Volumes/Randomos/[Circle Ohigetan (Ohigetan)] Ohigebon 64 [Highest Quality PNGs]"
    "Circle Ohigetan (Ohigetan)"
    "Ohigebon 64"
  , Test
    "/Volumes/Randomos/[Circle Ohigetan (Ohigetan)] Ohigebon 71-Onedori  Natsu da  Asedaku da  Muremurehokahoka oparty  Onee-chan Satsusei shimakuri natsuyasumi no jiyuu kenkyuu～"
    "Circle Ohigetan (Ohigetan)"
    "Ohigebon 71-Onedori  Natsu da  Asedaku da  Muremurehokahoka oparty  Onee-chan Satsusei shimakuri natsuyasumi no jiyuu kenkyuu～"
  , Test
    "/Volumes/Randomos/[Circle Ohigetan (Ohigetan)] Ohigebon 75-Christmas no hi ni okini no cosplayer-san ga ie ni yattekita"
    "Circle Ohigetan (Ohigetan)"
    "Ohigebon 75-Christmas no hi ni okini no cosplayer-san ga ie ni yattekita"
  , Test
    "/Volumes/Randomos/[Circle Ohigetan (Ohigetan)] Ohigebon-61 Hatsukoi no Eroin 1"
    "Circle Ohigetan (Ohigetan)"
    "Ohigebon-61 Hatsukoi no Eroin 1"
  , Test
    "/Volumes/Randomos/[Circle Ohigetan (Ohigetan)] Ohigebon-74 5-Onedori  C90 Omakebon [TEXTLESS (EroGPx)]"
    "Circle Ohigetan (Ohigetan)"
    "Ohigebon-74 5-Onedori  C90 Omakebon"
  , Test
    "/Volumes/Randomos/[Rokkaku Yasosuke] Ranran ♪ Onikumatsuri"
    "Rokkaku Yasosuke"
    "Ranran ♪ Onikumatsuri"
  , Test
    "/Volumes/Randomos/[Zero no Mono] Karada ni Kiite"
    "Zero no Mono"
    "Karada ni Kiite"
  , Test
    "/Volumes/Randomos/[Zero no Mono] Seiteki na Kanojo - She is Sexual"
    "Zero no Mono"
    "Seiteki na Kanojo - She is Sexual"
  ]


data Result = Success | Failure deriving Eq


compareResult :: Test -> Title -> IO Result
compareResult answer@(Test path author title) result@(Title author' title')
  | (Title author title) == result = return Success
  | otherwise = do
      printf "Testcase %s:\n" path
      when (author /= author') $ printf "  Author %s != %s\n" author author'
      when (title  /= title' ) $ printf "  Title  %s != %s\n" title  title'
      putStrLn ""
      return Failure


runTest :: Test -> IO Result
runTest answer@(Test path _ _) = 
  case parsePath path of
    Nothing -> do
      printf "Testcase %s:\n" path
      printf "  Parse Failure\n"
      putStrLn ""
      return Failure
    Just result -> compareResult answer result


main :: IO ()
main = do
  checks <- forM tests runTest
  unless (all (== Success) checks) exitFailure
