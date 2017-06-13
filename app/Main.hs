module Main where

import           ExPanda

main :: IO ()
main = do
  loginInfo <- queryLoginInfo
  Just jar <- exCookieJar loginInfo
  panda <- observePanda jar
  print panda
