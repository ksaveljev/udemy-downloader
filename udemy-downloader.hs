{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
import Network.HTTP.Types
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import qualified Data.Text
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8

getLoginFormUrl :: String
getLoginFormUrl = "https://www.udemy.com/join/login-popup"

loginUrl :: String
loginUrl = "https://www.udemy.com/join/login-submit"

findCsrfToken :: LC8.ByteString -> String
findCsrfToken page =
  let cursor = fromDocument $ parseLBS page
  in Data.Text.unpack $ head $ attribute "value" (head $ cursor $// (element "input" >=> attributeIs "name" "csrf"))

getCsrfToken :: IO (String, [Cookie])
getCsrfToken = do
  request <- parseUrl getLoginFormUrl
  response <- withManager $ httpLbs request
  return (findCsrfToken $ responseBody response, destroyCookieJar $ responseCookieJar response)

signIn :: String -> String -> IO (Maybe [Cookie])
signIn username password = do
  (csrfToken, initialCookies) <- getCsrfToken
  request <- parseUrl loginUrl
  response <- withManager $ httpLbs $ configureLoginRequest request initialCookies csrfToken
  case filter (\x -> cookie_name x == "access_token") $ destroyCookieJar $ responseCookieJar response of
    [x] -> return $ Just $ destroyCookieJar $ responseCookieJar response
    _ -> return Nothing

  where
    formData :: String -> [(C8.ByteString, C8.ByteString)]
    formData csrfToken = [("isSubmitted", "1"), ("email", C8.pack username), ("password", C8.pack password), ("displayType", "json"), ("csrf", C8.pack csrfToken)]

    configureLoginRequest :: Request -> [Cookie] -> String -> Request
    configureLoginRequest request initialCookies csrfToken =
      urlEncodedBody (formData csrfToken) $
      request {
        method = methodPost,
        cookieJar = Just $ createCookieJar initialCookies
      }

findCourseId :: LC8.ByteString -> String
findCourseId page =
  let cursor = fromDocument $ parseLBS page
  in Data.Text.unpack $ head $ attribute "data-courseId" (head $ cursor $// (element "div" >=> hasAttribute "data-courseId"))

getCourseId :: [Cookie] -> String -> IO String
getCourseId cookies courseUrl = do
  request <- parseUrl courseUrl
  response <- withManager $ httpLbs $ configureRequest request cookies
  return (findCourseId $ responseBody response)
  where
    configureRequest :: Request -> [Cookie] -> Request
    configureRequest request requestCookies =
      request {
        cookieJar = Just $ createCookieJar requestCookies
      }

main :: IO()
main = do
  cookies <- signIn "aa@bb.cc" "pass" -- you need to supply correct username/password here
  case cookies of
    Just cookies' -> do
      putStrLn "GOOD"
      courseId <- getCourseId cookies' "https://www.udemy.com/official-udemy-instructor-course/" -- you have to take this course
      print courseId
    Nothing -> putStrLn "Couldn't authenticate"
