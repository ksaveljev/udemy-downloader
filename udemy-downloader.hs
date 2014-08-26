{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
import Network.HTTP.Types
import Text.HTML.DOM (parseLBS)
import Text.XML.Cursor
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Aeson as A
import qualified Course
import qualified Curriculum

getLoginFormUrl :: String
getLoginFormUrl = "https://www.udemy.com/join/login-popup"

loginUrl :: String
loginUrl = "https://www.udemy.com/join/login-submit"

findCsrfToken :: LC8.ByteString -> String
findCsrfToken page =
  let cursor = fromDocument $ parseLBS page
  in T.unpack $ head $ attribute "value" (head $ cursor $// (element "input" >=> attributeIs "name" "csrf"))

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
  in T.unpack $ head $ attribute "data-courseId" (head $ cursor $// (element "div" >=> hasAttribute "data-courseId"))

getCourseId :: [Cookie] -> String -> IO String
getCourseId cookies courseUrl = do
  request <- parseUrl courseUrl
  response <- withManager $ httpLbs $ request { cookieJar = Just $ createCookieJar cookies }
  return (findCourseId $ responseBody response)

getCourseInfo :: [Cookie] -> String -> IO (Maybe Course.Course)
getCourseInfo cookies courseId = do
  request <- parseUrl ("https://www.udemy.com/api-1.1/courses/" ++ courseId)
  response <- withManager $ httpLbs $ request { cookieJar = Just $ createCookieJar cookies }
  return (A.decode $ responseBody response)

getCourseCurriculum :: [Cookie] -> String -> IO (Maybe Curriculum.Curriculum)
getCourseCurriculum cookies courseId = do
  request <- parseUrl ("https://www.udemy.com/api-1.1/courses/" ++ courseId ++ "/curriculum") -- ?fields[lecture]=@all&fields[asset]=@all")
  let queryParams = [("fields[lecture]", Just "@all"), ("fields[asset]", Just "@all")]
  let request' = setQueryString queryParams request
  response <- withManager $ httpLbs $ request' { cookieJar = Just $ createCookieJar cookies }
  return (A.decode $ responseBody response)

main :: IO()
main = do
  cookies <- signIn "ft2000@mail.ru" "trouble" -- you need to supply correct username/password here
  case cookies of
    Just cookies' -> do
      putStrLn "Authenticated!"
      courseId <- getCourseId cookies' "https://www.udemy.com/official-udemy-instructor-course/" -- you have to take this course
      courseInfo <- getCourseInfo cookies' courseId
      courseCurriculum <- getCourseCurriculum cookies' courseId
      print courseCurriculum
    Nothing -> putStrLn "Couldn't authenticate"
