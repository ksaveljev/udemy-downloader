{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Data.List (find)
import System.Directory
import System.FilePath.Posix
import System.Environment (getArgs)
import Text.XML.Cursor
import Text.HTML.DOM (parseLBS)
import Control.Concurrent.ParallelIO
import Network.HTTP.Types
import Network.HTTP.Conduit
import Data.Conduit.Binary (sinkFile)
import qualified Data.Conduit as C
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import qualified Data.Aeson as A

import Course
import Curriculum
import Asset
import DownloadableContent
import EBookResource
import AudioResource
import VideoResource
import VideoMashupResource

-- | Given an HTML page we have multiple (if I am not mistaken) input fields with name "csrf"
-- which has the value we are interested in. Using html-conduit we find the input field and
-- grab the value for future use (needed for login)
findCsrfToken :: LC8.ByteString -> String
findCsrfToken page =
  let cursor = fromDocument $ parseLBS page
  in T.unpack $ head $ attribute "value" (head $ cursor $// (element "input" >=> attributeIs "name" "csrf"))

-- | Using http-conduit grab the login-popup page, search for csrf token on it, meanwhile also
-- collecting the initial Cookies that were set by server
getCsrfToken :: IO (String, [Cookie])
getCsrfToken = do
  request <- parseUrl "https://www.udemy.com/join/login-popup"
  response <- withManager $ httpLbs request
  return (findCsrfToken $ responseBody response, destroyCookieJar $ responseCookieJar response)

-- | Using http-conduit we send a POST request to server with our credentials and csrf token.
-- If everything is OK then there should be a cookie with name "access_token" set by server.
signIn :: String -> String -> IO (Maybe [Cookie])
signIn username password = do
  (csrfToken, initialCookies) <- getCsrfToken
  request <- parseUrl "https://www.udemy.com/join/login-submit"
  response <- withManager $ httpLbs $ configureLoginRequest request initialCookies csrfToken
  case find (\x -> cookie_name x == "access_token") $ destroyCookieJar $ responseCookieJar response of
    Just _ -> return $ Just $ destroyCookieJar $ responseCookieJar response
    Nothing -> return Nothing

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

-- | Given an HTML page we have multiple div elements with attribute name "data-courseId"
-- which has the value we are interested in. Using html-conduit we find the dev element and
-- grab the value for future use (needed for using Udemy API to collect course info)
findCourseId :: LC8.ByteString -> String
findCourseId page =
  let cursor = fromDocument $ parseLBS page
  in T.unpack $ head $ attribute "data-courseId" (head $ cursor $// (element "div" >=> hasAttribute "data-courseId"))

-- | Using http-conduit grab the course page, search for courseId on it
getCourseId :: [Cookie] -> String -> IO String
getCourseId cookies courseUrl = do
  request <- parseUrl courseUrl
  response <- withManager $ httpLbs $ request { cookieJar = Just $ createCookieJar cookies }
  return (findCourseId $ responseBody response)

-- | Get simple course info form Udemy API. Currently not used as we do not get anything
-- of value from the course info (except maybe course name)
getCourseInfo :: [Cookie] -> String -> IO (Maybe Course)
getCourseInfo cookies courseId = do
  request <- parseUrl ("https://www.udemy.com/api-1.1/courses/" ++ courseId)
  response <- withManager $ httpLbs $ request { cookieJar = Just $ createCookieJar cookies }
  return (A.decode $ responseBody response)

-- | Using Udemy API we grab course curriculum including all interesting information about
-- lectures and assets for each lecture. This is done via GET params fields[lecture]=@all
-- and fields[asset]=@all.
getCourseCurriculum :: [Cookie] -> String -> IO (Maybe Curriculum)
getCourseCurriculum cookies courseId = do
  request <- parseUrl ("https://www.udemy.com/api-1.1/courses/" ++ courseId ++ "/curriculum")
  let queryParams = [("fields[lecture]", Just "@all"), ("fields[asset]", Just "@all")]
  let request' = setQueryString queryParams request
  response <- withManager $ httpLbs $ request' { cookieJar = Just $ createCookieJar cookies }
  return (A.decode $ responseBody response)

-- | Given course Curriculum we go over all chapters/lectures/quizes and collect all the links
-- that we are interested in. Currently it is simplified to mostly just download major content
-- and ignore minor one (like slides and stuff like that)
collectDownloadableContent :: Maybe Curriculum -> [DownloadableContent]
collectDownloadableContent (Just curriculum) = processChapters curriculum
  where
    -- Given chapter index and chapter name we construct a name of the folder
    chapterToFolderName :: Content -> String
    chapterToFolderName (Chapter title objectIndex) = show objectIndex ++ "." ++ T.unpack title
    chapterToFolderName _ = fail "Only Chapter objects are allowed"

    -- Given lecture index and asset title we construct a name of the file to be written
    assetName :: Int -> T.Text -> String
    assetName lectureIndex assetTitle = show lectureIndex ++ "." ++ T.unpack assetTitle

    -- Depending on the asset type we collect the data we would like to download
    getAssetContent :: String -> Int -> Asset -> [DownloadableContent]
    getAssetContent _ _ (Article _ _) = [] -- currently we are not interested in this asset
    getAssetContent chapterName lectureIndex (EBook title (EBookResource _ url)) = [DownloadableContent chapterName (assetName lectureIndex title) url]
    getAssetContent chapterName lectureIndex (Audio title (AudioResource _ url)) = [DownloadableContent chapterName (assetName lectureIndex title) url]
    getAssetContent chapterName lectureIndex (Video title (VideoResource _ _ url)) = [DownloadableContent chapterName (assetName lectureIndex title) url]
    getAssetContent chapterName lectureIndex (VideoMashup title (VideoMashupResource [url] _ _ _)) = [DownloadableContent chapterName (assetName lectureIndex title) url]
    getAssetContent _ _ _ = fail "For some reason couldn't identify what to download!"

    -- Process all Lectures and Quizzes until the next chapter is met in the Curriculum
    processLecturesAndQuizzes :: String -> Curriculum -> [DownloadableContent]
    processLecturesAndQuizzes _ [] = []
    processLecturesAndQuizzes chapterName (courseContent : otherCourseContent) =
      case courseContent of
        (Lecture _ objectIndex _ asset) -> getAssetContent chapterName objectIndex asset ++ processLecturesAndQuizzes chapterName otherCourseContent
        (Quiz _) -> processLecturesAndQuizzes chapterName otherCourseContent
        (Chapter _ _) -> []

    -- For every Chapter in the Curriculum we want to create a folder for that chapter and download
    -- the assets from that Chapter to that folder
    processChapters :: Curriculum -> [DownloadableContent]
    processChapters [] = []
    processChapters (courseContent : otherCourseContent) =
      case courseContent of
        chapter@(Chapter _ _) -> processLecturesAndQuizzes (chapterToFolderName chapter) otherCourseContent ++ processChapters otherCourseContent
        _ -> processChapters otherCourseContent
collectDownloadableContent Nothing = fail "Nothing to download"

-- | Using parallel-io we download concurrently everything we have found in the course curriculum
downloadEverything :: [DownloadableContent] -> IO()
downloadEverything downloadableContent =
  let
    isSafe = not . (`elem` charactersBadForFs)
    charactersBadForFs = "/\\"
    download dc = do
      let file = filter isSafe $ fileName dc
      let folderName = folder dc
      let url = downloadUrl dc
      putStrLn $ "Downloading " ++ file
      createDirectoryIfMissing True folderName
      req <- parseUrl url
      withManager $ \manager -> do
        response <- http req manager
        responseBody response C.$$+- sinkFile $ folderName </> file
      return ()
  in do
    parallel_ $ map download downloadableContent
    stopGlobalPool

main :: IO()
main = do
  args <- getArgs
  let username = args !! 0 -- no validation for supplied input
  let courseUrl = args !! 1 -- no validation for supplied input
  putStr "Password: "
  hFlush stdout
  password <- getLine
  cookies <- signIn username password -- you need to supply correct username/password here
  case cookies of
    Just cookies' -> do
      putStrLn "Authenticated!"
      courseId <- getCourseId cookies' courseUrl
      putStrLn $ "Got CourseId: " ++ courseId
      courseInfo <- getCourseInfo cookies' courseId -- not used for anything right now
      courseCurriculum <- getCourseCurriculum cookies' courseId
      putStrLn "Got Course Curriculum"
      let downloadableContent = collectDownloadableContent courseCurriculum
      putStrLn "Let's Download!"
      downloadEverything downloadableContent
    Nothing -> putStrLn "Couldn't authenticate"
