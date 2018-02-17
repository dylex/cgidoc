{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow (left)
import           Control.Exception (handleJust)
import           Control.Monad (guard, when)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text.IO as T.IO
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Network.HTTP.Types.Header (hContentType, hIfModifiedSince, hLastModified)
import           Network.HTTP.Types.Status (ok200, notModified304, badRequest400, notFound404, internalServerError500)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.FastCGI as Wai
import qualified System.Posix.ByteString as Posix
import           System.Posix.FilePath (takeExtension)
import qualified System.IO.Error as IOE
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import qualified Text.Pandoc as Pandoc

import Waimwork.HTTP
import Waimwork.Result

unRawFilePath :: Posix.RawFilePath -> FilePath
unRawFilePath = BSC.unpack -- XXX unsafe encoding

pandoc :: String -> Posix.RawFilePath -> IO (Either Pandoc.PandocError Html.Html)
pandoc fmt path = Pandoc.runIO $ do
  doc <- case Pandoc.getReader fmt of
    Left e -> throwError $ Pandoc.PandocParseError $ "unknown format: " ++ e
    Right (Pandoc.TextReader f, e) -> liftIO (T.IO.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
    Right (Pandoc.ByteStringReader f, e) -> liftIO (BSL.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
  Pandoc.writeHtml5 Pandoc.def doc

app :: Wai.Request -> IO Wai.Response
app req = do
  filename <- maybe (result $ Wai.responseLBS badRequest400 [] "") return $ header "SCRIPT_FILENAME"
  stat <- handleJust (guard . IOE.isDoesNotExistError) (\() -> result $ Wai.responseLBS notFound404 [] "") $
    Posix.getFileStatus filename
  let modtime = posixSecondsToUTCTime $ Posix.modificationTimeHiRes stat
  when (any (modtime <=) $ parseHTTPDate =<< header hIfModifiedSince) $
    result $ Wai.responseLBS notModified304 [] ""
  html <- case takeExtension filename of
    ".md" -> left show <$> pandoc "markdown" filename
    _ -> return $ Left "unknown file extension"
  return $ either
    (Wai.responseLBS internalServerError500 [] . BSLC.pack)
    (Wai.responseBuilder ok200
      [ (hContentType, "text/html;charset=utf-8")
      , (hLastModified, formatHTTPDate modtime)
      ] . Html.renderHtmlBuilder)
    html
  where
  header n = lookup n headers
  headers = Wai.requestHeaders req
  -- respond $ Wai.responseLBS ok200 [(hContentType,"text/plain")] $ BSLC.pack $ show req

main :: IO ()
main = Wai.run $ resultApplication app
