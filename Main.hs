{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow (first)
import           Control.Exception (handleJust)
import           Control.Monad (guard, join, when, (<=<))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as T.IO
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.HTTP.Media as MT
import           Network.HTTP.Types.Header (hAccept, hContentType, hIfModifiedSince, hLastModified)
import           Network.HTTP.Types.Status (ok200, notModified304, badRequest400, notFound404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.FastCGI as Wai
import qualified System.Posix.ByteString as Posix
import           System.Posix.FilePath (takeExtension)
import           System.IO (hPutStrLn, stderr)
import qualified System.IO.Error as IOE
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import qualified Text.Pandoc as Pandoc

import Waimwork.HTTP
import Waimwork.Result

unRawFilePath :: Posix.RawFilePath -> FilePath
unRawFilePath = BSC.unpack -- XXX unsafe encoding

pandoc :: Posix.RawFilePath -> String -> IO (Either Pandoc.PandocError Html.Html)
pandoc path fmt = Pandoc.runIO $ do
  doc <- case Pandoc.getReader fmt of
    Left e -> throwError $ Pandoc.PandocParseError $ "unknown format: " ++ e
    Right (Pandoc.TextReader f, e) -> liftIO (T.IO.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
    Right (Pandoc.ByteStringReader f, e) -> liftIO (BSL.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
  Pandoc.writeHtml5 Pandoc.def doc

outputType :: MT.MediaType
outputType = "text" MT.// "html" MT./: ("charset","utf-8")

fileType :: BS.ByteString -> (Maybe String, MT.MediaType)
fileType ".md" = (Just "markdown", "text" MT.// "markdown")
fileType _ = (Nothing, "application" MT.// "octet-stream")

app :: Wai.Request -> IO Wai.Response
app req = do
  filename <- maybe (result $ Wai.responseLBS badRequest400 [] "") return $ header "SCRIPT_FILENAME"
  stat <- handleJust (guard . IOE.isDoesNotExistError) (\() -> result $ Wai.responseLBS notFound404 [] "") $
    Posix.getFileStatus filename
  let modtime = posixSecondsToUTCTime $ Posix.modificationTimeHiRes stat
  when (any (modtime <=) $ parseHTTPDate =<< header hIfModifiedSince) $
    result $ Wai.responseLBS notModified304 [] ""
  let (pt, mt) =
        first (>>= \pt' -> do
          accept <- header hAccept
          guard $ all (`notElem` [Nothing, Just "1"]) $ query "raw"
          join $ MT.mapAcceptMedia [(mt, Nothing), (outputType, Just pt')] accept)
        $ fileType $ takeExtension filename
  html <- maybe
    (return Nothing)
    (either
      (\e -> Nothing <$ hPutStrLn stderr (show filename ++ ": " ++ show e))
      (return . Just) <=< pandoc filename)
    pt
  let headers ct =
        [ (hContentType, MT.renderHeader ct)
        , (hLastModified, formatHTTPDate modtime)
        ]
  return $ maybe
    (Wai.responseFile ok200 (headers mt) (unRawFilePath filename) Nothing)
    (Wai.responseBuilder ok200 (headers outputType) . Html.renderHtmlBuilder)
    html
  where
  query v = lookup v $ Wai.queryString req
  header n = lookup n $ Wai.requestHeaders req

main :: IO ()
main = Wai.run $ resultApplication app
