{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow (first)
import           Control.Exception (bracket, handleJust)
import           Control.Monad ((<=<), forM_, guard, join, when)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Fixed (Milli)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T.IO
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Network.HTTP.Media as MT
import           Network.HTTP.Types.Header (hAccept, hContentType, hIfModifiedSince, hLastModified)
import           Network.HTTP.Types.Status (ok200, notModified304, badRequest400, notFound404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.FastCGI as Wai
import           Numeric (showFFloat)
import qualified System.Posix.ByteString as Posix
import           System.Posix.FilePath ((</>), takeExtension, splitFileName)
import           System.IO (hPutStrLn, stderr)
import qualified System.IO.Error as IOE
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Pandoc as Pandoc

import qualified Waimwork.Blaze as Html
import           Waimwork.HTTP (formatHTTPDate, parseHTTPDate)
import           Waimwork.Result (result, resultApplication)

unRawFilePath :: Posix.RawFilePath -> FilePath
unRawFilePath = BSC.unpack -- XXX unsafe encoding

statFile :: Posix.RawFilePath -> IO (Maybe Posix.FileStatus)
statFile f = handleJust (guard . IOE.isDoesNotExistError) (\() -> return Nothing) $
  Just <$> Posix.getFileStatus f

indexFile :: Posix.RawFilePath
indexFile = "index.auto"

readDirPlus :: Posix.RawFilePath -> IO [(Posix.RawFilePath, Posix.FileStatus)]
readDirPlus dir = bracket (Posix.openDirStream dir) Posix.closeDirStream rdp where
  rdp ds = loop id where
    loop l = do
      name <- Posix.readDirStream ds
      case BSC.uncons name of
        Nothing -> return $ l []
        Just ('.', _) -> loop l
        _ | name == indexFile -> loop l
        _ -> loop . maybe l ((l .) . (:) . (,) name) =<<
          statFile (dir </> name)

pandoc :: Posix.RawFilePath -> String -> IO (Either Pandoc.PandocError Html.Html)
pandoc path fmt = Pandoc.runIO $ do
  doc <- case Pandoc.getReader fmt of
    Left e -> throwError $ Pandoc.PandocParseError $ "unknown format: " ++ e
    Right (Pandoc.TextReader f, e) -> liftIO (T.IO.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
    Right (Pandoc.ByteStringReader f, e) -> liftIO (BSL.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
  Pandoc.writeHtml5 Pandoc.def doc

htmlType :: MT.MediaType
htmlType = "text" MT.// "html" MT./: ("charset","utf-8")

fileType :: BS.ByteString -> (Maybe String, MT.MediaType)
fileType ".md" = (Just "markdown", "text" MT.// "markdown")
fileType _ = (Nothing, "application" MT.// "octet-stream")

byteSize :: Integral a => a -> String
byteSize = choose " KMGTPE" . realToFrac where
  choose (_:p@(_:_)) x | x >= 1000 = choose p (x / 1024)
  choose (' ':_) x = sf 0 x "B"
  choose ~(c:_) x
    | x >= 1000 = sf 0 x [c,'B']
    | x >= 100  = sf 1 x [c,'B']
    | x >= 10   = sf 2 x [c,'B']
    | otherwise = sf 3 x [c,'B']
  sf :: Int -> Float -> ShowS
  sf = showFFloat . Just

autoIndex :: Posix.RawFilePath -> [(Posix.RawFilePath, Posix.FileStatus)] -> Html.Html
autoIndex path dir = H.docTypeHtml $ do
  H.head $ do
    H.title $ Html.byteString path
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.type_ "text/css"
      H.! HA.href (dtcdn <> "css")
    H.script
      H.! HA.type_ "text/javascript"
      H.! HA.src (dtcdn <> "js")
      $ mempty
    H.script
      H.! HA.type_ "text/javascript"
      $ "$(function(){$('#dir').DataTable({paging:false});})"
  H.body $ do
    H.table H.! HA.id "dir" $ do
      H.thead $ do
        H.tr $ do
          H.th "name"
          H.th "size"
          H.th "time"
      H.tbody $ do
        forM_ dir $ \(name, stat) -> do
          H.tr $ do
            H.td $ Html.byteString name
            H.td H.! Html.dataAttribute "order" (Html.stringValue $ show $ Posix.fileSize stat) $
              Html.string $ byteSize $ Posix.fileSize stat
            H.td H.! Html.dataAttribute "order" (Html.stringValue $ show $ (realToFrac $ Posix.modificationTimeHiRes stat :: Milli)) $
              Html.string $ show $ posixSecondsToUTCTime $ Posix.modificationTimeHiRes stat
  where
  dtcdn = "https://cdn.datatables.net/v/dt/jq-3.2.1/dt-1.10.16/datatables.min."

app :: Wai.Request -> IO Wai.Response
app req = do
  filename <- maybe (result $ Wai.responseLBS badRequest400 [] "") return $ header "SCRIPT_FILENAME"
  stat <- maybe (result $ Wai.responseLBS notFound404 [] "") return =<< statFile filename
  let (dirname, basename) = splitFileName filename
  if Posix.fileSize stat == 0 && basename == indexFile then
    Wai.responseBuilder ok200 [(hContentType, MT.renderHeader htmlType)] . Html.renderHtmlBuilder .
      autoIndex dirname <$> readDirPlus dirname
  else do
    let modtime = posixSecondsToUTCTime $ Posix.modificationTimeHiRes stat
    when (any (modtime <=) $ parseHTTPDate =<< header hIfModifiedSince) $
      result $ Wai.responseLBS notModified304 [] ""
    let (pt, mt) =
          first (>>= \pt' -> do
            accept <- header hAccept
            guard $ all (`notElem` [Nothing, Just "1"]) $ query "raw"
            join $ MT.mapAcceptMedia [(mt, Nothing), (htmlType, Just pt')] accept)
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
      (Wai.responseBuilder ok200 (headers htmlType) . Html.renderHtmlBuilder)
      html
  where
  query v = lookup v $ Wai.queryString req
  header n = lookup n $ Wai.requestHeaders req

main :: IO ()
main = Wai.run $ resultApplication app
