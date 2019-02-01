{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import           Control.Exception (bracket, handleJust)
import           Control.Monad ((<=<), forM_, guard, join, when)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import           Data.Fixed (Milli)
import           Data.Monoid ((<>))
import qualified Data.Text.IO as T.IO
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
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
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Pandoc as Pandoc

import qualified Waimwork.Blaze as Html
import           Waimwork.HTTP (formatHTTPDate, parseHTTPDate)
import           Waimwork.Response (okResponse)
import           Waimwork.Result (result, resultApplication)

unRawFilePath :: Posix.RawFilePath -> FilePath
unRawFilePath = BSC.unpack -- XXX unsafe encoding

statFile :: Posix.RawFilePath -> IO (Maybe Posix.FileStatus)
statFile f = handleJust (guard . IOE.isDoesNotExistError) (\() -> return Nothing) $
  Just <$> Posix.getFileStatus f

indexFile :: Posix.RawFilePath
indexFile = "index.auto"

maxFiles :: Int
maxFiles = 1000

readDirPlus :: Posix.RawFilePath -> IO [(Posix.RawFilePath, Posix.FileStatus)]
readDirPlus dir = bracket (Posix.openDirStream dir) Posix.closeDirStream rdp where
  rdp ds = loop maxFiles id where
    loop 0 l = return $ l []
    loop n l = do
      name <- Posix.readDirStream ds
      case BSC.uncons name of
        Nothing -> return $ l []
        Just ('.', _) -> loop n l
        _ | name == indexFile -> loop n l
        _ -> loop (pred n) . maybe l ((l .) . (:) . (,) name) =<<
          statFile (dir </> name)

pandoc :: Posix.RawFilePath -> String -> IO (Either Pandoc.PandocError H.Html)
pandoc path fmt = Pandoc.runIO $ do
  doc <- case Pandoc.getReader fmt of
    Left e -> throwError $ Pandoc.PandocParseError $ "unknown format: " ++ e
    Right (Pandoc.TextReader f, e) -> liftIO (T.IO.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
    Right (Pandoc.ByteStringReader f, e) -> liftIO (BSL.readFile $ unRawFilePath path) >>= f Pandoc.def{ Pandoc.readerExtensions = e }
  Pandoc.writeHtml5 Pandoc.def doc

baseHtml :: Wai.Request -> H.Html -> H.Html -> H.Html
baseHtml req hd bd = H.docTypeHtml $ do
  H.head $ do
    mapM_ (H.title . Html.byteString) $ lookup "REQUEST_URI" $ Wai.requestHeaders req
    hd
  H.body bd

htmlType :: MT.MediaType
htmlType = "text" MT.// "html" MT./: ("charset","utf-8")

fileType :: BS.ByteString -> (Maybe String, MT.MediaType)
fileType ".md" = (Just "markdown", "text" MT.// "markdown")
fileType ".rst" = (Just "rst", "text" MT.// "x-rst")
fileType ".tex" = (Just "latex", "application" MT.// "x-latex")
fileType ".docx" = (Just "docx", "application" MT.// "vnd.openxmlformats-officedocument.wordprocessingml.document")
fileType ".html" = (Nothing, "text" MT.// "html")
fileType ".htm" = (Nothing, "text" MT.// "html")
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

autoIndex :: Wai.Request -> [(Posix.RawFilePath, Posix.FileStatus)] -> H.Html
autoIndex req dir = baseHtml req (do
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.type_ "text/css"
      H.! HA.href (dtcdn <> "css")
    H.style
      H.! HA.type_ "text/css"
      $ ".fixed{font-family:monospace}"
    H.script
      H.! HA.type_ "text/javascript"
      H.! HA.src (dtcdn <> "js")
      $ mempty
    H.script
      H.! HA.defer mempty
      H.! HA.src "https://use.fontawesome.com/releases/v5.7.0/js/all.js"
      $ mempty
    H.script
      H.! HA.type_ "text/javascript"
      $ "$(function(){$('#dir').DataTable({paging:false});})")
  $ do
    H.table H.! HA.id "dir" $ do
      H.thead $ do
        H.tr $ do
          H.th H.! H.dataAttribute "width" "20px" H.! H.dataAttribute "class-name" "dt-body-right" $ mempty
          H.th H.! H.dataAttribute "class-name" "fixed" $ "name"
          H.th H.! H.dataAttribute "class-name" "dt-body-right fixed" $ "size"
          H.th "time"
      H.tbody $ do
        H.tr $ do
          H.td H.! H.dataAttribute "order" mempty $ H.span H.! HA.class_ "fas fa-folder" $ mempty
          H.td $ H.a H.! HA.href "../" $ "../"
          H.td mempty
          H.td mempty
        forM_ dir $ \(name, stat) -> do
          let
            isdir = Posix.isDirectory stat
            size | isdir = Nothing
                 | otherwise = Just $ Posix.fileSize stat
            mtime = Posix.modificationTimeHiRes stat
            ext = takeExtension name
            name'
              | isdir = name `BSC.snoc` '/'
              | otherwise = name
            icon _ | isdir = "folder-open"
            icon ".txt" = "file-alt"
            icon ".html"= "file-alt"
            icon ".htm" = "file-alt"
            icon ".md"  = "file-alt"
            icon ".rst" = "file-alt"
            icon ".tex" = "file-alt"
            icon ".doc" = "file-word"
            icon ".docx"= "file-word"
            icon ".pdf" = "file-pdf"
            icon ".png" = "file-image"
            icon ".gif" = "file-image"
            icon ".c"   = "file-code"
            icon ".cxx" = "file-code"
            icon ".cpp" = "file-code"
            icon ".h"   = "file-code"
            icon ".py"  = "file-code"
            icon ".csv" = "file-excel"
            icon ".xls" = "file-excel"
            icon ".xlsx"= "file-excel"
            icon ".tar" = "file-archive"
            icon ".zip" = "file-archive"
            icon ".gz"  = "file-archive"
            icon ".tgz" = "file-archive"
            icon _ = "file"
          H.tr $ do
            H.td H.! H.dataAttribute "order" (if isdir then "-" else if BS.null ext then "." else Html.byteStringValue ext) $
              H.span H.! HA.class_ ("fas fa-" <> icon ext) $ mempty
            H.td $
              H.a H.! HA.href (Html.byteStringValue name') $ Html.byteString name'
            H.td H.! H.dataAttribute "order" (foldMap (H.stringValue . show) size) $
              mapM_ (H.string . byteSize) size
            H.td H.! H.dataAttribute "order" (H.stringValue $ show $ (realToFrac mtime :: Milli)) $
              H.string $ formatTime defaultTimeLocale "%F %R %Z" $ posixSecondsToUTCTime mtime
  where
  dtcdn = "https://cdn.datatables.net/v/dt/jq-3.3.1/dt-1.10.18/datatables.min."

app :: Wai.Request -> IO Wai.Response
app req = do
  filename <- maybe (result $ Wai.responseLBS badRequest400 [] "") return $ header "REQUEST_FILENAME" <|> header "SCRIPT_FILENAME"
  stat <- maybe (result $ Wai.responseLBS notFound404 [] "") return =<< statFile filename
  let (dirname, basename) = splitFileName filename
  if Posix.fileSize stat == 0 && basename == indexFile then
    okResponse [] . autoIndex req <$> readDirPlus dirname
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
        (return . Just . baseHtml req mempty) <=< pandoc filename)
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

_apptest :: Wai.Request -> IO Wai.Response
_apptest = return . okResponse [] . BSC.unlines . map (\(h,v) -> CI.original h <> "=" <> v) . Wai.requestHeaders

main :: IO ()
main = Wai.run $ resultApplication app
