{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow (first)
import           Control.Exception (bracket, handle, handleJust, SomeException)
import           Control.Monad ((<=<), forM_, guard, join, mfilter)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import           Data.Fixed (Milli)
import           Data.List (elemIndex)
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Network.HTTP.Media as MT
import           Network.HTTP.Types.Header (hAccept, hContentType, hIfModifiedSince, hLastModified)
import           Network.HTTP.Types.Status (ok200, notModified304, forbidden403, notFound404, internalServerError500)
import qualified Network.Socket as Net
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.RequestLogger as WLog
import           Numeric (showFFloat)
import           System.Environment (getArgs)
import qualified System.Posix.ByteString as Posix
import           System.Posix.FilePath ((</>), takeExtension, splitFileName, takeDirectory)
import           System.Posix.Files (removeLink)
import           System.IO (stderr)
import qualified System.IO.Error as IOE
import qualified Text.Blaze.Html.Renderer.Utf8 as Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Pandoc as Pandoc

import qualified Waimwork.Blaze as Html
import           Waimwork.HTTP (formatHTTPDate, parseHTTPDate)
import           Waimwork.Response (okResponse)

unRawFilePath :: Posix.RawFilePath -> FilePath
unRawFilePath = BSC.unpack -- XXX unsafe encoding

statFile :: Posix.RawFilePath -> IO (Maybe Posix.FileStatus)
statFile f
  | BSC.length f > 1024 = return Nothing
  | otherwise = IOE.catchIOError
  (Just <$> Posix.getFileStatus f)
  (\_ -> return Nothing)

autoIndexFiles :: [Posix.RawFilePath]
autoIndexFiles = ["index.auto", ".index.auto", autoIndexFileRec]

autoIndexFileRec :: Posix.RawFilePath
autoIndexFileRec = "index.autorec"

indexFiles :: [Posix.RawFilePath]
indexFiles = 
  [ "index.html"
  , "index.htm"
  , "index.md"
  ] ++ autoIndexFiles ++
  [ "README.md"
  , "README"]

maxFiles :: Int
maxFiles = 10000

statMatching :: Posix.FileStatus -> Posix.RawFilePath -> IO (Maybe Posix.FileStatus)
statMatching match f = mfilter matches <$> statFile f where
  matches stat =
    Posix.deviceID  stat == Posix.deviceID  match &&
    Posix.fileOwner stat == Posix.fileOwner match

readDirPlus :: Posix.RawFilePath -> IO [(Posix.RawFilePath, Posix.FileStatus)]
readDirPlus dir = bracket (Posix.openDirStream dir) Posix.closeDirStream rdp where
  rdp ds = loop maxFiles id where
    loop 0 l = return $ l []
    loop n l = do
      name <- Posix.readDirStream ds
      case BSC.uncons name of
        Nothing -> return $ l []
        Just ('.', _) -> loop n l
        _ | name `elem` autoIndexFiles -> loop n l
        _ -> loop (pred n) . maybe l ((l .) . (:) . (,) name) =<<
          statFile (dir </> name)

pandoc :: Posix.RawFilePath -> T.Text -> IO (Either Pandoc.PandocError H.Html)
pandoc path fmt = Pandoc.runIO $ do
  rdr <- Pandoc.getReader fmt
  doc <- case rdr of
    (Pandoc.TextReader       f, e) -> liftIO (TIO.readFile path') >>= f Pandoc.def{ Pandoc.readerExtensions = e }
    (Pandoc.ByteStringReader f, e) -> liftIO (BSL.readFile path') >>= f Pandoc.def{ Pandoc.readerExtensions = e }
  Pandoc.writeHtml5 Pandoc.def doc
  where
  path' = unRawFilePath path

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

mintersperseMap :: Monoid m => m -> (a -> m) -> [a] -> m
mintersperseMap _ _ [] = mempty
mintersperseMap d f (x:l) = f x <> foldMap ((<>) d . f) l

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
      H.! HA.src "https://use.fontawesome.com/releases/v5.9.0/js/all.js"
      $ mempty
    H.script
      H.! HA.type_ "text/javascript"
      $ "$(document).ready(function(){$('#dir').DataTable({paging:false,order:["
        <> mintersperseMap "," (\(d,o) -> "[" <> H.toMarkup o <> ",'" <> (if d then "a" else "de") <> "sc']") order <> "]});})")
  $ do
    H.table H.! HA.id "dir" $ do
      H.thead $ do
        H.tr $ do
          H.th H.! H.dataAttribute "width" "20px" H.! H.dataAttribute "class-name" "dt-body-right" $ mempty
          H.th H.! H.dataAttribute "class-name" "fixed" $ "name"
          H.th H.! H.dataAttribute "class-name" "dt-body-right fixed" H.! H.dataAttribute "type" "num" $ "size"
          H.th "time"
      H.tbody $ do
        H.tr $ do
          H.td H.! H.dataAttribute "order" mempty $ H.span H.! HA.class_ "fas fa-folder" $ mempty
          H.td H.! H.dataAttribute "order" ".." $ H.a H.! HA.href "../" $ "../"
          H.td H.! H.dataAttribute "order" "0" $ mempty
          H.td H.! H.dataAttribute "order" "0" $ mempty
        forM_ dir $ \(name, stat) -> do
          let
            isdir = Posix.isDirectory stat
            size = Posix.fileSize stat
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
            H.td H.! H.dataAttribute "order" (Html.byteStringValue name) $
              H.a H.! HA.href (Html.byteStringValue name') $ Html.byteString name'
            H.td H.! H.dataAttribute "order" (H.stringValue $ show $ size) $
              H.string $ byteSize $ size
            H.td H.! H.dataAttribute "order" (H.stringValue $ show $ (realToFrac mtime :: Milli)) $
              H.string $ formatTime defaultTimeLocale "%F %R %Z" $ posixSecondsToUTCTime mtime
  where
  dtcdn = "https://cdn.datatables.net/v/dt/jq-3.3.1/dt-1.10.24/datatables.min."
  order = mapMaybe orders $ Wai.queryString req
  orders ("order",Just f) = case BSC.uncons f of
    Just ('+',s) -> (,) True <$> field s
    Just ('-',s) -> (,) False <$> field s
    _ -> (,) True <$> field f
  orders _ = Nothing
  field f = elemIndex f ["type","name","size","time"]

app :: Wai.Request -> IO Wai.Response
app req = statFile filename >>= maybe
  (return $ Wai.responseLBS notFound404 [] "No such file or directory")
  (\stat -> handle (\e -> do
    BSC.hPutStrLn stderr $ filename <> ": " <> BSC.pack (show (e :: SomeException))
    return $ Wai.responseLBS internalServerError500 [] mempty)
    $ if BSC.null basename
    then checkIndex stat dirname indexFiles
    else serve filename (dirname, basename) stat)
  where
  filename = Wai.rawPathInfo req
  (dirname, basename) = splitFileName filename
  checkIndex _ _ [] = return $ Wai.responseLBS forbidden403 [] "Permission denied"
  checkIndex stat dir (file:files) = maybe
    (checkIndex stat dir files)
    (serve filepath (dir, file))
    =<< (if file == autoIndexFileRec
      then checkParents stat dir
      else id) (statMatching stat filepath)
    where
    filepath = dir </> file
  checkParents stat dir pre = pre >>= maybe
    (maybe
      (return Nothing)
      (\pstat -> checkParents pstat parent $
        mfilter ((0 ==) . Posix.fileSize) <$> statMatching pstat (parent </> autoIndexFileRec))
      =<< statMatching stat parent)
    (return . Just)
    where
    parent = dir </> ".."
  serve file (dir, base) stat
    | Posix.fileSize stat == 0 && base `elem` autoIndexFiles =
      okResponse [] . autoIndex req <$> readDirPlus dir
    | any (modtime <=) $ parseHTTPDate =<< header hIfModifiedSince =
      return $ Wai.responseLBS notModified304 [] ""
    | otherwise =
      maybe
        (maybe
          (Wai.responseFile ok200 (headers mt) (unRawFilePath file) Nothing)
          (\u -> Wai.responseLBS ok200 (("X-Accel-Redirect", "/_" <> takeDirectory u </> base) : headers mt) mempty)
          (header "REQUEST_URI"))
        (Wai.responseBuilder ok200 (headers htmlType) . Html.renderHtmlBuilder)
      <$> maybe
        (return Nothing)
        (either
          (\e -> Nothing <$ BSC.hPutStrLn stderr (file <> ": " <> BSC.pack (show e)))
          (return . Just . baseHtml req mempty) <=< pandoc file . T.pack)
        pt
      where
      modtime = posixSecondsToUTCTime $ Posix.modificationTimeHiRes stat
      (pt, mt) =
        first (>>= \pt' -> do
          accept <- header hAccept
          guard $ all (`notElem` [Nothing, Just "1"]) $ query "raw"
          join $ MT.mapAcceptMedia [(mt, Nothing), (htmlType, Just pt')] accept)
        $ fileType $ takeExtension file
      headers ct =
        [ (hContentType, MT.renderHeader ct)
        , (hLastModified, formatHTTPDate modtime)
        ]
  query v = lookup v $ Wai.queryString req
  header n = lookup n $ Wai.requestHeaders req

_apptest :: Wai.Request -> IO Wai.Response
_apptest = return . okResponse [] . BSC.unlines . map (\(h,v) -> CI.original h <> "=" <> v) . Wai.requestHeaders

main :: IO ()
main = do
  [arg] <- getArgs
  handleJust (guard . IOE.isDoesNotExistError) return $ removeLink arg
  sock <- Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol
  Net.bind sock $ Net.SockAddrUnix arg
  Net.listen sock 4
  logger <- WLog.mkRequestLogger WLog.defaultRequestLoggerSettings
    { WLog.outputFormat = WLog.Apache WLog.FromHeader
    }
  Warp.runSettingsSocket Warp.defaultSettings sock
    $ logger
    $ (>>=) . app
