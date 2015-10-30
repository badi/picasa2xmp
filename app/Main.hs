{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (makeAbsolute, getDirectoryContents)
import System.FilePath (takeFileName, takeDirectory, (</>))
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))


-- reading .ini files (eg .picasa.ini)
import Data.Ini (Ini, readIniFile, lookupValue)

-- text
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- pipes
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Safe (SafeT, runSafeT, MonadSafe)
import Pipes.Files (find, filename_, regular)

import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.Either (rights, lefts)
import Data.Maybe (catMaybes)
import Data.Biapplicative (bipure, biliftA2)

import Data.Time (UTCTime, formatTime, parseTimeM, defaultTimeLocale)


-- -------------------------------------------------- misc utils

class ToText a where toText :: a -> Text

instance ToText Int where toText = T.pack . show

-- format string according to <http://tools.ietf.org/html/rfc3339#section-5.6 RFC3339>
rfc3339Format :: String
rfc3339Format = "%FT%T%z"

yearFormat :: String
yearFormat = "%Y"

-- -------------------------------------------------- Exiv / XMP

flattenXmpValue :: XMPValue -> [Text]
flattenXmpValue v = maybe [] (singleton . toText) (valueType v)  ++ txt
    where txt =  [T.concat ["'", valueText v, "'"]]

          singleton :: a -> [a]
          singleton a = [a]


eval :: Exiv2ModifyCommand -> String
eval c = T.unpack 
         $ T.intercalate " " 
         $ case c of
             SET k v -> ["set", unKey k] ++ flattenXmpValue v
             ADD k v -> ["add", unKey k] ++ flattenXmpValue v
             DEL k   -> ["del", unKey k]


data ProcessResult a = Result {
      cmd :: [Text]
    , exitCode :: ExitCode
    , stdout :: Text
    , stderr :: Text
    , result :: a
    } deriving (Eq, Show)

run1cmd :: FilePath -> Exiv2ModifyCommand -> IO (ProcessResult ())
run1cmd imagePath cmd = do
  let args = ["-M" <> eval cmd, imagePath]
      run = readProcessWithExitCode "exiv2" args ""
  (ec, out, err) <- run
  return $ Result ("exiv2" : map T.pack args) ec (T.pack out) (T.pack err) ()


data Exiv2ModifyCommand = SET XMPKey XMPValue
                        | ADD XMPKey XMPValue
                        | DEL XMPKey
                          deriving (Eq, Show)

newtype XMPKey = XMPKey {unKey :: Text}
    deriving (Eq, Show)

instance ToText XMPKey where toText (XMPKey t) = t

data XMPType = XmpText
             | XmpAlt
             | XmpBag
             | XmpSeq
             | LangAlt
               deriving (Eq, Show)

instance ToText XMPType where toText = T.pack . show

data XMPValue = XMPValue {valueType :: (Maybe XMPType) , valueText :: Text}
              deriving (Eq, Show)

instance ToText XMPValue where
    toText (XMPValue t v) =
        case t of
          Just t' -> toText t' <> " " <> v
          Nothing -> v

cmd2Args :: Exiv2ModifyCommand -> Text
cmd2Args (SET k v) = T.intercalate " " ["set", toText k, toText v]
cmd2Args (ADD k v) = T.intercalate " " ["add", toText k, toText v]
cmd2Args (DEL k  ) = T.intercalate " " ["del", toText k]

xmp :: Text -- ^ tag name
    -> XMPType -- ^ type
    -> Text -- ^ value
    -> (XMPKey, XMPValue)
xmp tag typ val = ( (XMPKey tag)
                  , (XMPValue (Just typ)) val
                  )

at :: Text -> Int -> Text
at t i = t <> "[" <> toText i <> "]"

at' :: XMPKey -> Int -> XMPKey
at' (XMPKey t) = XMPKey . at t

bag :: Text -- ^ tag name
    -> XMPType -- ^ value type
    -> [Text] -- ^ values
    -> (XMPKey, [XMPValue])
bag tag typ vals = (k, vs)
    where k  = XMPKey tag
          vs = map (XMPValue (Just typ)) vals

setBag :: Text -- ^ tag name
       -> XMPType -- ^ value type
       -> [Text] -- ^ values
       -> [Exiv2ModifyCommand]
setBag tag typ vals = 
    let (key, vals') = bag tag typ vals
        add i v = SET (key `at'` i) v
    in SET key (XMPValue (Just XmpBag) "") 
       : zipWith add [1..] vals'


-- -------------------------------------------------- Picasa


data PicasaAlbum = PicasaAlbum {
      albumId :: Text
    , albumName :: Text
    , albumCreateDate :: UTCTime
    } deriving (Eq, Show)


data PicasaMetadata = PicasaMetadata {
      star :: Bool
    , albums :: [PicasaAlbum]
    } deriving (Eq, Show)

data PicasaImage = PicasaImage {
      imagePath :: FilePath
    , metadata :: PicasaMetadata
    } deriving (Eq, Show)

-- | Return `Left` if there is no metadata for the file, else `Right`
picasaImage :: Ini -> FilePath -> Either String PicasaImage
picasaImage ini path = do
  let name = T.pack $ takeFileName path

  -- star
  let isStarred = either (const False) (=="yes") $ lookupValue name "star" ini

  -- albums
  albumIds <- T.splitOn "," <$> lookupValue name "albums" ini
  albums <- mapM (picasaAlbum ini) albumIds

  let metadata = PicasaMetadata isStarred albums
  return $ PicasaImage path metadata

picasaAlbum :: Ini -> Text -> Either String PicasaAlbum
picasaAlbum ini aid = PicasaAlbum <$> pure aid <*> name <*> time
    where section = ".album:" <> aid
          name = lookupValue section "name" ini
          date = lookupValue section "date" ini
          time = squash $ parseTimeM False defaultTimeLocale rfc3339Format . T.unpack <$> date

          squash :: Either String (Maybe b) -> Either String b
          squash (Left e) = Left e
          squash (Right Nothing) = Left $ "Failed to parse date " <> show date
          squash (Right (Just d)) = Right d


loadPicasaImage :: FilePath -> IO [PicasaImage]
loadPicasaImage picasa_ini_path = do
  path <- makeAbsolute picasa_ini_path
  let dirname = takeDirectory path
  files <- map (dirname</>) 
           . filter (not . (`elem` [".", "..", ".picasa.ini", ".picasaoriginals"]))
          <$> getDirectoryContents dirname
  ini <- readIniFile path
  return $ rights $ either fail (\ini' -> map (picasaImage ini') files) ini


-- -------------------------------------------------- Lightroom

data LightroomSettings = LightroomSettings {
      albumPrefix :: Text
    , hierarchySeparator :: Text
    } deriving (Eq, Show)

defaultSettings :: LightroomSettings
defaultSettings = LightroomSettings {
                    albumPrefix = "album"
                  , hierarchySeparator = "|"
                  }

-- -------------------------------------------------- picasa -> cmd

picasaStar2cmd :: PicasaImage -> Maybe [Exiv2ModifyCommand]
picasaStar2cmd p = 
    if star $ metadata p
    then Just [uncurry SET $ xmp "Xmp.xmp.Rating" XmpText "5"]
    else Nothing

picasaAlbums2cmd :: LightroomSettings -> PicasaImage -> Maybe [Exiv2ModifyCommand]
picasaAlbums2cmd settings = wrapMaybe . concatMap mk . albums . metadata
    where
      mk :: PicasaAlbum -> [Exiv2ModifyCommand]
      mk a = 
        let tags = [albumPrefix settings, time, albumName a]
            kwds = T.intercalate (hierarchySeparator settings) tags : tags
            time = T.pack $ formatTime defaultTimeLocale yearFormat $ albumCreateDate a
        in concat [
             [uncurry SET $ xmp "Xmp.lr.HierarchicalSubject" XmpText (albumPrefix settings)]
           , setBag "Xmp.dc.subject" XmpText tags
           , setBag "Xmp.lr.hierarchicalSubject" XmpText kwds
           ]

      wrapMaybe :: [a] -> Maybe [a]
      wrapMaybe l = if null l then Nothing else Just l


picasa2cmd :: LightroomSettings -> PicasaImage -> (FilePath, [Exiv2ModifyCommand])
picasa2cmd s p = (,) (imagePath p) 
                 $ concat $ catMaybes [
                   picasaStar2cmd p
                 , picasaAlbums2cmd s p
                 ]


-- -------------------------------------------------- execution pipeline

findDotPicasa :: FilePath -> Producer FilePath (SafeT IO) ()
findDotPicasa prefix = do
  find prefix (filename_ (==".picasa.ini") <> regular)

loadImages :: MonadIO m => Pipe FilePath PicasaImage m ()
loadImages = forever $ do
  path <- await
  imgs <- liftIO $ loadPicasaImage path
  mapM_ yield imgs

createCommands :: MonadIO m => LightroomSettings -> Pipe PicasaImage (FilePath, [Exiv2ModifyCommand]) m ()
createCommands settings = forever $ do
  img <- await
  let pair = picasa2cmd settings img
  yield pair


updateFileAction :: MonadIO m => Pipe (FilePath, [Exiv2ModifyCommand]) ((FilePath, IO [ProcessResult ()])) m ()
updateFileAction = forever $ do
  (path, cmds) <- await
  liftIO $ putStrLn $ "Updating " <> path
  let action = mapM (run1cmd path) cmds
  yield $ (path, action)

strength :: Functor f => (a, f b) -> f (a, b)
strength (a, fb) = fmap ((,) a) fb

runAction :: MonadIO m => Pipe (IO a) a m ()
runAction = forever $ do
  action <- await
  result <- liftIO action
  yield result

processResult :: (Show a, MonadIO m) => FilePath -> ProcessResult a -> m (Either Text a)
processResult path r =
  case exitCode r of
    ExitSuccess -> return $ Right $ result r
    ExitFailure e -> do liftIO $ putStrLn $ "FAILURE: " <> show (cmd r)
                        liftIO $ T.appendFile path $ T.pack $ show r <> "\n"
                        return $ Left $ stderr r

summarizeFileResults :: Monad m => Pipe (FilePath, [Either Text a]) (Either Text ()) m ()
summarizeFileResults = forever $ do
  (path, runResults) <- await
  yield $ if null (lefts runResults)
          then Right ()
          else Left $ "Failure occured while processing " <> T.pack path <> ", check error log for details"

countLeftRight :: Monad m => Pipe (Either a b) (Int, Int) m ()
countLeftRight = forever $ do
  e <- await
  let l = const (1, 0)
      r = const (0, 1)
      t = either l r e
  yield t


pipeline :: Foldable t => FilePath -> LightroomSettings -> t FilePath -> Producer (Either Text ()) (SafeT IO) ()
pipeline logfile settings prefixes = do
  for (each prefixes) findDotPicasa
  >-> loadImages
  >-> createCommands settings
  >-> updateFileAction
  >-> P.map strength
  >-> runAction
  >-> P.mapM (strength . fmap (mapM (processResult logfile)))
  >-> summarizeFileResults




-- -- -------------------------------------------------- main

main' :: FilePath -> LightroomSettings -> [FilePath] -> IO (Int, Int)
main' logfile settings prefixes = runSafeT
                                  $ P.fold (biliftA2 (+) (+)) (bipure 0 0) id
                                  $ pipeline logfile settings prefixes
                                    >-> countLeftRight

main :: IO ()
main = do
  prefixes <- getArgs
  let errfile = "err.txt"

  T.writeFile errfile ""
  (failures, successes) <- main' errfile defaultSettings prefixes

  putStrLn $ "Processed  " <> show (failures + successes) <> " files."
  putStrLn $ "Failures:  " <> show failures
  putStrLn $ "Successes: " <> show successes
