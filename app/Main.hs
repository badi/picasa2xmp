{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Directory
import System.FilePath
import System.Environment
import System.Process
import System.Exit

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Monad
import Control.Concurrent.Async
import Data.Text (Text)
import Data.Ini
--import Data.List
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Either
import Data.Maybe
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Pipes.Safe (SafeT, runSafeT, MonadSafe)
import Pipes.Files

class ToText a where toText :: a -> Text

instance ToText Int where toText = T.pack . show

-- -------------------------------------------------- Exiv / XMP

list :: a -> [a]
list a = [a]

flattenXmpValue :: XMPValue -> [Text]
flattenXmpValue v = maybe [] (list . toText) (valueType v)  ++ txt
    where txt =  [T.concat ["'", valueText v, "'"]]

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



xmp_Rating :: Int -> (XMPKey, XMPValue)
xmp_Rating = xmp "XMP.xmp.Rating" XmpText . T.pack . show

-- -------------------------------------------------- Picasa


data PicasaAlbum = PicasaAlbum {
      albumId :: Text
    , albumName :: Text
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
picasaAlbum ini aid = PicasaAlbum <$> pure aid <*> name
    where name = lookupValue (".album:"<>aid) "name" ini


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
    then Just [uncurry SET $ xmp_Rating 5]
    else Nothing

picasaAlbums2cmd :: LightroomSettings -> PicasaImage -> Maybe [Exiv2ModifyCommand]
picasaAlbums2cmd settings = wrapMaybe . concatMap mk . albums . metadata
    where
      mk :: PicasaAlbum -> [Exiv2ModifyCommand]
      mk a = 
        let tags = [albumPrefix settings, albumName a]
            kwds = T.intercalate (hierarchySeparator settings) tags : tags
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

findDotPicasa :: FilePath -> P.Producer FilePath (SafeT IO) ()
findDotPicasa prefix = do
  find prefix (filename_ (==".picasa.ini") <> regular)

loadImages :: P.MonadIO m => P.Pipe FilePath PicasaImage m ()
loadImages = forever $ do
  path <- P.await
  imgs <- P.liftIO $ loadPicasaImage path
  mapM_ P.yield imgs

createCommands :: P.MonadIO m => LightroomSettings -> P.Pipe PicasaImage (FilePath, [Exiv2ModifyCommand]) m ()
createCommands settings = forever $ do
  img <- P.await
  let pair = picasa2cmd settings img
  P.yield pair

updateFileAction :: P.MonadIO m => P.Pipe (FilePath, [Exiv2ModifyCommand]) (IO (ProcessResult ())) m ()
updateFileAction = forever $ do
  (path, cmds) <- P.await
  P.liftIO $ putStrLn $ "Updating " <> path
  let results = map (run1cmd path) cmds
  mapM_ P.yield results


runAction :: P.MonadIO m => P.Pipe (IO a) a m ()
runAction = forever $ do
  action <- P.await
  result <- P.liftIO action
  P.yield result

processResult :: (Show a, P.MonadIO m) => FilePath -> P.Pipe (ProcessResult a) (Either Text a) m ()
processResult path = forever $ do
  r <- P.await
  case exitCode r of
    ExitSuccess -> P.yield $ Right $ result r
    ExitFailure e -> do P.liftIO $ putStrLn $ "FAILURE: " <> show (cmd r)
                        P.liftIO $ T.appendFile path $ T.pack $ show r <> "\n"
                        P.yield $ Left $ stderr r

doAsync :: P.MonadIO m => P.Pipe (IO a) (Async a) m ()
doAsync = forever $ do
  action <- P.await
  forked <- P.liftIO $ async action
  P.yield forked

collect :: P.MonadIO m => P.Pipe (Async a) a m ()
collect = forever $ do
  forked <- P.await
  result <- P.liftIO $ wait forked
  P.yield result

-- -------------------------------------------------- main

main' :: FilePath -> LightroomSettings -> [FilePath] -> IO ()
main' logfile settings prefixes = runSafeT $ P.runEffect $ do
  P.for (P.each prefixes) findDotPicasa
  P.>-> loadImages
  P.>-> createCommands settings
  P.>-> updateFileAction
  P.>-> runAction
  P.>-> processResult logfile
  P.>-> P.drain

main :: IO ()
main = do
  prefixes <- getArgs
  let errfile = "err.txt"
  T.writeFile errfile ""
  main' errfile defaultSettings prefixes
