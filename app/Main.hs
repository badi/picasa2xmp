{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import System.FilePath

import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)
import Data.Ini
import Data.List
import qualified Data.HashMap.Strict as H
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Either
import Data.Maybe

class ToText a where toText :: a -> Text

-- -------------------------------------------------- Exiv / XMP

data Exiv2ModifyCommand = SET XMPKey XMPValue
                        | ADD XMPKey XMPValue
                        | DEL XMPKey
                          deriving (Eq, Show)

newtype XMPKey = XMPKey Text
    deriving (Eq, Show)

instance ToText XMPKey where toText (XMPKey t) = t

data XMPType = XmpText
             | XmpAlt
             | XmpBag
             | XmpSeq
             | LangAlt
               deriving (Eq, Show)

instance ToText XMPType where toText = T.pack . show

data XMPValue = XMPValue (Maybe XMPType) Text
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

xmpCaption :: Text -> (XMPKey, XMPValue)
xmpCaption = (,) (XMPKey "Xmp.dc.description") . XMPValue (Just LangAlt)

xmpTitle :: Text -> (XMPKey, XMPValue)
xmpTitle = (,) (XMPKey "XMP.dc.title") . XMPValue (Just LangAlt)

xmpLabel :: Text -> (XMPKey, XMPValue)
xmpLabel = (,) (XMPKey "XMP.xmp.Label") . XMPValue (Just XmpText)

xmpRating :: Int -> (XMPKey, XMPValue)
xmpRating = (,) (XMPKey "XMP.xmp.Rating") . XMPValue (Just XmpText) . T.pack . show

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


loadPicasaMetadata :: FilePath -> IO [PicasaImage]
loadPicasaMetadata picasa_ini_path = do
  path <- makeAbsolute picasa_ini_path
  let dirname = takeDirectory path
  files <- map (dirname</>) 
           . filter (not . (`elem` [".", "..", ".picasa.ini", ".picasaoriginals"]))
          <$> getDirectoryContents dirname
  ini <- readIniFile path
  return $ rights $ either fail (\ini' -> map (picasaImage ini') files) ini

picasaStar2cmd :: PicasaImage -> Maybe Exiv2ModifyCommand
picasaStar2cmd p = 
    if star $ metadata p
    then Just $ uncurry SET $ xmpRating 5
    else Nothing

picasaAlbums2cmd :: Text -> PicasaImage -> Maybe Exiv2ModifyCommand
picasaAlbums2cmd prefix = fmap (uncurry SET . xmpLabel)
                          . toMaybe
                          . T.intercalate ","
                          . map toLabelText
                          . albums . metadata
    where
      toLabelText :: PicasaAlbum -> Text
      toLabelText a = prefix <> albumName a

      toMaybe :: Text -> Maybe Text
      toMaybe t = if T.null t then Nothing else Just t

data ConvertSettings = ConvertSettings {
      albumPrefix :: Text
    } deriving (Eq, Show)

defaultSettings :: ConvertSettings
defaultSettings = ConvertSettings "album:"

picasa2cmd :: ConvertSettings -> PicasaImage -> (FilePath, [Exiv2ModifyCommand])
picasa2cmd s p = (,) (imagePath p) 
                 $ catMaybes [
                   picasaStar2cmd p
                 , picasaAlbums2cmd (albumPrefix s) p
                 ]


-- eval :: FilePath -> [Exiv2ModifyCommand] -> IO ()
-- eval imagePath = run
--     where
--       cmdline cmd = ["exiv2", "-M" <> cmd2Args cmd, imagePath ]
--       run cmd = 

main :: IO ()
main = putStrLn "Hello"
