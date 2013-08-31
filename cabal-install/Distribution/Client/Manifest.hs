module Manifest (
  Manifest(..),
  writeManifest, readManifest, tryReadManifest, scanDirectory
) where

import Distribution.Package (InstalledPackageId)
import Distribution.ParseUtils (simpleField, listField, parseFieldsFlat, ParseResult(..), showFilePath, parseFilePathQ, showFields, FieldDescr)
import Distribution.Text (Text(disp, parse))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Text.PrettyPrint (empty)

data Manifest = Manifest
  { libraryId :: Maybe InstalledPackageId
  , manifestFiles :: [FilePath]
  }

writeManifest :: Manifest -> FilePath -> IO ()
writeManifest manifest file = writeFile file (showManifest manifest)

readManifest :: FilePath -> IO Manifest
readManifest file = do
  manifest <- tryReadManifest file
  case manifest of
    ParseOk _ manifest' -> return manifest'
    ParseFailed e ->  error (show e)  -- TODO: handle in a more appropriate way

tryReadManifest :: FilePath -> IO (ParseResult Manifest)
tryReadManifest file = readFile file >>= return . parseManifest

scanDirectory :: FilePath -> IO [FilePath]
scanDirectory root = scan ""
  where
    scan relative = do
      let absolute = root </> relative
      isDirectory <- doesDirectoryExist absolute
      if isDirectory
        then getDirectoryContents' absolute >>= mapM (scan . (relative </>)) >>= return . concat
        else return [relative]

    getDirectoryContents' absolute = do
      contents <- getDirectoryContents absolute
      return $ filter (\p -> p /= "." && p /= "..") contents

parseManifest :: String -> ParseResult Manifest
parseManifest = parseFieldsFlat fieldsManifest emptyManifest

showManifest :: Manifest -> String
showManifest = showFields fieldsManifest

emptyManifest :: Manifest
emptyManifest = Manifest {
  libraryId = Nothing,
  manifestFiles = []
  }

fieldsManifest :: [FieldDescr Manifest]
fieldsManifest = 
  [ simpleField "id" (maybe empty disp) (fmap Just parse) libraryId (\x r -> r {libraryId=x})
  , listField "files" showFilePath parseFilePathQ manifestFiles (\xs r -> r {manifestFiles=xs})
  ]
