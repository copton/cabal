-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Manifest
-- Copyright   :  (c) Google Inc., 2013
-- License     :  BSD-like
--
-- Maintainer  :  Alexander Bernauer <copton@google.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- Maintain metadata of packages.
--
-- Needed to support uninstall.
-----------------------------------------------------------------------------
module Distribution.Client.Manifest (
  -- * Data types
  Manifest(..),

  -- * Functions
  -- ** Reading and writing manifests.
  writeManifest, readManifest, tryReadManifest,
  -- ** utilities
  scanDirectory
) where

import Distribution.Package
         ( InstalledPackageId )
import Distribution.ParseUtils
         ( simpleField, listField, parseFieldsFlat, ParseResult(..),
           showFilePath, parseFilePathQ, showFields, FieldDescr )
import Distribution.Text
         ( Text(disp, parse) )
import System.Directory
         ( getDirectoryContents, doesDirectoryExist )
import System.FilePath
         ( (</>), splitPath, joinDrive )
import Text.PrettyPrint
         ( empty )

-- | Metadata of a package
data Manifest = Manifest
  { libraryId :: Maybe InstalledPackageId  -- ^ If the cabal package contains a library, this is that library's ID
  , manifestFiles :: [FilePath]  -- ^ The list of files copied to the file system when installing the package
  }

-- | Depending on the operating system and cabal file paths inside the destdir are subject to the following convention:
data PathConvention
  = Unix                 -- ^ replace destdir with '/' to get the install path of a file
  | OldWindows FilePath  -- ^ replace destdir with the given drive to geht the install path of a file
  | NewWindows           -- ^ the first directory hierachy under destdir encodes the install drive for each file

-- | Write a manifest to disk.
writeManifest :: Manifest -> FilePath -> IO ()
writeManifest manifest file = writeFile file (showManifest manifest)

-- | Read a manifest from disk.
-- This function fails with 'error' in case of a parse error. This should actually never happen,
-- since manifest files are only written by Cabal itself. See 'tryReadManifest' if you need to handle
-- these cases separately.
readManifest :: FilePath -> IO Manifest
readManifest file = do
  manifest <- tryReadManifest file
  case manifest of
    ParseOk _ manifest' -> return manifest'
    ParseFailed e ->  error (show e)  -- TODO: handle in a more appropriate way

-- | Read a manifest from disk.
-- This function returns a 'ParseResult', thus allowing to handle corrupt manifest files.
tryReadManifest :: FilePath -> IO (ParseResult Manifest)
tryReadManifest file = readFile file >>= return . parseManifest

-- | Scan a destdir and return absolute install paths for each file.
scanDirectory :: PathConvention -> FilePath -> IO [FilePath]
scanDirectory convention root = scan ""
  where
    scan relative = do
      let absolute = root </> relative
      isDirectory <- doesDirectoryExist absolute
      if isDirectory
        then     getDirectoryContents' absolute
             >>= mapM (scan . (relative </>))
             >>= return . concat
        else return [installPath convention relative]

    getDirectoryContents' absolute = do
      contents <- getDirectoryContents absolute
      return $ filter (\p -> p /= "." && p /= "..") contents

-- | Turn a relative path to an absolute install path.
installPath
  :: PathConvention
  -> FilePath  -- ^ relative path within the destdir directory; subject to path convention
  -> FilePath  -- ^ absolute path; correct for the respective operating system
installPath Unix relative = "/" </> relative
installPath (OldWindows drive) relative = joinDrive drive relative
installPath NewWindows relative =
  let
    (driveComponent:pathComponents) = splitPath relative
    (driveLetter:_) = driveComponent
    drive = driveLetter : ":\\"
  in
    joinDrive drive (concat pathComponents)


-- | The actual manifest parser.
parseManifest :: String -> ParseResult Manifest
parseManifest = parseFieldsFlat fieldsManifest emptyManifest

-- | A printer for manifests.
showManifest :: Manifest -> String
showManifest = showFields fieldsManifest

-- | The empty manifest, used as initial state by the parser.
emptyManifest :: Manifest
emptyManifest = Manifest {
  libraryId = Nothing,
  manifestFiles = []
  }

-- | Field descriptions of a manifest, used by the parser.
fieldsManifest :: [FieldDescr Manifest]
fieldsManifest = 
  [ simpleField "id" (maybe empty disp) (fmap Just parse) libraryId (\x r -> r {libraryId=x})
  , listField "files" showFilePath parseFilePathQ manifestFiles (\xs r -> r {manifestFiles=xs})
  ]
