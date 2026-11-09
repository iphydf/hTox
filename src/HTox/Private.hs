{-# LANGUAGE CPP #-}
-- | Files for what holds secret keys.
module HTox.Private
  ( private
  ) where

import           Control.Exception  (IOException, try)
import           Control.Monad      (void)
import           System.Directory   (removeFile)
#if defined(mingw32_HOST_OS)
import qualified Data.ByteString    as BS
#else
import           System.Posix.Files (ownerReadMode, ownerWriteMode,
                                     unionFileModes)
import           System.Posix.IO    (OpenFileFlags (..), OpenMode (..), closeFd,
                                     defaultFileFlags, openFd)
#endif

-- | An empty file, made anew, that only its owner may read. Copies of it
-- keep that.
private :: FilePath -> IO ()
private path = do
  void (try (removeFile path) :: IO (Either IOException ()))
#if defined(mingw32_HOST_OS)
  -- A user's files are theirs alone already.
  BS.writeFile path BS.empty
#else
  closeFd =<< openFd path WriteOnly defaultFileFlags
    { creat = Just (unionFileModes ownerReadMode ownerWriteMode), exclusive = True }
#endif
