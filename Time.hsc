{-# LANGUAGE RecordWildCards #-}

module Time
  ( parseDatetime
  , parseDatetime'
  ) where

import           Control.Monad (guard)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.System (SystemTime(..), utcToSystemTime, systemToUTCTime)
import           Foreign.C (CString, CBool(..), withCString)
import           Foreign.Ptr (Ptr)
import           Foreign.Marshal (alloca, with, maybeWith)
import qualified Foreign.Marshal.Unsafe as Unsafe (unsafeLocalState)
import           Foreign.Storable (Storable(..))

#include <sys/time.h>

-- |cf Data.Time.Clock.Internal.CTimespec
newtype Timespec = Timespec SystemTime

instance Storable Timespec where
  sizeOf _ = #size struct timespec
  alignment _ = #alignment struct timespec
  peek p = do
    tv_sec  <- #{peek struct timespec, tv_sec  } p
    tv_nsec <- #{peek struct timespec, tv_nsec } p
    return $ Timespec $ MkSystemTime tv_sec tv_nsec
  poke p (Timespec (MkSystemTime sec nsec)) = do
    #{poke struct timespec, tv_sec  } p sec
    #{poke struct timespec, tv_nsec } p nsec

foreign import ccall unsafe parse_datetime :: Ptr Timespec -> CString -> Ptr Timespec -> IO CBool

parseDatetime :: Maybe UTCTime -> String -> IO (Maybe UTCTime)
parseDatetime now s =
  maybeWith with (Timespec . utcToSystemTime <$> now) $ \cnow ->
    withCString s $ \cs ->
      alloca $ \r -> do
        ok <- parse_datetime r cs cnow
        Timespec st <- peek r
        return $ systemToUTCTime st <$ guard (ok /= 0)
        
parseDatetime' :: UTCTime -> String -> Maybe UTCTime
parseDatetime' now = Unsafe.unsafeLocalState . parseDatetime (Just now)
