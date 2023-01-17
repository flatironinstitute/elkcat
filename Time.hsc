{-# LANGUAGE RecordWildCards #-}

module Time
  ( getLocalTime
  , initTime
  ) where

import           Control.Monad (when)
import           Data.Maybe (isNothing)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import           Foreign.C (CString, CInt(..), CLong(..), withCString)
import           Foreign.Ptr (Ptr)
import           Foreign.Marshal (alloca)
import           Foreign.Storable (Storable(..))
import           System.Environment (lookupEnv, setEnv)

#include <time.h>

data TM = TM
  { tm_sec
  , tm_min
  , tm_hour
  , tm_mday
  , tm_mon
  , tm_year
  , tm_wday
  , tm_yday
  , tm_isdst :: CInt
  , tm_gmtoff :: CLong
  , tm_zone :: CString
  }

instance Storable TM where
  sizeOf _ = #size struct tm
  alignment _ = #alignment struct tm
  peek p = do
    tm_sec    <- #{peek struct tm, tm_sec   } p
    tm_min    <- #{peek struct tm, tm_min   } p
    tm_hour   <- #{peek struct tm, tm_hour  } p
    tm_mday   <- #{peek struct tm, tm_mday  } p
    tm_mon    <- #{peek struct tm, tm_mon   } p
    tm_year   <- #{peek struct tm, tm_year  } p
    tm_wday   <- #{peek struct tm, tm_wday  } p
    tm_yday   <- #{peek struct tm, tm_yday  } p
    tm_isdst  <- #{peek struct tm, tm_isdst } p
    tm_gmtoff <- #{peek struct tm, tm_gmtoff} p
    tm_zone   <- #{peek struct tm, tm_zone  } p
    return TM{..}
  poke p TM{..} = do
    #{poke struct tm, tm_sec   } p tm_sec    
    #{poke struct tm, tm_min   } p tm_min    
    #{poke struct tm, tm_hour  } p tm_hour   
    #{poke struct tm, tm_mday  } p tm_mday   
    #{poke struct tm, tm_mon   } p tm_mon    
    #{poke struct tm, tm_year  } p tm_year   
    #{poke struct tm, tm_wday  } p tm_wday   
    #{poke struct tm, tm_yday  } p tm_yday   
    #{poke struct tm, tm_isdst } p tm_isdst  
    #{poke struct tm, tm_gmtoff} p tm_gmtoff 
    #{poke struct tm, tm_zone  } p tm_zone   

foreign import ccall unsafe getdate_r :: CString -> Ptr TM -> IO CInt

getdateErr :: CInt -> String
getdateErr 1 = "The environment variable 'DATEMSK' is not defined or null."
getdateErr 2 = "The template file denoted by the 'DATEMSK' environment variable cannot be opened."
getdateErr 3 = "Information about the template file cannot retrieved."
getdateErr 4 = "The template file is not a regular file."
getdateErr 5 = "An I/O error occurred while reading the template file."
getdateErr 6 = "Not enough memory available to execute the function."
getdateErr 7 = "The template file contains no matching template."
getdateErr 8 = "The input date is invalid, but would match a template otherwise.  This includes dates like February 31st, and dates which cannot be represented in a 'time_t' variable."
getdateErr _ = "Undefined error"

getdate :: String -> IO (Either String TM)
getdate d = withCString d $ \s -> alloca $ \p -> do
  r <- getdate_r s p
  if r == 0
    then Right <$> peek p
    else return $ Left $ getdateErr r

tmToLocalTime :: TM -> LocalTime
tmToLocalTime TM{..} = LocalTime
  { localTimeOfDay = TimeOfDay (fromIntegral tm_hour) (fromIntegral tm_min) (fromIntegral tm_sec)
  , localDay = fromGregorian (1900 + fromIntegral tm_year) (fromIntegral tm_mon) (fromIntegral tm_mday)
  }

getLocalTime :: String -> IO (Either String LocalTime)
getLocalTime = fmap (fmap tmToLocalTime) . getdate

initTime :: FilePath -> IO ()
initTime datemsk = do
  c <- lookupEnv "DATEMSK"
  when (isNothing c) $ setEnv "DATEMSK" datemsk
