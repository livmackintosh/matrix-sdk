{- Copyright 2020 Olivia Mackintosh -}
module Matrix.Olm.Binding where

import Matrix.Olm.Types

import Data.Aeson
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics (Generic)

import qualified Data.ByteString.Char8 as B

#include <olm/olm.h>


data Account
{#pointer *OlmAccount as AccountPtr -> Account #}

data Session
{#pointer *OlmSession as SessionPtr -> Session #}

data Utility
{#pointer *OlmUtility as UtilityPtr -> Utility #}


olmGetLibraryVersion :: IO OlmVer
olmGetLibraryVersion =
  allocaBytes 3 $ \p -> do
    {#call olm_get_library_version#} p (plusPtr p 1) (plusPtr p 2)
    major <- peek p
    minor <- peek $ plusPtr p 1
    patch <- peek $ plusPtr p 2
    pure $ OlmVer (major, minor, patch)


newAccount :: IO AccountPtr
newAccount = do
  ptrA <- mallocBytes (fromEnum {#call pure olm_account_size#})
  acctPtr <- {#call olm_account#} ptrA
  randSize <- {#call olm_create_account_random_length#} acctPtr
  ptrB <- mallocBytes (fromEnum randSize)
  -- TODO: Write random data of randSize at ptrB.
  {#call olm_create_account#} acctPtr ptrB randSize
  pure acctPtr


clearAccount :: AccountPtr -> IO CULong
-- TODO: Incoporate this into account finalizer
clearAccount = {#call olm_clear_account #}


identKeys :: AccountPtr -> IO (Maybe IdentityKeys)
identKeys a = allocaBytes (fromEnum size) $ \p -> do
  writSize <- {#call olm_account_identity_keys#} a p (size)
  iks <- peekCString (castPtr p)
  pure $ decodeStrict $ B.take (fromEnum writSize) $ B.pack iks
  where size = {#call pure olm_account_identity_keys_length#} a


oneTimeKeys :: AccountPtr -> IO (Maybe OneTimeKeys)
oneTimeKeys a = allocaBytes (fromEnum size) $ \p -> do
  writSize <- {#call olm_account_one_time_keys#} a p (size)
  otks <- peekCString (castPtr p)
  pure $ decodeStrict $ B.take (fromEnum writSize) $ B.pack otks
  where size = {#call pure olm_account_one_time_keys_length#} a


genOneTimeKeys :: Int -> AccountPtr -> IO CULong
genOneTimeKeys n a = allocaBytes (fromEnum size) $ \p -> do
  -- TODO: Write random data to p
  {#call olm_account_generate_one_time_keys #} a nK p size
  where
  nK = toEnum n :: CULong
  size = {#call pure olm_account_generate_one_time_keys_random_length#} a nK


markKeysAsPublished :: AccountPtr -> IO CULong
markKeysAsPublished = {#call olm_account_mark_keys_as_published #}

--accountLastError :: AccountPtr -> IO String
--accountLastError ap = do
--  ptrChar <- {#call olm_account_last_error#} ap
--  a <- peek ptrChar
--  pure (show a)

--olmAccount :: Ptr () -> IO AccountPtr
--olmAccount = {#call olm_account #}
--
--olmSession :: Ptr () -> IO SessionPtr
--olmSession = {#call olm_session #}
--
--olmUtility :: Ptr () -> IO UtilityPtr
--olmUtility = {#call olm_utility #}
--
--olmAccountLastError :: AccountPtr -> IO (Ptr CChar)
--olmAccountLastError = {#call olm_account_last_error #}
--
--olmAccountSize :: CULong
--olmAccountSize = {#call pure olm_account_size#}
--
--olmSessionSize :: CULong
--olmSessionSize = {#call pure olm_session_size#}
--
--olmUtilitySize :: CULong
--olmUtilitySize = {#call pure olm_utility_size #}
--
--
--olmClearSession :: SessionPtr -> IO CULong
--olmClearSession = {#call olm_clear_session #}
--
--olmClearUtility :: UtilityPtr -> IO CULong
--olmClearUtility = {#call olm_clear_utility #}
--
--olmCreateAccountRandomLength :: AccountPtr -> IO CULong
--olmCreateAccountRandomLength = {#call olm_create_account_random_length #}
--
--olmCreateAccount :: AccountPtr -> Ptr () -> CULong -> IO CULong
--olmCreateAccount = {#call olm_create_account #}
--
