{- Copyright 2020 Olivia Mackintosh -}
module Matrix.Olm.Binding where

import Matrix.Olm.Types

import Crypto.Random(MonadRandom, getSystemDRG, withRandomBytes, getRandomBytes)
import Data.Aeson
import Data.ByteArray(copyByteArrayToPtr)
import Foreign
import Foreign.C.String
import Foreign.C.Types

import qualified Data.ByteString.Char8 as B

#include <olm/olm.h>


-- use ForeignPtr with finalizer for each of these
-- primary opaque pointer types
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
  -- TODO: add some way of freeing this memory automatically when ptr not
  -- accessible e.g. ForeignPtr
  acctPtr <- mallocBytes (fromEnum {#call pure olm_account_size#}) >>= \p ->
    {#call olm_account#} p
  randSize <- {#call olm_create_account_random_length#} acctPtr
  allocaBytes (fromEnum randSize) $ \rndPtr -> do
    getSystemDRG >>= \g ->
      fst $ withRandomBytes g (fromEnum randSize) (copyBSToPtr rndPtr)
    {#call olm_create_account#} acctPtr rndPtr randSize
  pure acctPtr


clearAccount :: AccountPtr -> IO CULong
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
  where
  size = {#call pure olm_account_one_time_keys_length#} a


type RandomData = B.ByteString
genOneTimeKeys :: RandomData -> CULong -> AccountPtr -> IO CULong
genOneTimeKeys r nK a = allocaBytes (fromEnum size) $ \rndPtr -> do
  withCString (B.unpack r) $ \r' -> poke (castPtr rndPtr) r'
  {#call olm_account_generate_one_time_keys #} a nK rndPtr size
  where
  size = {#call pure olm_account_generate_one_time_keys_random_length#} a nK


markKeysAsPublished :: AccountPtr -> IO CULong
markKeysAsPublished = {#call olm_account_mark_keys_as_published #}


signMessage :: AccountPtr -> String -> IO String
signMessage a m = allocaBytes (sM + sS) $ \p -> do
  let pM = p
      pS = plusPtr p sM
  withCString m $ \m' -> poke pM m'
  {#call olm_account_sign #} a (castPtr pM) (toEnum sM) (castPtr pS) (toEnum sS)
  peekCString (castPtr pS)
  where
  sM = length m
  sS = fromEnum $ {#call pure olm_account_signature_length#} a


copyBSToPtr :: Ptr a -> B.ByteString -> IO ()
copyBSToPtr = flip copyByteArrayToPtr

generateRandom :: MonadRandom m => Int -> m B.ByteString
generateRandom s = getRandomBytes s
