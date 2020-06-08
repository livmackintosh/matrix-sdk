{- Copyright 2020 Olivia Mackintosh
   Low-level FFI wrapper around <olm/olm.h> -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Matrix.Olm.C where

import Foreign
import Foreign.C.Types


-- TODO: Define these types. can't find a struct in C header
data OlmAccount = OlmAccount

data OlmSession = OlmSession

data OlmUtility = OlmUtility


-- |Get the version number of the library. Arguments will be updated if
-- |non-null.
foreign import ccall safe "olm/olm.h olm_get_library_version"
  c_olm_get_library_version :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()


-- |The size of an account object in bytes
foreign import ccall safe "olm/olm.h olm_account_size"
  c_olm_account_size :: IO CUInt


-- |The size of an session object in bytes
foreign import ccall safe "olm/olm.h olm_session_size"
  c_olm_session_size :: IO CUInt


-- |The size of an utility object in bytes
foreign import ccall safe "olm/olm.h olm_utility_size"
  c_olm_utility_size :: IO CUInt


-- TODO: Type needs to implement Storable but not yet sure what the struct
-- looks like
-- foreign import ccall safe "olm/olm.h olm_account"
--   c_olm_account :: Storable a => a -> IO OlmAccount
-- foreign import ccall safe "olm/olm.h olm_session"
--   c_olm_session :: Storable a => a -> IO OlmSession
-- foreign import ccall safe "olm/olm.h olm_utility"
--   c_olm_utility:: Storable a => a -> IO OlmUtility


-- |The value that olm will return from a function if there was an error
foreign import ccall safe "olm/olm.h olm_error" c_olm_error :: CUInt


-- |A null terminated string describing the most recent error to happen to an
-- |account
foreign import ccall safe "olm/olm.h olm_account_last_error"
  c_olm_account_last_error :: Ptr OlmAccount -> Ptr Char


-- |A null terminated string describing the most recent error to happen to an
-- |account
foreign import ccall safe "olm/olm.h olm_session_last_error"
  c_olm_session_last_error :: Ptr OlmSession -> Ptr Char


-- |A null terminated string describing the most recent error to happen to an
-- |account
foreign import ccall safe "olm/olm.h olm_utility_last_error"
  c_olm_utility_last_error :: Ptr OlmUtility -> Ptr Char


-- |Clears the memory used to back this account
foreign import ccall safe "olm/olm.h olm_clear_account"
  c_olm_clear_account :: Ptr OlmAccount -> IO CUInt


-- |Clears the memory used to back this session
foreign import ccall safe "olm/olm.h olm_clear_session"
  c_olm_clear_session :: Ptr OlmSession -> IO CUInt


-- |Clears the memory used to back this utility
foreign import ccall safe "olm/olm.h olm_clear_utility"
  c_olm_clear_utility :: Ptr OlmUtility -> IO CUInt


-- |Returns the number of bytes needed to store an account
foreign import ccall safe "olm/olm.h olm_pickle_account_length"
  c_olm_pickle_account_length :: Ptr OlmAccount -> IO CUInt


-- |Returns the number of bytes needed to store a session
foreign import ccall safe "olm/olm.h olm_pickle_session_length"
  c_olm_pickle_session_length :: Ptr OlmSession -> IO CUInt


foreign import ccall safe "olm/olm.h olm_pickle_account"
  c_olm_pickle_account :: IO CUInt


foreign import ccall safe "olm/olm.h olm_pickle_session"
  c_olm_pickle_session :: IO Word64


foreign import ccall safe "olm/olm.h olm_unpickle_account"
  c_olm_unpickle_account :: IO Word64


foreign import ccall safe "olm/olm.h olm_unpickle_session"
  c_olm_unpickle_session :: IO Word64


foreign import ccall safe "olm/olm.h olm_create_account_random_length"
  c_olm_create_account_random_length :: IO Word64


foreign import ccall safe "olm/olm.h olm_create_account"
  c_olm_create_account :: IO Word64


foreign import ccall safe "olm/olm.h olm_account_identity_keys_length"
  c_olm_account_identity_keys_length :: IO Word64


--foreign import ccall unsafe "olm/olm.h  olm_account_signature_length"

--  /** The length of an ed25519 signature encoded as base64. */
--  size_t olm_account_signature_length(
--      OlmAccount * account
--  );
--
--  /** Signs a message with the ed25519 key for this account. Returns olm_error()
--   * on failure. If the signature buffer was too small then
--   * olm_account_last_error() will be "OUTPUT_BUFFER_TOO_SMALL" */
--  size_t olm_account_sign(
--      OlmAccount * account,
--      void const * message, size_t message_length,
--      void * signature, size_t signature_length
--  );
--
--  /** The size of the output buffer needed to hold the one time keys */
--  size_t olm_account_one_time_keys_length(
--      OlmAccount * account
--  );
--
--  /** Writes the public parts of the unpublished one time keys for the account
--   * into the one_time_keys output buffer.
--   * <p>
--   * The returned data is a JSON-formatted object with the single property
--   * <tt>curve25519</tt>, which is itself an object mapping key id to
--   * base64-encoded Curve25519 key. For example:
--   * <pre>
--   * {
--   *     curve25519: {
--   *         "AAAAAA": "wo76WcYtb0Vk/pBOdmduiGJ0wIEjW4IBMbbQn7aSnTo",
--   *         "AAAAAB": "LRvjo46L1X2vx69sS9QNFD29HWulxrmW11Up5AfAjgU"
--   *     }
--   * }
--   * </pre>
--   * Returns olm_error() on failure.
--   * <p>
--   * If the one_time_keys buffer was too small then olm_account_last_error()
--   * will be "OUTPUT_BUFFER_TOO_SMALL". */
--  size_t olm_account_one_time_keys(
--      OlmAccount * account,
--      void * one_time_keys, size_t one_time_keys_length
--  );
--
--  /** Marks the current set of one time keys as being published. */
--  size_t olm_account_mark_keys_as_published(
--      OlmAccount * account
--  );
--
--  /** The largest number of one time keys this account can store. */
--  size_t olm_account_max_number_of_one_time_keys(
--      OlmAccount * account
--  );
--
--  /** The number of random bytes needed to generate a given number of new one
--   * time keys. */
--  size_t olm_account_generate_one_time_keys_random_length(
--      OlmAccount * account,
--      size_t number_of_keys
--  );
--
--  /** Generates a number of new one time keys. If the total number of keys stored
--   * by this account exceeds max_number_of_one_time_keys() then the old keys are
--   * discarded. Returns olm_error() on error. If the number of random bytes is
--   * too small then olm_account_last_error() will be "NOT_ENOUGH_RANDOM". */
--  size_t olm_account_generate_one_time_keys(
--      OlmAccount * account,
--      size_t number_of_keys,
--      void * random, size_t random_length
--  );
--
--  /** The number of random bytes needed to create an outbound session */
--  size_t olm_create_outbound_session_random_length(
--      OlmSession * session
--  );
--
--  /** Creates a new out-bound session for sending messages to a given identity_key
--   * and one_time_key. Returns olm_error() on failure. If the keys couldn't be
--   * decoded as base64 then olm_session_last_error() will be "INVALID_BASE64"
--   * If there weren't enough random bytes then olm_session_last_error() will
--   * be "NOT_ENOUGH_RANDOM". */
--  size_t olm_create_outbound_session(
--      OlmSession * session,
--      OlmAccount * account,
--      void const * their_identity_key, size_t their_identity_key_length,
--      void const * their_one_time_key, size_t their_one_time_key_length,
--      void * random, size_t random_length
--  );
--
--  /** Create a new in-bound session for sending/receiving messages from an
--   * incoming PRE_KEY message. Returns olm_error() on failure. If the base64
--   * couldn't be decoded then olm_session_last_error will be "INVALID_BASE64".
--   * If the message was for an unsupported protocol version then
--   * olm_session_last_error() will be "BAD_MESSAGE_VERSION". If the message
--   * couldn't be decoded then then olm_session_last_error() will be
--   * "BAD_MESSAGE_FORMAT". If the message refers to an unknown one time
--   * key then olm_session_last_error() will be "BAD_MESSAGE_KEY_ID". */
--  size_t olm_create_inbound_session(
--      OlmSession * session,
--      OlmAccount * account,
--      void * one_time_key_message, size_t message_length
--  );
--
--  /** Create a new in-bound session for sending/receiving messages from an
--   * incoming PRE_KEY message. Returns olm_error() on failure. If the base64
--   * couldn't be decoded then olm_session_last_error will be "INVALID_BASE64".
--   * If the message was for an unsupported protocol version then
--   * olm_session_last_error() will be "BAD_MESSAGE_VERSION". If the message
--   * couldn't be decoded then then olm_session_last_error() will be
--   * "BAD_MESSAGE_FORMAT". If the message refers to an unknown one time
--   * key then olm_session_last_error() will be "BAD_MESSAGE_KEY_ID". */
--  size_t olm_create_inbound_session_from(
--      OlmSession * session,
--      OlmAccount * account,
--      void const * their_identity_key, size_t their_identity_key_length,
--      void * one_time_key_message, size_t message_length
--  );
--
--  /** The length of the buffer needed to return the id for this session. */
--  size_t olm_session_id_length(
--      OlmSession * session
--  );
--
--  /** An identifier for this session. Will be the same for both ends of the
--   * conversation. If the id buffer is too small then olm_session_last_error()
--   * will be "OUTPUT_BUFFER_TOO_SMALL". */
--  size_t olm_session_id(
--      OlmSession * session,
--      void * id, size_t id_length
--  );
--
--  int olm_session_has_received_message(
--      OlmSession *session
--  );
--
--  /**
--   * Write a null-terminated string describing the internal state of an olm
--   * session to the buffer provided for debugging and logging purposes.
--   */
--  void olm_session_describe(OlmSession * session, char *buf, size_t buflen);
--
--  /** Checks if the PRE_KEY message is for this in-bound session. This can happen
--   * if multiple messages are sent to this account before this account sends a
--   * message in reply. The one_time_key_message buffer is destroyed. Returns 1 if
--   * the session matches. Returns 0 if the session does not match. Returns
--   * olm_error() on failure. If the base64 couldn't be decoded then
--   * olm_session_last_error will be "INVALID_BASE64".  If the message was for an
--   * unsupported protocol version then olm_session_last_error() will be
--   * "BAD_MESSAGE_VERSION". If the message couldn't be decoded then then
--   * olm_session_last_error() will be "BAD_MESSAGE_FORMAT". */
--  size_t olm_matches_inbound_session(
--      OlmSession * session,
--      void * one_time_key_message, size_t message_length
--  );
--
--  /** Checks if the PRE_KEY message is for this in-bound session. This can happen
--   * if multiple messages are sent to this account before this account sends a
--   * message in reply. The one_time_key_message buffer is destroyed. Returns 1 if
--   * the session matches. Returns 0 if the session does not match. Returns
--   * olm_error() on failure. If the base64 couldn't be decoded then
--   * olm_session_last_error will be "INVALID_BASE64".  If the message was for an
--   * unsupported protocol version then olm_session_last_error() will be
--   * "BAD_MESSAGE_VERSION". If the message couldn't be decoded then then
--   * olm_session_last_error() will be "BAD_MESSAGE_FORMAT". */
--  size_t olm_matches_inbound_session_from(
--      OlmSession * session,
--      void const * their_identity_key, size_t their_identity_key_length,
--      void * one_time_key_message, size_t message_length
--  );
--
--  /** Removes the one time keys that the session used from the account. Returns
--   * olm_error() on failure. If the account doesn't have any matching one time
--   * keys then olm_account_last_error() will be "BAD_MESSAGE_KEY_ID". */
--  size_t olm_remove_one_time_keys(
--      OlmAccount * account,
--      OlmSession * session
--  );
--
--  /** The type of the next message that olm_encrypt() will return. Returns
--   * OLM_MESSAGE_TYPE_PRE_KEY if the message will be a PRE_KEY message.
--   * Returns OLM_MESSAGE_TYPE_MESSAGE if the message will be a normal message.
--   * Returns olm_error on failure. */
--  size_t olm_encrypt_message_type(
--      OlmSession * session
--  );
--
--  /** The number of random bytes needed to encrypt the next message. */
--  size_t olm_encrypt_random_length(
--      OlmSession * session
--  );
--
--  /** The size of the next message in bytes for the given number of plain-text
--   * bytes. */
--  size_t olm_encrypt_message_length(
--      OlmSession * session,
--      size_t plaintext_length
--  );
--
--  /** Encrypts a message using the session. Returns the length of the message in
--   * bytes on success. Writes the message as base64 into the message buffer.
--   * Returns olm_error() on failure. If the message buffer is too small then
--   * olm_session_last_error() will be "OUTPUT_BUFFER_TOO_SMALL". If there
--   * weren't enough random bytes then olm_session_last_error() will be
--   * "NOT_ENOUGH_RANDOM". */
--  size_t olm_encrypt(
--      OlmSession * session,
--      void const * plaintext, size_t plaintext_length,
--      void * random, size_t random_length,
--      void * message, size_t message_length
--  );
--
--  /** The maximum number of bytes of plain-text a given message could decode to.
--   * The actual size could be different due to padding. The input message buffer
--   * is destroyed. Returns olm_error() on failure. If the message base64
--   * couldn't be decoded then olm_session_last_error() will be
--   * "INVALID_BASE64". If the message is for an unsupported version of the
--   * protocol then olm_session_last_error() will be "BAD_MESSAGE_VERSION".
--   * If the message couldn't be decoded then olm_session_last_error() will be
--   * "BAD_MESSAGE_FORMAT". */
--  size_t olm_decrypt_max_plaintext_length(
--      OlmSession * session,
--      size_t message_type,
--      void * message, size_t message_length
--  );
--
--  /** Decrypts a message using the session. The input message buffer is destroyed.
--   * Returns the length of the plain-text on success. Returns olm_error() on
--   * failure. If the plain-text buffer is smaller than
--   * olm_decrypt_max_plaintext_length() then olm_session_last_error()
--   * will be "OUTPUT_BUFFER_TOO_SMALL". If the base64 couldn't be decoded then
--   * olm_session_last_error() will be "INVALID_BASE64". If the message is for
--   * an unsupported version of the protocol then olm_session_last_error() will
--   * be "BAD_MESSAGE_VERSION". If the message couldn't be decoded then
--   * olm_session_last_error() will be BAD_MESSAGE_FORMAT".
--   * If the MAC on the message was invalid then olm_session_last_error() will
--   * be "BAD_MESSAGE_MAC". */
--  size_t olm_decrypt(
--      OlmSession * session,
--      size_t message_type,
--      void * message, size_t message_length,
--      void * plaintext, size_t max_plaintext_length
--  );
--
--  /** The length of the buffer needed to hold the SHA-256 hash. */
--  size_t olm_sha256_length(
--     OlmUtility * utility
--  );
--
--  /** Calculates the SHA-256 hash of the input and encodes it as base64. If the
--   * output buffer is smaller than olm_sha256_length() then
--   * olm_utility_last_error() will be "OUTPUT_BUFFER_TOO_SMALL". */
--  size_t olm_sha256(
--      OlmUtility * utility,
--      void const * input, size_t input_length,
--      void * output, size_t output_length
--  );
--
--  /** Verify an ed25519 signature. If the key was too small then
--   * olm_utility_last_error() will be "INVALID_BASE64". If the signature was invalid
--   * then olm_utility_last_error() will be "BAD_MESSAGE_MAC". */
--  size_t olm_ed25519_verify(
--      OlmUtility * utility,
--      void const * key, size_t key_length,
--      void const * message, size_t message_length,
--      void * signature, size_t signature_length
--  );
