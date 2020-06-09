module Matrix.Olm.Binding where

import Foreign
import Foreign.C.Types
import Foreign.Ptr

#include <olm/olm.h>

olmGetLibraryVersion :: Ptr CUChar -> Ptr CUChar -> Ptr CUChar -> IO ()
olmGetLibraryVersion = {#call olm_get_library_version #}

data Account
{#pointer *OlmAccount as AccountPtr -> Account #}

data Session
{#pointer *OlmSession as SessionPtr -> Session #}

data Utility
{#pointer *OlmUtility as UtilityPtr -> Utility #}

olmAccount :: Ptr () -> IO AccountPtr
olmAccount = {#call olm_account #}

olmSession :: Ptr () -> IO SessionPtr
olmSession = {#call olm_session #}

olmUtility :: Ptr () -> IO UtilityPtr
olmUtility = {#call olm_utility #}

olmAccountSize :: CULong
olmAccountSize = {#call pure olm_account_size#}

olmSessionSize:: CULong
olmSessionSize= {#call pure olm_session_size#}

olmUtilitySize :: CULong
olmUtilitySize = {#call pure olm_utility_size #}
