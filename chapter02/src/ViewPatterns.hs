{-# LANGUAGE ViewPatterns #-}

module ViewPatterns where

import Section02 (Client (Company), clientName)

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

{-
> import Section02
> import ViewPatterns
> specialClient (GovOrg "NASA")
True
-}
specialClient :: Client -> Bool
specialClient (clientName -> "NASA") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False