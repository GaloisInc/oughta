module Oughta.Hooks
  ( Hooks(..)
  , defaultHooks
  ) where

import HsLua (Lua)

data Hooks
  = Hooks
    { -- | Hook that runs before execution of the Lua program
      preHook :: Lua ()
      -- | Hook that runs after successful execution of the Lua program
    , postHook :: Lua ()
    }

defaultHooks :: Hooks
defaultHooks =
  Hooks
  { preHook = pure ()
  , postHook = pure ()
  }
