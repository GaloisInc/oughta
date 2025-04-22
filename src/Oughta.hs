-- | See the package [README] for a high-level description.
--
-- [README]: https://github.com/GaloisInc/oughta/blob/main/README.md
module Oughta
  ( -- * Running checks
    Output(..)
  , check
  , check'
    -- * Loading Lua programs
  , OE.LuaProgram
  , OE.addPrefix
  , OE.plainLuaProgram
  , OE.fromLines
  , OE.fromLineComments
    -- * Interpreting results
  , OR.Failure(..)
  , OR.Progress(..)
  , OR.Success(..)
  , OR.Result(..)
  , OR.resultNull
  , OR.printResult
    -- ** Source locations
  , Loc(..)
  , Pos(..)
  , OP.Span(..)
  ) where

import Control.Exception qualified as X
import Data.ByteString (ByteString)
import Oughta.Extract (LuaProgram)
import Oughta.Extract qualified as OE
import Oughta.LuaApi qualified as FCLA
import Oughta.Pos (Loc(..), Pos(..))
import Oughta.Pos qualified as OP
import Oughta.Result qualified as OR
import GHC.Stack (HasCallStack)
import Prelude hiding (lines, span)

-- | Output of the program under test
newtype Output = Output ByteString

-- | Check some program output against a Oughta Lua program.
check ::
  LuaProgram ->
  Output ->
  IO OR.Result
check prog (Output out) = FCLA.check prog out

-- | Like 'check', but throws an exception on failure.
check' ::
  HasCallStack =>
  LuaProgram ->
  Output ->
  IO ()
check' prog out = do
  OR.Result r <- check prog out
  case r of
    Left f -> X.throwIO f
    Right {} -> pure ()
