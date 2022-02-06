module HW3.Pretty(
    prettyValue
) where

import HW3.Base (
        HiFun(..),
        HiValue(..),
        HiExpr(..),
        HiError(..))

import Prettyprinter.Internal ()
import Data.Text.Prettyprint.Doc.Render.Terminal ( AnsiStyle )
import Data.Ratio
import Data.Text.Prettyprint.Doc

prettyValue :: HiValue -> Doc AnsiStyle
-- prettyValue (HiValueNumber rat) = pretty $ showRational rat
prettyValue val = viaShow val
-- prettyValue hiNum@(HiValueNumber num) = viaShow hiNum
-- prettyValue hiFun@(HiValueFunction f) = viaShow hiFun
-- prettyValue hiBool@(HiValueBool b) = viaShow hiBool