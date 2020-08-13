module Controller where

-- External imports
import Control.Monad (forM_)

-- External imports: Keera Hails project
import Data.ReactiveValue ((<:=), (<^>), (=:>))

-- Internal imports
import Data.Action (Action (Clear, Equals))
import Model       (Model, modelAddDigit, modelApplyAction, modelApplyOperator,
                    modelValue, reactiveModel)
import View        (UI (..))

-- | Connect view and model using reactive rules
controller :: UI -> Model -> IO ()
controller ui model = do
  textField ui <:= (show <^> modelValue model)

  forM_ (numbers ui) $ \button ->
    button =:> modelAddDigit model

  forM_ (operators ui) $ \button ->
    button =:> modelApplyOperator model

  forM_ (actions ui) $ \button ->
    button =:> modelApplyAction model
