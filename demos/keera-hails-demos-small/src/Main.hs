-- Internal imports
import Controller (controller)
import Model      (reactiveModel)
import View       (buildUI)

main :: IO ()
main = do
  ui    <- buildUI
  model <- reactiveModel
  controller ui model
