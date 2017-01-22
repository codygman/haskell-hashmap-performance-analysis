module Main where

import qualified Data.HashMap.Strict as HM
import Pipes
import Pipes.Safe
import qualified Pipes.Text as Text
import qualified Pipes.Text.IO as Text
import Pipes.Group (takes')
import Control.Lens

main :: IO ()
main = runSafeT $ runEffect $ do
  takeLines 3 (Text.readFile "../words.txt") >-> Text.stdout
  where takeLines n = view Text.unlines . takes' n . view Text.lines
