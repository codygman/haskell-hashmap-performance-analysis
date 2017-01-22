{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

main = do
  hm <- withFile "../words.txt" ReadMode $ \h -> do
    T.hGetLine h >>= \txt -> go 3 h txt HM.empty
  print (HM.lookup "'ll" hm)
  where go 0 _ ts hm = return hm
        go n h ts hm = hIsEOF h >>= \case
              True -> return hm
              False -> do
                txt <- T.hGetLine h
                go (n-1) h txt (HM.insert txt 0 hm)



-- pipes solution I canned because I can't be bothered to work through those types now
-- import Pipes
-- import qualified Pipes.Prelude as P
-- import Pipes.Safe
-- import qualified Pipes.Text as Text
-- import qualified Pipes.Text.IO as Text
-- import Pipes.Group (takes')
-- import Control.Lens
-- import qualified Control.Foldl as F

-- main :: IO ()
-- main = do
--   hm <- runSafeT . runEffect $ do
--     F.purely Text.folds
--      (F.Fold (\acc txt -> HM.insert txt 0 acc) HM.empty id)
--      (takes' 5 . view Text.lines $ Text.readFile "../words.txt")

--   print $ HM.lookup "'ll'" hm

-- src/Main.hs:17:14-23: error: …
--     • Couldn't match type ‘HM.HashMap Text.Text Integer’ with ‘X’
--       Expected type: (x -> Text.Text -> x)
--                      -> x
--                      -> (x -> HM.HashMap Text.Text Integer)
--                      -> Text.FreeT (Producer Text.Text (SafeT IO)) (SafeT IO) ()
--                      -> Effect (SafeT IO) (HM.HashMap k0 v0)
--         Actual type: (x -> Text.Text -> x)
--                      -> x
--                      -> (x -> HM.HashMap Text.Text Integer)
--                      -> Text.FreeT
--                           (Producer Text.Text (SafeT IO)) (SafeT IO) (HM.HashMap k0 v0)
--                      -> Producer
--                           (HM.HashMap Text.Text Integer) (SafeT IO) (HM.HashMap k0 v0)
--     • In the first argument of ‘F.purely’, namely ‘Text.folds’
--       In a stmt of a 'do' block:
--         F.purely
--           Text.folds
--           (F.Fold (\ acc txt -> HM.insert txt 0 acc) HM.empty id)
--           (takes' 5 . view Text.lines $ Text.readFile "../words.txt")
--       In the second argument of ‘($)’, namely
--         ‘do { F.purely
--                 Text.folds
--                 (F.Fold (\ acc txt -> HM.insert txt 0 acc) HM.empty id)
--                 (takes' 5 . view Text.lines $ Text.readFile "../words.txt") }’
-- src/Main.hs:21:3-29: error: …
--     • Ambiguous type variable ‘v0’ arising from a use of ‘print’
--       prevents the constraint ‘(Show v0)’ from being solved.
--       Relevant bindings include
--         hm :: HM.HashMap k0 v0
--           (bound at /home/cody/source/haskell-hashmap-performance-analysis/map-analysis/src/Main.hs:16:3)
--       Probable fix: use a type annotation to specify what ‘v0’ should be.
--       These potential instances exist:
--         instance (Show b, Show a) => Show (Either a b)
--           -- Defined in ‘Data.Either’
--         instance forall k a (b :: k). Show a => Show (Const a b)
--           -- Defined in ‘Data.Functor.Const’
--         instance Show a => Show (Identity a)
--           -- Defined in ‘Data.Functor.Identity’
--         ...plus 31 others
--         ...plus 193 instances involving out-of-scope types
--         (use -fprint-potential-instances to see them all)
--     • In a stmt of a 'do' block: print $ HM.lookup "'ll'" hm
--       In the expression:
--         do { hm <- runSafeT . runEffect
--                    $ do { F.purely
--                             Text.folds
--                             (F.Fold (\ acc txt -> HM.insert txt 0 acc) HM.empty id)
--                             (takes' 5 . view Text.lines $ Text.readFile "../words.txt") };
--              print $ HM.lookup "'ll'" hm }
--       In an equation for ‘main’:
--           main
--             = do { hm <- runSafeT . runEffect
--                          $ do { F.purely
--                                   Text.folds
--                                   (F.Fold (\ acc txt -> ...) HM.empty id)
--                                   (takes' 5 . view Text.lines $ Text.readFile "../words.txt") };
--                    print $ HM.lookup "'ll'" hm }
-- src/Main.hs:21:11-29: error: …
--     • Ambiguous type variable ‘k0’ arising from a use of ‘HM.lookup’
--       prevents the constraint ‘(Eq k0)’ from being solved.
--       Relevant bindings include
--         hm :: HM.HashMap k0 v0
--           (bound at /home/cody/source/haskell-hashmap-performance-analysis/map-analysis/src/Main.hs:16:3)
--       Probable fix: use a type annotation to specify what ‘k0’ should be.
--       These potential instances exist:
--         instance (Eq b, Eq a) => Eq (Either a b)
--           -- Defined in ‘Data.Either’
--         instance forall a k (b :: k). Eq a => Eq (Const a b)
--           -- Defined in ‘Data.Functor.Const’
--         instance Eq a => Eq (Identity a)
--           -- Defined in ‘Data.Functor.Identity’
--         ...plus 30 others
--         ...plus 186 instances involving out-of-scope types
--         (use -fprint-potential-instances to see them all)
--     • In the second argument of ‘($)’, namely ‘HM.lookup "'ll'" hm’
--       In a stmt of a 'do' block: print $ HM.lookup "'ll'" hm
--       In the expression:
--         do { hm <- runSafeT . runEffect
--                    $ do { F.purely
--                             Text.folds
--                             (F.Fold (\ acc txt -> HM.insert txt 0 acc) HM.empty id)
--                             (takes' 5 . view Text.lines $ Text.readFile "../words.txt") };
--              print $ HM.lookup "'ll'" hm }
-- src/Main.hs:21:21-26: error: …
--     • Ambiguous type variable ‘k0’ arising from the literal ‘"'ll'"’
--       prevents the constraint ‘(Data.String.IsString
--                                   k0)’ from being solved.
--       Relevant bindings include
--         hm :: HM.HashMap k0 v0
--           (bound at /home/cody/source/haskell-hashmap-performance-analysis/map-analysis/src/Main.hs:16:3)
--       Probable fix: use a type annotation to specify what ‘k0’ should be.
--       These potential instances exist:
--         instance Data.String.IsString a =>
--                  Data.String.IsString (Identity a)
--           -- Defined in ‘Data.Functor.Identity’
--         instance Data.String.IsString Text.ByteString
--           -- Defined in ‘bytestring-0.10.8.1:Data.ByteString.Internal’
--         instance Data.String.IsString Text.Text -- Defined in ‘Data.Text’
--         ...plus one other
--         ...plus 9 instances involving out-of-scope types
--         (use -fprint-potential-instances to see them all)
--     • In the first argument of ‘HM.lookup’, namely ‘"'ll'"’
--       In the second argument of ‘($)’, namely ‘HM.lookup "'ll'" hm’
--       In a stmt of a 'do' block: print $ HM.lookup "'ll'" hm
-- Compilation failed.



-- src/Main.hs:16:9: error: …
--     • Found hole: _ :: m0 () -> IO (HM.HashMap k0 v0)
--       Where: ‘k0’ is an ambiguous type variable
--              ‘v0’ is an ambiguous type variable
--              ‘m0’ is an ambiguous type variable
--     • In the first argument of ‘(.)’, namely ‘_’
--       In the expression: _ . runSafeT . runEffect
--       In a stmt of a 'do' block:
--         hm <- _ . runSafeT . runEffect
--               $ F.purely
--                   Text.folds
--                   (F.Fold (\ acc txt -> HM.insert txt 0 acc) HM.empty id)
--                   (takes' 5 . view Text.lines $ Text.readFile "../words.txt")



  
  -- print 1
  -- takeLines 3 (Text.readFile "../words.txt") >-> F.purely Text.folds (\acc txt -> HM.insert txt 0 acc) HM.empty id
  -- takeLines 3 (Text.readFile "../words.txt") >-> P.fold (\acc txt -> HM.insert txt 0 acc) HM.empty id ()
  -- where takeLines n = view Text.unlines . takes' n . view Text.lines

-- hm <- runSafeT $ runEffect $ P.fold
  --       (\acc txt -> HM.insert txt 0 acc)
  --       HM.empty
  --       id
  --       (takeLines 3 $ Text.readFile "../words.txt")
  -- hm <- runSafeT . runEffect $ F.purely Text.folds (F.Fold (\acc txt -> HM.insert txt 0 acc) HM.empty id)
  
