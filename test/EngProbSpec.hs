--------------------------------------------------
-- Copyright (C) 2017, All rights reserved.
--------------------------------------------------

module EngProbSpec (spec) where

import           EngProb
import           Test.Hspec

spec :: Spec
spec = do
    describe "sample" $
        it "runs" $ sample