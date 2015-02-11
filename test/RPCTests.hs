module RPCTests (rpcs) where

import           TestPrelude
import qualified Data.Vector as V

rpcs :: Spec
rpcs = describe "RPC_PING_RES" $ do
  describe "is awesome" $ do
    it "does awesome things" $
      return True `shouldReturn` True
