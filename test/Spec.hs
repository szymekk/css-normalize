import BalancedSpec
import ParseSpec
import Test.Hspec

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
  BalancedSpec.spec
  ParseSpec.spec
