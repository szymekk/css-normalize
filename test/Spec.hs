import BalancedSpec
import NormalizeSpec
import ParseSpec
import SelectorsSpec
import Test.Hspec

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
  BalancedSpec.spec
  NormalizeSpec.spec
  ParseSpec.spec
  SelectorsSpec.spec
