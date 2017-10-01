import           Test.Streaming.Producer (properties)
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]
