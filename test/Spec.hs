import DocumentFactoryTest
import ModelFactoryTest
import Test.Hspec

main :: IO ()
main = hspec $ do
  createDocuments
  createModels
