import DocumentTest
import LDATest
import Test.Hspec

main :: IO ()
main = hspec $ do
  createDocumentTests
  initialModelTest
