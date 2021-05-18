import DocumentTest
import NewLDATest
import Test.Hspec

main :: IO ()
main = hspec $ do
  createDocumentTests
  initialModelTest
