import DocumentTest
import RegressionTest
import ModelFactoryTest
import Test.Hspec
import TopicCountsTest

main :: IO ()
main = hspec $ do
  createTopicCounts
  createDocuments
  createModels
  estimateDocuments
