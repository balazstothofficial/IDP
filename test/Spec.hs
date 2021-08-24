import DocumentFactoryTest
import LDAEstimatorTest
import ModelFactoryTest
import Test.Hspec
import TopicCountsFactoryTest

main :: IO ()
main = hspec $ do
  createTopicCounts
  createDocuments
  createModels
  estimateDocuments
