import DocumentTest
import Test.HUnit (runTestTT, Counts)

main :: IO Counts
main = runTestTT createDocumentTests
