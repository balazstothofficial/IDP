{-# LANGUAGE DisambiguateRecordFields #-}

module RegressionTest where

import qualified Data.Map as Map
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
import Document (Document)
import HyperParameter
import InitialTopics
import LDAEstimator (estimate)
import qualified LDAEstimator
import LDAFinalizer
import Model (Model)
import qualified Model
import qualified ModelFactory
import Test.HUnit
import Test.Hspec

estimateDocuments :: Spec
estimateDocuments = do
  describe "Estimate" $ do
    it "Unit Test 1 - Compare With JGibbLDA implementation" $ do
      computePhi
        ( computeTheta $
            estimate
              LDAEstimator.Input
                { model = initialModel,
                  iterations = 9,
                  factors = factors
                }
        )
        @?= finalModel

finalModel :: Model
finalModel =
  Model.Model
    { hyperParameter =
        HyperParameter
          { alpha = 10.0,
            beta = 0.1
          },
      numberOfTopics = 5,
      vocabularySize = 16,
      numberOfWords = 16,
      numberOfDocuments = 5,
      numberOfUpdates = 0, --TODO
      topicCounts = Map.fromList [(0, 1), (1, 4), (2, 6), (3, 1), (4, 4)],
      documents = documents,
      vocabulary =
        Set.fromList
          [ "Halli",
            "Hallo",
            "Tralla",
            "Test",
            "Testi",
            "Testo",
            "Gollo",
            "Trollo",
            "Lollo",
            "Gau",
            "Lau",
            "Flau",
            "Tau",
            "Tatz",
            "Fratz",
            "Katz"
          ],
      topicAssignments = [[4, 2, 2], [2, 2, 1], [2, 1, 4], [3, 1, 0, 1], [4, 4, 2]],
      theta =
        Matrix.fromList
          5
          5
          [ 0.18867924528301888,
            0.18867924528301888,
            0.22641509433962265,
            0.18867924528301888,
            0.20754716981132076,
            0.18867924528301888,
            0.20754716981132076,
            0.22641509433962265,
            0.18867924528301888,
            0.18867924528301888,
            0.18867924528301888,
            0.20754716981132076,
            0.20754716981132076,
            0.18867924528301888,
            0.20754716981132076,
            0.2037037037037037,
            0.2222222222222222,
            0.18518518518518517,
            0.2037037037037037,
            0.18518518518518517,
            0.18867924528301888,
            0.18867924528301888,
            0.20754716981132076,
            0.18867924528301888,
            0.22641509433962265
          ],
      phi =
        Matrix.fromList
          5
          16
          [ 0.4230769230769231,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            0.19642857142857145,
            1.785714285714286e-2,
            1.785714285714286e-2,
            0.19642857142857145,
            1.785714285714286e-2,
            1.785714285714286e-2,
            0.19642857142857145,
            1.785714285714286e-2,
            0.19642857142857145,
            1.3157894736842106e-2,
            1.3157894736842106e-2,
            1.3157894736842106e-2,
            0.14473684210526316,
            1.3157894736842106e-2,
            0.14473684210526316,
            0.14473684210526316,
            1.3157894736842106e-2,
            1.3157894736842106e-2,
            1.3157894736842106e-2,
            1.3157894736842106e-2,
            0.14473684210526316,
            0.14473684210526316,
            1.3157894736842106e-2,
            0.14473684210526316,
            1.3157894736842106e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            0.4230769230769231,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            3.8461538461538464e-2,
            1.785714285714286e-2,
            0.19642857142857145,
            1.785714285714286e-2,
            1.785714285714286e-2,
            0.19642857142857145,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            0.19642857142857145,
            0.19642857142857145,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2,
            1.785714285714286e-2
          ],
      wordTopicMap =
        Map.fromList
          [ (("Flau", 0), 1),
            (("Fratz", 4), 1),
            (("Gau", 3), 1),
            (("Gollo", 2), 1),
            (("Halli", 4), 1),
            (("Hallo", 2), 1),
            (("Katz", 2), 1),
            (("Lau", 1), 1),
            (("Lollo", 4), 1),
            (("Tatz", 4), 1),
            (("Tau", 1), 1),
            (("Test", 2), 1),
            (("Testi", 2), 1),
            (("Testo", 1), 1),
            (("Tralla", 2), 1),
            (("Trollo", 1), 1)
          ],
      documentTopicMap =
        Map.fromList
          [ ((head documents, 2), 2),
            ((head documents, 4), 1),
            ((documents !! 1, 1), 1),
            ((documents !! 1, 2), 2),
            ((documents !! 2, 1), 1),
            ((documents !! 2, 2), 1),
            ((documents !! 2, 4), 1),
            ((documents !! 3, 0), 1),
            ((documents !! 3, 1), 2),
            ((documents !! 3, 3), 1),
            ((documents !! 4, 2), 1),
            ((documents !! 4, 4), 2)
          ]
    }

initialModel :: Model
initialModel =
  create
    ModelFactory.Input
      { documents = documents,
        topics = create [2 :: Int, 2, 1, 1, 1, 4, 3, 4, 4, 1, 0, 2, 0, 4, 4, 1],
        numberOfTopics = 5
      }

documents :: [Document]
documents =
  [ create "Doc1" ["Halli", "Hallo", "Tralla"],
    create "Doc2" ["Test", "Testi", "Testo"],
    create "Doc3" ["Gollo", "Trollo", "Lollo"],
    create "Doc4" ["Gau", "Lau", "Flau", "Tau"],
    create "Doc5" ["Tatz", "Fratz", "Katz"]
  ]

factors :: [Double]
factors =
  [ 0.19826677458072428,
    0.6468626448816774,
    0.6242907637877683,
    0.13472351118831172,
    0.2996095213241087,
    0.06338508740831261,
    0.851482218394351,
    0.13531232208333854,
    0.6825707535607076,
    0.4351421074443904,
    0.6869899657255265,
    0.5533763072324237,
    0.10148853288780901,
    0.9263952259563535,
    0.6936378565033825,
    0.8972143126528224,
    0.07533668993487308,
    0.8362812291575583,
    0.8670155729850891,
    0.9706648749847882,
    0.07037494917230902,
    0.0495148500844842,
    0.9456662824932296,
    0.21058161323948676,
    0.05940257053481901,
    0.13442361524494262,
    0.8615643624956302,
    0.8978521694533362,
    0.07507710690009373,
    0.6249666098518136,
    0.5074278808876835,
    0.7322260562758176,
    0.3132371213465963,
    0.0634276031040838,
    0.7015110531316692,
    0.6918798253308591,
    0.06189425497086953,
    0.4517624289380403,
    0.2158742338273153,
    0.06056306606705042,
    0.232536480921666,
    0.8621193965408486,
    7.304072133264006E-4,
    0.9983318574356793,
    0.5526108275133798,
    0.09084052395217745,
    0.28531840879234316,
    0.056291253706319155,
    0.5415305412333148,
    0.4052906366839749,
    0.06485505829569771,
    0.5914317637803627,
    0.2372785295516343,
    0.6291502256151881,
    0.593469641652524,
    0.12455894977812687,
    0.2666547070806681,
    0.03934704031592384,
    0.8946258938897087,
    0.3024799423328731,
    0.25944223411949585,
    0.8065908395347139,
    0.9733882993253349,
    0.27502371605925924,
    0.795243857047851,
    0.0015139652212575738,
    0.3689907078950031,
    0.9781370398588461,
    0.041465887097950405,
    0.5097106390037597,
    0.9215174868861585,
    0.026211253680581192,
    0.9919044731872734,
    0.5515054960359497,
    0.14310794393664517,
    0.6599597937320931,
    0.7476148581932625,
    0.8860725181141602,
    0.3819477124029639,
    0.4705798011129011,
    0.5952211487398825,
    0.1945631411668447,
    0.04641111772632078,
    0.4265956482036847,
    0.7325508076704729,
    0.5831923603637876,
    0.517070549656101,
    0.06669634258279566,
    0.36745398495374526,
    0.6247703542850642,
    0.015890911003170194,
    0.590042162124345,
    0.5728955147110176,
    0.8659561965629117,
    0.6247777492686022,
    0.9749310108542223,
    0.7198827510376747,
    0.2675492168317347,
    0.8497504581197128,
    0.18737783978778888,
    0.5295197146571862,
    0.6164229895296324,
    0.6123620885848995,
    0.003075684606846818,
    0.7864545045757781,
    0.5502455173915772,
    0.3165906166623188,
    0.9593700964159325,
    0.5382132345500036,
    0.46991863509205767,
    0.6872760606460611,
    0.4782931270940115,
    0.1683904793844172,
    0.27970842281170183,
    0.4052812870640743,
    0.257367220921607,
    0.07639286910402776,
    0.5913278008203109,
    0.17490468514887592,
    0.611803076093202,
    0.8224087965764244,
    0.755771043976373,
    0.7506748051896914,
    0.5698800549202858,
    0.5035040108541636,
    0.8454897955030667,
    0.6235088621118556,
    0.17271128165465122,
    0.8152191857936782,
    0.49953815817031433,
    0.5203181508531447,
    0.6102600047572958,
    0.7227724095627596,
    0.3656047257132512,
    0.6064953201674833,
    0.5373878644080369,
    0.9169457880851415,
    0.7253872642814476,
    0.2973197652796874,
    0.15687936629234434,
    0.26196223108428185,
    0.7735559909165758,
    0.8821693194334586,
    0.43080140071484097
  ]