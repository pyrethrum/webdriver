module HTTP.LocateCombinatorTest where

import Control.Monad (replicateM)
import Data.Text qualified as T
import Data.Text (Text)
import HTTP.Runner (WDSession, closeWDSession, getWDSession)
import Prelude
import Test.Falsify.Generator as G (Gen, frequency, integral)
import Test.Falsify.Interactive (sample)
import Test.Falsify.Range as R (between)
import Test.Tasty (TestTree, inOrderTestGroup, testGroup, withResource)
import Utils (txt)
import qualified Data.Text.IO as T

-- >>> _eval tests
-- *** Exception: ExitSuccess
tests :: TestTree
tests =
  withResource getWDSession closeWDSession runSessionTests
  where
  runSessionTests :: IO WDSession -> TestTree
  runSessionTests _ses =
    inOrderTestGroup "Locate Combinator Tests"
      [ testGroup "TODO: Add tests here" []
      ]


data DOMClass = A | B | C | D | E deriving (Eq, Show)
data Node = Div {
                  autoId :: Text,
                  classes :: [DOMClass],
                  children :: [Node] 
                } | 
                Span {
                  autoId :: Text,
                  classes :: [DOMClass]
                }
                deriving (Eq, Show)

genNode :: Gen Node
genNode = 
  genDivNodeAt nodeRootDepth rootAutoId
  where
    nodeMaxDepth = 6
    nodeRootDepth = 1
    rootAutoId = "1"
    autoIdSeparator = "-"
    minChildrenPerLevel = 1
    maxChildrenPerLevel = 10
    spanNodeWeight = 3
    divNodeWeight = 1
    maxDomClassesPerNode = 5

    domClassCountWeights :: [(Word, Int)]
    domClassCountWeights =
      (\classCount -> (fromIntegral classCount, classCount)) <$> [1 .. maxDomClassesPerNode]

    genNodeAt :: Int -> Text -> Gen Node
    genNodeAt depth parentAutoId
      | depth >= nodeMaxDepth = genSpanNode parentAutoId
      | otherwise =
          frequency
            [ (spanNodeWeight, genSpanNode parentAutoId),
              (divNodeWeight, genDivNodeAt depth parentAutoId)
            ]

    genDivNodeAt :: Int -> Text -> Gen Node
    genDivNodeAt depth parentAutoId = do
      nodeClasses <- genDomClasses
      nodeChildren <- genChildrenAt depth parentAutoId
      pure $ Div {autoId = parentAutoId, classes = nodeClasses, children = nodeChildren}

    genSpanNode :: Text -> Gen Node
    genSpanNode parentAutoId = do
      nodeClasses <- genDomClasses
      pure $ Span {autoId = parentAutoId, classes = nodeClasses}

    genChildrenAt :: Int -> Text -> Gen [Node]
    genChildrenAt depth parentAutoId = do
      childCount <- G.integral $ R.between (minChildrenPerLevel, maxChildrenPerLevel)
      traverse (\childIndex -> genNodeAt (depth + 1) (mkChildAutoId parentAutoId childIndex)) [1 .. childCount]

    mkChildAutoId :: Text -> Int -> Text
    mkChildAutoId parentAutoId childIndex = parentAutoId <> autoIdSeparator <> txt childIndex

    genDomClasses :: Gen [DOMClass]
    genDomClasses = do
      classCount <- genDomClassCount
      genUniqueDomClasses classCount [A, B, C, D, E]

    genUniqueDomClasses :: Int -> [DOMClass] -> Gen [DOMClass]
    genUniqueDomClasses classCount availableClasses
      | classCount <= 0 = pure []
      | null availableClasses = pure []
      | otherwise = do
          nextClass <- genDomClassFrom availableClasses
          restClasses <- genUniqueDomClasses (classCount - 1) (filter (/= nextClass) availableClasses)
          pure $ nextClass : restClasses

    genDomClassCount :: Gen Int
    genDomClassCount = frequency ((\(w, c) -> (w, pure c)) <$> domClassCountWeights)

    genDomClassFrom :: [DOMClass] -> Gen DOMClass
    genDomClassFrom availableClasses =
      frequency $ ((\dc -> (1, pure dc)) <$> availableClasses)

prettyNode :: Int -> Node -> Text
prettyNode idx node = "Node " <> txt idx <> ":\n" <> txt node

_eval :: IO ()
_eval = do
  nodes <- replicateM 5 $ sample genNode
  T.putStrLn $  T.intercalate "\n\n" $ zipWith prettyNode [1 :: Int ..] nodes

-- >>> _eval
