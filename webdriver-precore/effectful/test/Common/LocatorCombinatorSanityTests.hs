module Common.LocatorCombinatorSanityTests
  ( -- * Sanity check test group
    tests,

    -- * Abstract locator model (shared with the combinator tests)
    Node (Div, Span, autoId, classes, children),
    DOMClass (..),
    AbsLoc (Or', And', Under, Match, locs, parentLoc, descendantLoc, domClass),
    match,
  )
where

import Data.Function ((&))
import Data.List (intersect, nub, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, unpack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Prelude

{-
  Locator combinator tests rely on the abstract locator model as a source of truth. An abstract DOM is genrated, a
  real DOM is genrated from the abstract one, a real locate is executed against the real DOM and the result compared 
  to the result of running an abstract version of the same locator against the abstract DOM.

  This module sanity checks the abstract locator logic against a hard coded expectation on matches against a simple
  DOM model. IE this module is to give us confidence the "oracle" logic works.
-}

tests :: TestTree
tests =
  testGroup
    "sanity checks for abstract locator"
    [ sanitySimpleA,
      sanityAOrB,
      sanityAandC,
      sanityAUnderB,
      sanityAandCUnderC,
      sanityAandCUnderB,
      sanityNested,
      sanityNested2
    ]

data DOMClass = A | B | C | D | E deriving (Eq, Show)

data Node
  = Div
      { autoId :: Text,
        classes :: [DOMClass],
        children :: [Node]
      }
  | Span
      { autoId :: Text,
        classes :: [DOMClass]
      }
  deriving (Eq, Show)

data AbsLoc
  = Or' {locs :: NonEmpty AbsLoc}
  | And' {locs :: NonEmpty AbsLoc}
  | Under
      { parentLoc :: AbsLoc,
        descendantLoc :: AbsLoc
      }
  | Match
      { domClass :: DOMClass
      }
  deriving (Eq, Show)

match :: Node -> AbsLoc -> [Text]
match root sel =
  let matchingIds = selectIds root sel
   in flatten root
        & fmap (.autoId)
        & filter (`elem` matchingIds)

-- Evaluate a selection against a subtree and return matching node ids.
selectNodes :: Node -> AbsLoc -> [Node]
selectNodes root sel =
  case sel of
    Match {domClass} ->
      flatten root
        & filter (elem domClass . (.classes))
    Or' {locs} -> nub . concat $ selectNodes root <$> locs
    And' {locs} -> foldSelections root locs
    Under {parentLoc, descendantLoc} ->
      nub . concat $ selectUnder descendantLoc <$> (selectNodes root parentLoc)

-- Evaluate a selection against a subtree and return matching node ids.
selectIds :: Node -> AbsLoc -> [Text]
selectIds root sel = (.autoId) <$> selectNodes root sel

-- Evaluate all child selections under a specific parent node.
selectUnder :: AbsLoc -> Node -> [Node]
selectUnder descendantLoc parentNode =
  descendants parentNode >>= flip selectNodes descendantLoc

foldSelections :: Node -> NonEmpty AbsLoc -> [Node]
foldSelections root (firstSel :| rest) =
  foldl' intersect (selectNodes root firstSel) $ selectNodes root <$> rest

flatten :: Node -> [Node]
flatten = \case
  divNode@Div {children} -> divNode : concatMap flatten children
  spanNode@Span {} -> [spanNode]

descendants :: Node -> [Node]
descendants = \case
  Div {children} -> concatMap flatten children
  Span {} -> []

chkListContentEq :: (Ord a, Show a) => Text -> [a] -> [a] -> IO ()
chkListContentEq message expected actual =
  assertEqual (unpack message) (sort expected) (sort actual)

sanityChk' :: Node -> Text -> [Text] -> AbsLoc -> TestTree
sanityChk' node message expected selection =
  testCase (unpack message) $ do
    let actual = match node selection
    chkListContentEq message expected actual

sanityChk :: Text -> [Text] -> AbsLoc -> TestTree
sanityChk message expected selection =
  sanityChk' sanityNode message expected selection

sanityNode :: Node
sanityNode =
  Div
    { autoId = "1",
      classes = [A, B],
      children =
        [ Span {autoId = "1-1", classes = [A]},
          Div
            { autoId = "1-2",
              classes = [B],
              children =
                [ Span {autoId = "1-2-1", classes = [A, C]},
                  Span {autoId = "1-2-2", classes = [C]}
                ]
            },
          Div
            { autoId = "1-3",
              classes = [C],
              children =
                [ Span {autoId = "1-3-1", classes = [A, C]},
                  Span {autoId = "1-3-2", classes = [C]}
                ]
            }
        ]
    }

-- Tests run against sanityNode (above)
sanitySimpleA :: TestTree
sanitySimpleA = sanityChk "Simple Match A" ["1", "1-1", "1-2-1", "1-3-1"] $ Match A

sanityAOrB :: TestTree
sanityAOrB = sanityChk "A or B" ["1", "1-1", "1-2-1", "1-3-1", "1-2"] $ Or' (Match A :| [Match B])

sanityAandC :: TestTree
sanityAandC = sanityChk "A and C" ["1-2-1", "1-3-1"] $ And' (Match A :| [Match C])

sanityAandCUnderC :: TestTree
sanityAandCUnderC = sanityChk "A and C under C" ["1-3-1"] $ Under {parentLoc = Match C, descendantLoc = And' (Match A :| [Match C])}

sanityAandCUnderB :: TestTree
sanityAandCUnderB = sanityChk "A and C under B" ["1-2-1", "1-3-1"] $ Under {parentLoc = Match B, descendantLoc = And' (Match A :| [Match C])}

sanityAUnderB :: TestTree
sanityAUnderB = sanityChk "A under B" ["1-1", "1-2-1", "1-3-1"] $ Under {parentLoc = Match B, descendantLoc = Match A}

sanityNode2 :: Node
sanityNode2 =
  Div
    { autoId = "1-1-3",
      classes = [A],
      children =
        [ Div
            { autoId = "1-1-3-6",
              classes = [A, B],
              children = [Span {autoId = "1-1-3-6-1", classes = [B]}]
            }
        ]
    }

sanityNested :: TestTree
sanityNested =
  sanityChk' sanityNode2 "Double nested" [] $
    Under
      { parentLoc = Match B,
        descendantLoc =
          Under
            { parentLoc = Match A,
              descendantLoc = Match B
            }
      }

sanityNode3 :: Node
sanityNode3 =
  Div
    { autoId = "1",
      classes = [C],
      children =
        [ Div
            { autoId = "1-1-3-6",
              classes = [A],
              children =
                [ Span {autoId = "1-1-3-6-1", classes = [B]},
                  Div
                    { autoId = "1-1-3-6-2",
                      classes = [B],
                      children = [Span {autoId = "1-1-3-6-2-1", classes = [C]}]
                    }
                ]
            }
        ]
    }

sanityNested2 :: TestTree
sanityNested2 =
  sanityChk' sanityNode3 "Double nested 2" ["1-1-3-6-2-1"] $
    Under
      { parentLoc = Match C,
        descendantLoc =
          Under
            { parentLoc = Match B,
              descendantLoc = Match C
            }
      }
