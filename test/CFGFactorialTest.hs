module CFGFactorialTest where 

import CFG

import Syntax
import FactorialProgram

import Prelude hiding (init)
import Test.HUnit 
import Data.Set 

tci01 :: Test
tci01 = TestCase (assertEqual "for init factorial, " 1 (init factorial))

tci02 :: Test
tci02 = TestCase (assertEqual "for init s01, " 1 (init s01))

tci03 :: Test
tci03 = TestCase (assertEqual "for init s02, " 2 (init s02))

tci04 :: Test
tci04 = TestCase (assertEqual "for init s03, " 3 (init s03))

tci05 :: Test
tci05 = TestCase (assertEqual "for init s04, " 6 (init s04))

tcf01 :: Test
tcf01 = TestCase (assertEqual "for final factorial, " (fromList [6]) (final factorial))

tcf02 :: Test
tcf02 = TestCase (assertEqual "for final s01, " (fromList [1]) (final s01))

tcf03 :: Test
tcf03 = TestCase (assertEqual "for final s02, " (singleton 2) (final s02))

tcf04 :: Test
tcf04 = TestCase (assertEqual "for final s03, " (singleton 3) (final s03))

tcf05 :: Test
tcf05 = TestCase (assertEqual "for final s04, " (singleton 6) (final s04))

tcb01 :: Test
tcb01 = TestCase (assertEqual "for blocks factorial, " (fromList [BlocksStmt s01, BlocksStmt s02, BlocksTest whileTeste, BlocksStmt whileS1, BlocksStmt whileS2, BlocksStmt s04]) (blocks factorial))

tcb02 :: Test
tcb02 = TestCase (assertEqual "for blocks s01, " (singleton (BlocksStmt s01))  (blocks s01))

tcb03 :: Test
tcb03 = TestCase (assertEqual "for blocks s02, " (singleton (BlocksStmt s02)) (blocks s02))

tcb04 :: Test
tcb04 = TestCase (assertEqual "for blocks s03, " (fromList [BlocksTest whileTeste, BlocksStmt whileS1, BlocksStmt whileS2]) (blocks s03))

tcb05 :: Test
tcb05 = TestCase (assertEqual "for blocks s04, " (singleton (BlocksStmt s04)) (blocks s04))


tcl01 :: Test
tcl01 = TestCase (assertEqual "for labels factorial, " (fromList [1, 2, 3, 4, 5, 6]) (labels factorial))

tcl02 :: Test
tcl02 = TestCase (assertEqual "for labels s01, " (singleton 1)  (labels s01))

tcl03 :: Test
tcl03 = TestCase (assertEqual "for labels s02, " (singleton 2) (labels s02))

tcl04 :: Test
tcl04 = TestCase (assertEqual "for labels s03, " (fromList [3, 4, 5]) (labels s03))

tcl05 :: Test
tcl05 = TestCase (assertEqual "for labels s04, " (singleton 6) (labels s04))


tcflow01 :: Test
tcflow01 = TestCase (assertEqual "for flow factorial, " (fromList [(1, 2), (2, 3), (4, 5), (3, 4), (5, 3), (3, 6)]) (flow factorial))

tcflow02 :: Test
tcflow02 = TestCase (assertEqual "for flow s01, " empty  (flow s01))

tcflow03 :: Test
tcflow03 = TestCase (assertEqual "for flow s02, " empty (flow s02))

tcflow04 :: Test
tcflow04 = TestCase (assertEqual "for flow s03, " (fromList [(4, 5), (3, 4), (5, 3)]) (flow s03))

tcflow05 :: Test
tcflow05 = TestCase (assertEqual "for flow s04, " empty (flow s04))


testFactorialSuite :: Test
testFactorialSuite = TestList [ TestLabel "tci01" tci01, 
                       TestLabel "tci02" tci02, 
                       TestLabel "tci03" tci03, 
                       TestLabel "tci04" tci04, 
                       TestLabel "tci05" tci05, 
                       TestLabel "tcf01" tcf01, 
                       TestLabel "tcf02" tcf02,
                       TestLabel "tcf03" tcf03, 
                       TestLabel "tcf04" tcf04, 
                       TestLabel "tcf05" tcf05,
                       TestLabel "tcb01" tcb01, 
                       TestLabel "tcb02" tcb02,
                       TestLabel "tcb03" tcb03, 
                       TestLabel "tcb04" tcb04, 
                       TestLabel "tcb05" tcb05,
                       TestLabel "tcl01" tcl01, 
                       TestLabel "tcl02" tcl02,
                       TestLabel "tcl03" tcl03, 
                       TestLabel "tcl04" tcl04, 
                       TestLabel "tcl05" tcl05,
                       TestLabel "tcflow01" tcflow01, 
                       TestLabel "tcflow02" tcflow02,
                       TestLabel "tcflow03" tcflow03, 
                       TestLabel "tcflow04" tcflow04, 
                       TestLabel "tcflow05" tcflow05
                    ]
