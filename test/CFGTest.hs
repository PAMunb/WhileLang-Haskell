module CFGTest where 

import CFG

import Syntax
import FactorialProgram

import Prelude hiding (init)
import Test.HUnit 
import Data.Set 

tci01 = TestCase (assertEqual "for init factorial, " 1 (init factorial))
tci02 = TestCase (assertEqual "for init s01, " 1 (init s01))
tci03 = TestCase (assertEqual "for init s02, " 2 (init s02))
tci04 = TestCase (assertEqual "for init s03, " 3 (init s03))
tci05 = TestCase (assertEqual "for init s04, " 6 (init s04))

tcf01 = TestCase (assertEqual "for final factorial, " (fromList [1, 2, 3, 6]) (final factorial))
tcf02 = TestCase (assertEqual "for final s01, " (fromList [1]) (final s01))
tcf03 = TestCase (assertEqual "for final s02, " (singleton 2) (final s02))
tcf04 = TestCase (assertEqual "for final s03, " (singleton 3) (final s03))
tcf05 = TestCase (assertEqual "for final s04, " (singleton 6) (final s04))

testSuite = TestList [ TestLabel "tci01" tci01, 
						TestLabel "tci02" tci02, 
						TestLabel "tci03" tci03, 
						TestLabel "tci04" tci04, 
						TestLabel "tci05" tci05, 
						TestLabel "tcf01" tcf01, 
						TestLabel "tcf02" tcf02,
						TestLabel "tcf03" tcf03, 
						TestLabel "tcf04" tcf04, 
						TestLabel "tcf05" tcf05
					]
