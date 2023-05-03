module CFGTest where 

import CFG

import Syntax
import FactorialProgram

import Prelude hiding (init)
import Test.HUnit 

tc01 = TestCase (assertEqual "for init factorial, " 1 (init factorial))
tc02 = TestCase (assertEqual "for init s01, " 1 (init s01))
tc03 = TestCase (assertEqual "for init s02, " 2 (init s02))
tc04 = TestCase (assertEqual "for init s03, " 3 (init s03))
tc05 = TestCase (assertEqual "for init s04, " 6 (init s04))

testSuite = TestList [ TestLabel "tc01" tc01
                     , TestLabel "tc02" tc02
--		     , TestLabel "tc03" tc03      Undefined!
		     , TestLabel "tc04" tc04
		     ]
