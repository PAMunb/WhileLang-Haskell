module CFGPowerTest where 

import CFG

import Syntax
import PowerProgram

import Prelude hiding (init)
import Test.HUnit 
import Data.Set 

tcflow :: Test
tcflow = TestCase (assertEqual "for flow power, " (fromList [(1, 2), (2, 3), (3, 4), (4, 2)]) (flow power))

tcflowR :: Test
tcflowR = TestCase (assertEqual "for flowR power, " (fromList [(2, 1), (2, 4), (3, 2), (4, 3)]) (flowR power))

testPowerSuite :: Test
testPowerSuite = TestList [ TestLabel "tcflow" tcflow, 
                       TestLabel "tcflowR" tcflowR 
                    ]
