module DFATest where 

import DataFlowAnalysis.AvailableExpressions
import DataFlowAnalysis.LiveVariables
import DataFlowAnalysis.ReachingDefinitions
import DataFlowAnalysis.VeryBusyExpressions

import Framework.DFA
import Framework.Instances.AvailableExpressions
import Framework.Instances.LiveVariables
import Framework.Instances.ReachingDefinitions
import Framework.Instances.VeryBusyExpressions

import Programs.FactorialProgram
import Programs.AvailableExpressionsExample
import Programs.LiveVariablesExample
import Programs.VeryBusyExpressionsExample

import Test.HUnit 

tcAE :: Test
tcAE = TestCase (assertEqual "for AvailableExpressions, " (availableExpressions aeExample) (dfa mfAE aeExample))

tcLV :: Test
tcLV = TestCase (assertEqual "for LiveVariables, " (liveVariables lvtest) (dfa mfLV lvtest))

tcRD :: Test
tcRD = TestCase (assertEqual "for ReachingDefinitions, " (reachingDefinitions factorial) (dfa mfRD factorial))

tcVB :: Test
tcVB = TestCase (assertEqual "for VeryBusyExpressions, " (veryBusyExpressions vbExample) (dfa mfVB vbExample))

testDFASuite :: Test
testDFASuite = TestList [ TestLabel "tcAE" tcAE
                        , TestLabel "tcLV" tcLV
                        , TestLabel "tcRD" tcRD
                        , TestLabel "tcVB" tcVB
                    ]
