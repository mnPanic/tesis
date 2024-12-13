{-# OPTIONS_GHC -w #-}
module PPA.Parser(parseProgram, parseProgram', parseTerm) where

import ND.ND ( Form(..), Term(..) )
import PPA.PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification, Case )
import PPA.Lexer
import Data.List (intercalate)
import Debug.Trace (trace)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn6 (Program)
	| HappyAbsSyn7 ([Decl])
	| HappyAbsSyn8 (Decl)
	| HappyAbsSyn11 (TProof)
	| HappyAbsSyn12 (ProofStep)
	| HappyAbsSyn13 ([Case])
	| HappyAbsSyn14 (Case)
	| HappyAbsSyn15 (Justification)
	| HappyAbsSyn17 (String)
	| HappyAbsSyn18 (Form)
	| HappyAbsSyn19 (Term)
	| HappyAbsSyn21 ([Term])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,262) ([0,0,192,0,0,0,12,0,0,3072,0,0,0,3072,0,0,0,0,0,0,0,12,0,0,0,0,0,0,0,0,0,0,1028,0,0,16384,64,0,0,0,0,4,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,0,0,16384,0,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1024,223,0,0,61504,13,0,0,0,0,64,0,8192,0,4,2048,0,0,0,0,0,0,0,49152,0,0,0,3072,0,0,61440,4096,0,0,0,4,1024,0,57092,0,0,16384,3568,0,0,0,0,0,0,0,0,0,0,32768,0,0,0,2048,0,0,1024,0,0,0,3840,0,0,0,57092,0,0,16384,3568,0,0,1024,223,0,0,61504,13,0,0,0,0,0,0,512,0,0,0,32,0,0,0,0,0,0,248,0,0,0,3072,0,0,0,32768,11119,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,46840,2,0,16448,0,0,61504,13,0,0,16384,64,0,16384,3568,0,0,0,16448,0,0,61504,13,0,0,16384,64,0,0,0,256,0,0,128,0,0,0,8,0,0,32768,0,0,0,0,0,4,0,0,0,0,61504,13,0,0,57092,0,0,0,15,0,0,61440,0,0,0,0,0,0,0,0,0,0,0,15,0,0,61440,0,0,0,0,0,0,0,0,0,16,0,0,16384,0,0,0,128,0,0,1028,0,0,0,1,0,0,15,0,0,0,256,0,0,3840,0,1,0,0,1,0,0,15,256,0,0,256,0,0,0,0,0,0,0,0,0,16384,3568,0,0,0,0,0,0,61504,13,0,0,0,0,0,16384,3568,0,0,1024,223,0,0,0,0,0,0,0,2,0,0,0,2,0,0,0,128,0,61504,1037,0,0,49152,0,0,0,1024,4,0,0,256,0,0,0,0,1024,0,0,1,0,0,15,46840,2,1024,0,0,0,0,0,0,0,0,0,0,0,1024,4,0,61440,4096,0,0,3840,0,1,0,240,4096,0,0,15,0,0,0,0,0,0,0,0,0,0,0,28544,43,0,0,0,0,0,0,0,0,61504,13,0,0,57092,0,0,0,15,256,0,61440,32768,11119,0,0,512,0,0,0,0,0,0,0,0,0,0,16448,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","%start_hParseProgram","%start_hParseTerm","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Cases","Case","OptionalBy","Justification","Name","Form","Term","TermNoInfix","TermArgs","Terms","'('","')'","and","or","imp","iff","not","true","false","forall","exists","dot","id","var","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","equivalently","claim","case","cases","take","':='","consider","st","let","'`'","%eof"]
        bit_start = st Prelude.* 60
        bit_end = (st Prelude.+ 1) Prelude.* 60
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..59]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (39) = happyShift action_8
action_0 (40) = happyShift action_9
action_0 (6) = happyGoto action_14
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (39) = happyShift action_8
action_1 (40) = happyShift action_9
action_1 (6) = happyGoto action_13
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (35) = happyShift action_11
action_2 (36) = happyShift action_12
action_2 (19) = happyGoto action_10
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (39) = happyShift action_8
action_3 (40) = happyShift action_9
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 (9) = happyGoto action_6
action_3 (10) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (39) = happyShift action_8
action_5 (40) = happyShift action_9
action_5 (7) = happyGoto action_22
action_5 (8) = happyGoto action_5
action_5 (9) = happyGoto action_6
action_5 (10) = happyGoto action_7
action_5 _ = happyReduce_5

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 (35) = happyShift action_19
action_8 (43) = happyShift action_20
action_8 (17) = happyGoto action_21
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (35) = happyShift action_19
action_9 (43) = happyShift action_20
action_9 (17) = happyGoto action_18
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (59) = happyShift action_17
action_10 (60) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (23) = happyShift action_16
action_11 (21) = happyGoto action_15
action_11 _ = happyReduce_50

action_12 _ = happyReduce_45

action_13 (60) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (60) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_46

action_16 (35) = happyShift action_11
action_16 (36) = happyShift action_12
action_16 (19) = happyGoto action_26
action_16 (22) = happyGoto action_27
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (35) = happyShift action_25
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (37) = happyShift action_24
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_31

action_20 _ = happyReduce_32

action_21 (37) = happyShift action_23
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_4

action_23 (23) = happyShift action_33
action_23 (29) = happyShift action_34
action_23 (30) = happyShift action_35
action_23 (31) = happyShift action_36
action_23 (32) = happyShift action_37
action_23 (33) = happyShift action_38
action_23 (35) = happyShift action_39
action_23 (36) = happyShift action_12
action_23 (18) = happyGoto action_40
action_23 (19) = happyGoto action_32
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (23) = happyShift action_33
action_24 (29) = happyShift action_34
action_24 (30) = happyShift action_35
action_24 (31) = happyShift action_36
action_24 (32) = happyShift action_37
action_24 (33) = happyShift action_38
action_24 (35) = happyShift action_39
action_24 (36) = happyShift action_12
action_24 (18) = happyGoto action_31
action_24 (19) = happyGoto action_32
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (59) = happyShift action_30
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (38) = happyShift action_29
action_26 (59) = happyShift action_17
action_26 _ = happyReduce_52

action_27 (24) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_51

action_29 (35) = happyShift action_11
action_29 (36) = happyShift action_12
action_29 (19) = happyGoto action_26
action_29 (22) = happyGoto action_55
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (35) = happyShift action_53
action_30 (36) = happyShift action_54
action_30 (20) = happyGoto action_52
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (25) = happyShift action_41
action_31 (26) = happyShift action_42
action_31 (27) = happyShift action_43
action_31 (28) = happyShift action_44
action_31 (41) = happyShift action_51
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (35) = happyShift action_50
action_32 (59) = happyShift action_17
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (23) = happyShift action_33
action_33 (29) = happyShift action_34
action_33 (30) = happyShift action_35
action_33 (31) = happyShift action_36
action_33 (32) = happyShift action_37
action_33 (33) = happyShift action_38
action_33 (35) = happyShift action_39
action_33 (36) = happyShift action_12
action_33 (18) = happyGoto action_49
action_33 (19) = happyGoto action_32
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (23) = happyShift action_33
action_34 (29) = happyShift action_34
action_34 (30) = happyShift action_35
action_34 (31) = happyShift action_36
action_34 (32) = happyShift action_37
action_34 (33) = happyShift action_38
action_34 (35) = happyShift action_39
action_34 (36) = happyShift action_12
action_34 (18) = happyGoto action_48
action_34 (19) = happyGoto action_32
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_42

action_36 _ = happyReduce_43

action_37 (36) = happyShift action_47
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (36) = happyShift action_46
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (23) = happyShift action_16
action_39 (21) = happyGoto action_45
action_39 _ = happyReduce_50

action_40 (25) = happyShift action_41
action_40 (26) = happyShift action_42
action_40 (27) = happyShift action_43
action_40 (28) = happyShift action_44
action_40 _ = happyReduce_8

action_41 (23) = happyShift action_33
action_41 (29) = happyShift action_34
action_41 (30) = happyShift action_35
action_41 (31) = happyShift action_36
action_41 (32) = happyShift action_37
action_41 (33) = happyShift action_38
action_41 (35) = happyShift action_39
action_41 (36) = happyShift action_12
action_41 (18) = happyGoto action_77
action_41 (19) = happyGoto action_32
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (23) = happyShift action_33
action_42 (29) = happyShift action_34
action_42 (30) = happyShift action_35
action_42 (31) = happyShift action_36
action_42 (32) = happyShift action_37
action_42 (33) = happyShift action_38
action_42 (35) = happyShift action_39
action_42 (36) = happyShift action_12
action_42 (18) = happyGoto action_76
action_42 (19) = happyGoto action_32
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (23) = happyShift action_33
action_43 (29) = happyShift action_34
action_43 (30) = happyShift action_35
action_43 (31) = happyShift action_36
action_43 (32) = happyShift action_37
action_43 (33) = happyShift action_38
action_43 (35) = happyShift action_39
action_43 (36) = happyShift action_12
action_43 (18) = happyGoto action_75
action_43 (19) = happyGoto action_32
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (23) = happyShift action_33
action_44 (29) = happyShift action_34
action_44 (30) = happyShift action_35
action_44 (31) = happyShift action_36
action_44 (32) = happyShift action_37
action_44 (33) = happyShift action_38
action_44 (35) = happyShift action_39
action_44 (36) = happyShift action_12
action_44 (18) = happyGoto action_74
action_44 (19) = happyGoto action_32
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (35) = happyReduce_46
action_45 (59) = happyReduce_46
action_45 _ = happyReduce_33

action_46 (34) = happyShift action_73
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (34) = happyShift action_72
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_39

action_49 (24) = happyShift action_71
action_49 (25) = happyShift action_41
action_49 (26) = happyShift action_42
action_49 (27) = happyShift action_43
action_49 (28) = happyShift action_44
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (35) = happyShift action_11
action_50 (36) = happyShift action_12
action_50 (19) = happyGoto action_70
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (44) = happyShift action_59
action_51 (45) = happyShift action_60
action_51 (46) = happyShift action_61
action_51 (47) = happyShift action_62
action_51 (48) = happyShift action_63
action_51 (50) = happyShift action_64
action_51 (51) = happyShift action_65
action_51 (53) = happyShift action_66
action_51 (54) = happyShift action_67
action_51 (56) = happyShift action_68
action_51 (58) = happyShift action_69
action_51 (11) = happyGoto action_57
action_51 (12) = happyGoto action_58
action_51 _ = happyReduce_11

action_52 _ = happyReduce_47

action_53 (23) = happyShift action_16
action_53 (21) = happyGoto action_56
action_53 _ = happyReduce_50

action_54 _ = happyReduce_48

action_55 _ = happyReduce_53

action_56 _ = happyReduce_49

action_57 (42) = happyShift action_93
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (44) = happyShift action_59
action_58 (45) = happyShift action_60
action_58 (46) = happyShift action_61
action_58 (47) = happyShift action_62
action_58 (48) = happyShift action_63
action_58 (50) = happyShift action_64
action_58 (51) = happyShift action_65
action_58 (53) = happyShift action_66
action_58 (54) = happyShift action_67
action_58 (56) = happyShift action_68
action_58 (58) = happyShift action_69
action_58 (11) = happyGoto action_92
action_58 (12) = happyGoto action_58
action_58 _ = happyReduce_11

action_59 (35) = happyShift action_19
action_59 (43) = happyShift action_20
action_59 (17) = happyGoto action_91
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (23) = happyShift action_33
action_60 (29) = happyShift action_34
action_60 (30) = happyShift action_35
action_60 (31) = happyShift action_36
action_60 (32) = happyShift action_37
action_60 (33) = happyShift action_38
action_60 (35) = happyShift action_39
action_60 (36) = happyShift action_12
action_60 (18) = happyGoto action_90
action_60 (19) = happyGoto action_32
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (35) = happyShift action_19
action_61 (43) = happyShift action_20
action_61 (17) = happyGoto action_89
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (23) = happyShift action_33
action_62 (29) = happyShift action_34
action_62 (30) = happyShift action_35
action_62 (31) = happyShift action_36
action_62 (32) = happyShift action_37
action_62 (33) = happyShift action_38
action_62 (35) = happyShift action_39
action_62 (36) = happyShift action_12
action_62 (18) = happyGoto action_88
action_62 (19) = happyGoto action_32
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (35) = happyShift action_19
action_63 (43) = happyShift action_20
action_63 (17) = happyGoto action_87
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (23) = happyShift action_33
action_64 (29) = happyShift action_34
action_64 (30) = happyShift action_35
action_64 (31) = happyShift action_36
action_64 (32) = happyShift action_37
action_64 (33) = happyShift action_38
action_64 (35) = happyShift action_39
action_64 (36) = happyShift action_12
action_64 (18) = happyGoto action_86
action_64 (19) = happyGoto action_32
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (35) = happyShift action_19
action_65 (43) = happyShift action_20
action_65 (17) = happyGoto action_85
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (49) = happyShift action_84
action_66 (15) = happyGoto action_83
action_66 _ = happyReduce_28

action_67 (36) = happyShift action_82
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (36) = happyShift action_81
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (36) = happyShift action_80
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (59) = happyShift action_17
action_70 _ = happyReduce_34

action_71 _ = happyReduce_44

action_72 (23) = happyShift action_33
action_72 (29) = happyShift action_34
action_72 (30) = happyShift action_35
action_72 (31) = happyShift action_36
action_72 (32) = happyShift action_37
action_72 (33) = happyShift action_38
action_72 (35) = happyShift action_39
action_72 (36) = happyShift action_12
action_72 (18) = happyGoto action_79
action_72 (19) = happyGoto action_32
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (23) = happyShift action_33
action_73 (29) = happyShift action_34
action_73 (30) = happyShift action_35
action_73 (31) = happyShift action_36
action_73 (32) = happyShift action_37
action_73 (33) = happyShift action_38
action_73 (35) = happyShift action_39
action_73 (36) = happyShift action_12
action_73 (18) = happyGoto action_78
action_73 (19) = happyGoto action_32
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (25) = happyShift action_41
action_74 (26) = happyShift action_42
action_74 (27) = happyShift action_43
action_74 (28) = happyShift action_44
action_74 _ = happyReduce_38

action_75 (25) = happyShift action_41
action_75 (26) = happyShift action_42
action_75 (27) = happyShift action_43
action_75 (28) = happyShift action_44
action_75 _ = happyReduce_37

action_76 _ = happyReduce_36

action_77 _ = happyReduce_35

action_78 (25) = happyShift action_41
action_78 (26) = happyShift action_42
action_78 (27) = happyShift action_43
action_78 (28) = happyShift action_44
action_78 _ = happyReduce_40

action_79 (25) = happyShift action_41
action_79 (26) = happyShift action_42
action_79 (27) = happyShift action_43
action_79 (28) = happyShift action_44
action_79 _ = happyReduce_41

action_80 _ = happyReduce_21

action_81 (57) = happyShift action_106
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (55) = happyShift action_105
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (52) = happyShift action_104
action_83 (13) = happyGoto action_102
action_83 (14) = happyGoto action_103
action_83 _ = happyReduce_24

action_84 (35) = happyShift action_19
action_84 (43) = happyShift action_20
action_84 (16) = happyGoto action_100
action_84 (17) = happyGoto action_101
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (37) = happyShift action_99
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (25) = happyShift action_41
action_86 (26) = happyShift action_42
action_86 (27) = happyShift action_43
action_86 (28) = happyShift action_44
action_86 _ = happyReduce_17

action_87 (37) = happyShift action_98
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (25) = happyShift action_41
action_88 (26) = happyShift action_42
action_88 (27) = happyShift action_43
action_88 (28) = happyShift action_44
action_88 (49) = happyShift action_84
action_88 (15) = happyGoto action_97
action_88 _ = happyReduce_28

action_89 (37) = happyShift action_96
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (25) = happyShift action_41
action_90 (26) = happyShift action_42
action_90 (27) = happyShift action_43
action_90 (28) = happyShift action_44
action_90 (49) = happyShift action_84
action_90 (15) = happyGoto action_95
action_90 _ = happyReduce_28

action_91 (37) = happyShift action_94
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_10

action_93 _ = happyReduce_9

action_94 (23) = happyShift action_33
action_94 (29) = happyShift action_34
action_94 (30) = happyShift action_35
action_94 (31) = happyShift action_36
action_94 (32) = happyShift action_37
action_94 (33) = happyShift action_38
action_94 (35) = happyShift action_39
action_94 (36) = happyShift action_12
action_94 (18) = happyGoto action_118
action_94 (19) = happyGoto action_32
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_13

action_96 (23) = happyShift action_33
action_96 (29) = happyShift action_34
action_96 (30) = happyShift action_35
action_96 (31) = happyShift action_36
action_96 (32) = happyShift action_37
action_96 (33) = happyShift action_38
action_96 (35) = happyShift action_39
action_96 (36) = happyShift action_12
action_96 (18) = happyGoto action_117
action_96 (19) = happyGoto action_32
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_14

action_98 (23) = happyShift action_33
action_98 (29) = happyShift action_34
action_98 (30) = happyShift action_35
action_98 (31) = happyShift action_36
action_98 (32) = happyShift action_37
action_98 (33) = happyShift action_38
action_98 (35) = happyShift action_39
action_98 (36) = happyShift action_12
action_98 (18) = happyGoto action_116
action_98 (19) = happyGoto action_32
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (23) = happyShift action_33
action_99 (29) = happyShift action_34
action_99 (30) = happyShift action_35
action_99 (31) = happyShift action_36
action_99 (32) = happyShift action_37
action_99 (33) = happyShift action_38
action_99 (35) = happyShift action_39
action_99 (36) = happyShift action_12
action_99 (18) = happyGoto action_115
action_99 (19) = happyGoto action_32
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_27

action_101 (38) = happyShift action_114
action_101 _ = happyReduce_30

action_102 (42) = happyShift action_113
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (52) = happyShift action_104
action_103 (13) = happyGoto action_112
action_103 (14) = happyGoto action_103
action_103 _ = happyReduce_24

action_104 (23) = happyShift action_33
action_104 (29) = happyShift action_34
action_104 (30) = happyShift action_35
action_104 (31) = happyShift action_36
action_104 (32) = happyShift action_37
action_104 (33) = happyShift action_38
action_104 (35) = happyShift action_111
action_104 (36) = happyShift action_12
action_104 (43) = happyShift action_20
action_104 (17) = happyGoto action_109
action_104 (18) = happyGoto action_110
action_104 (19) = happyGoto action_32
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (35) = happyShift action_11
action_105 (36) = happyShift action_12
action_105 (19) = happyGoto action_108
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (35) = happyShift action_19
action_106 (43) = happyShift action_20
action_106 (17) = happyGoto action_107
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (37) = happyShift action_125
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (59) = happyShift action_17
action_108 _ = happyReduce_20

action_109 (37) = happyShift action_124
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (25) = happyShift action_41
action_110 (26) = happyShift action_42
action_110 (27) = happyShift action_43
action_110 (28) = happyShift action_44
action_110 (44) = happyShift action_59
action_110 (45) = happyShift action_60
action_110 (46) = happyShift action_61
action_110 (47) = happyShift action_62
action_110 (48) = happyShift action_63
action_110 (50) = happyShift action_64
action_110 (51) = happyShift action_65
action_110 (53) = happyShift action_66
action_110 (54) = happyShift action_67
action_110 (56) = happyShift action_68
action_110 (58) = happyShift action_69
action_110 (11) = happyGoto action_123
action_110 (12) = happyGoto action_58
action_110 _ = happyReduce_11

action_111 (23) = happyShift action_16
action_111 (37) = happyReduce_31
action_111 (21) = happyGoto action_45
action_111 _ = happyReduce_50

action_112 _ = happyReduce_23

action_113 _ = happyReduce_19

action_114 (35) = happyShift action_19
action_114 (43) = happyShift action_20
action_114 (16) = happyGoto action_122
action_114 (17) = happyGoto action_101
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (25) = happyShift action_41
action_115 (26) = happyShift action_42
action_115 (27) = happyShift action_43
action_115 (28) = happyShift action_44
action_115 (41) = happyShift action_121
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (25) = happyShift action_41
action_116 (26) = happyShift action_42
action_116 (27) = happyShift action_43
action_116 (28) = happyShift action_44
action_116 (49) = happyShift action_84
action_116 (15) = happyGoto action_120
action_116 _ = happyReduce_28

action_117 (25) = happyShift action_41
action_117 (26) = happyShift action_42
action_117 (27) = happyShift action_43
action_117 (28) = happyShift action_44
action_117 (49) = happyShift action_84
action_117 (15) = happyGoto action_119
action_117 _ = happyReduce_28

action_118 (25) = happyShift action_41
action_118 (26) = happyShift action_42
action_118 (27) = happyShift action_43
action_118 (28) = happyShift action_44
action_118 _ = happyReduce_12

action_119 _ = happyReduce_16

action_120 _ = happyReduce_15

action_121 (44) = happyShift action_59
action_121 (45) = happyShift action_60
action_121 (46) = happyShift action_61
action_121 (47) = happyShift action_62
action_121 (48) = happyShift action_63
action_121 (50) = happyShift action_64
action_121 (51) = happyShift action_65
action_121 (53) = happyShift action_66
action_121 (54) = happyShift action_67
action_121 (56) = happyShift action_68
action_121 (58) = happyShift action_69
action_121 (11) = happyGoto action_128
action_121 (12) = happyGoto action_58
action_121 _ = happyReduce_11

action_122 _ = happyReduce_29

action_123 _ = happyReduce_25

action_124 (23) = happyShift action_33
action_124 (29) = happyShift action_34
action_124 (30) = happyShift action_35
action_124 (31) = happyShift action_36
action_124 (32) = happyShift action_37
action_124 (33) = happyShift action_38
action_124 (35) = happyShift action_39
action_124 (36) = happyShift action_12
action_124 (18) = happyGoto action_127
action_124 (19) = happyGoto action_32
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (23) = happyShift action_33
action_125 (29) = happyShift action_34
action_125 (30) = happyShift action_35
action_125 (31) = happyShift action_36
action_125 (32) = happyShift action_37
action_125 (33) = happyShift action_38
action_125 (35) = happyShift action_39
action_125 (36) = happyShift action_12
action_125 (18) = happyGoto action_126
action_125 (19) = happyGoto action_32
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (25) = happyShift action_41
action_126 (26) = happyShift action_42
action_126 (27) = happyShift action_43
action_126 (28) = happyShift action_44
action_126 (49) = happyShift action_131
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (25) = happyShift action_41
action_127 (26) = happyShift action_42
action_127 (27) = happyShift action_43
action_127 (28) = happyShift action_44
action_127 (44) = happyShift action_59
action_127 (45) = happyShift action_60
action_127 (46) = happyShift action_61
action_127 (47) = happyShift action_62
action_127 (48) = happyShift action_63
action_127 (50) = happyShift action_64
action_127 (51) = happyShift action_65
action_127 (53) = happyShift action_66
action_127 (54) = happyShift action_67
action_127 (56) = happyShift action_68
action_127 (58) = happyShift action_69
action_127 (11) = happyGoto action_130
action_127 (12) = happyGoto action_58
action_127 _ = happyReduce_11

action_128 (42) = happyShift action_129
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_18

action_130 _ = happyReduce_26

action_131 (35) = happyShift action_19
action_131 (43) = happyShift action_20
action_131 (16) = happyGoto action_132
action_131 (17) = happyGoto action_101
action_131 _ = happyFail (happyExpListPerState 131)

action_132 _ = happyReduce_22

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  8 happyReduction_6
happyReduction_6 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 9 happyReduction_8
happyReduction_8 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (DAxiom happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 7 10 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (DTheorem happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_2  11 happyReduction_10
happyReduction_10 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  11 happyReduction_11
happyReduction_11  =  HappyAbsSyn11
		 ([]
	)

happyReduce_12 = happyReduce 4 12 happyReduction_12
happyReduction_12 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSSuppose happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  12 happyReduction_13
happyReduction_13 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (PSThusBy happy_var_2 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn15  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (PSThusBy happy_var_2 (["-"] ++ happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 5 12 happyReduction_15
happyReduction_15 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSHaveBy happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 5 12 happyReduction_16
happyReduction_16 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSHaveBy happy_var_2 happy_var_4 (["-"] ++ happy_var_5)
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  12 happyReduction_17
happyReduction_17 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (PSEquiv happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 7 12 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSClaim happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 12 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSCases happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 12 happyReduction_20
happyReduction_20 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSTake happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  12 happyReduction_21
happyReduction_21 (HappyTerminal (Token _ (TokenVar happy_var_2)))
	_
	 =  HappyAbsSyn12
		 (PSLet happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 8 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (PSConsider happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  13 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  13 happyReduction_24
happyReduction_24  =  HappyAbsSyn13
		 ([]
	)

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyAbsSyn11  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (("-", happy_var_2, happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 14 happyReduction_26
happyReduction_26 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((happy_var_2, happy_var_4, happy_var_5)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  15 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  15 happyReduction_28
happyReduction_28  =  HappyAbsSyn15
		 ([]
	)

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn15
		 ([ happy_var_1 ]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  18 happyReduction_33
happyReduction_33 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn18
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  18 happyReduction_34
happyReduction_34 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal (Token _ (TokenId happy_var_2)))
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (FPred happy_var_2 [happy_var_1, happy_var_3]
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  18 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FAnd (FImp happy_var_1 happy_var_3) (FImp happy_var_3 happy_var_1)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  18 happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (FNot happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 18 happyReduction_40
happyReduction_40 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 4 18 happyReduction_41
happyReduction_41 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn18
		 (FTrue
	)

happyReduce_43 = happySpecReduce_1  18 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn18
		 (FFalse
	)

happyReduce_44 = happySpecReduce_3  18 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  19 happyReduction_45
happyReduction_45 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn19
		 (TVar happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  19 happyReduction_46
happyReduction_46 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn19
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 5 19 happyReduction_47
happyReduction_47 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenId happy_var_3))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (TFun happy_var_3 [happy_var_1, happy_var_5]
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_1  20 happyReduction_48
happyReduction_48 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn19
		 (TVar happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  20 happyReduction_49
happyReduction_49 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn19
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  21 happyReduction_50
happyReduction_50  =  HappyAbsSyn21
		 ([]
	)

happyReduce_51 = happySpecReduce_3  21 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  22 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  22 happyReduction_53
happyReduction_53 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1 : happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 60 60 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 23;
	Token _ TokenParenClose -> cont 24;
	Token _ TokenAnd -> cont 25;
	Token _ TokenOr -> cont 26;
	Token _ TokenImp -> cont 27;
	Token _ TokenIff -> cont 28;
	Token _ TokenNot -> cont 29;
	Token _ TokenTrue -> cont 30;
	Token _ TokenFalse -> cont 31;
	Token _ TokenForall -> cont 32;
	Token _ TokenExists -> cont 33;
	Token _ TokenDot -> cont 34;
	Token _ (TokenId happy_dollar_dollar) -> cont 35;
	Token _ (TokenVar happy_dollar_dollar) -> cont 36;
	Token _ TokenDoubleColon -> cont 37;
	Token _ TokenComma -> cont 38;
	Token _ TokenAxiom -> cont 39;
	Token _ TokenTheorem -> cont 40;
	Token _ TokenProof -> cont 41;
	Token _ TokenEnd -> cont 42;
	Token _ (TokenQuotedName happy_dollar_dollar) -> cont 43;
	Token _ TokenSuppose -> cont 44;
	Token _ TokenThus -> cont 45;
	Token _ TokenThen -> cont 46;
	Token _ TokenHence -> cont 47;
	Token _ TokenHave -> cont 48;
	Token _ TokenBy -> cont 49;
	Token _ TokenEquivalently -> cont 50;
	Token _ TokenClaim -> cont 51;
	Token _ TokenCase -> cont 52;
	Token _ TokenCases -> cont 53;
	Token _ TokenTake -> cont 54;
	Token _ TokenAssign -> cont 55;
	Token _ TokenConsider -> cont 56;
	Token _ TokenSuchThat -> cont 57;
	Token _ TokenLet -> cont 58;
	Token _ TokenBacktick -> cont 59;
	_ -> happyError' (tk, [])
	})

happyError_ explist 60 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Alex a
happyReturn = (Prelude.return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [Prelude.String]) -> Alex a
happyError' tk = parseError tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

hParseProgram = happySomeParser where
 happySomeParser = happyThen (happyParse action_1) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

hParseTerm = happySomeParser where
 happySomeParser = happyThen (happyParse action_2) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: (Token, [String]) -> Alex a
parseError ((Token p t), next) =
        alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:")
        --alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:" ++ trace ("a" ++ (show $ null next)) (show next))

parseProgram' :: FilePath -> String -> Either String Program
parseProgram' = runAlex' hParseProgram

parseProgram :: String -> Either String Program
parseProgram s = runAlex s hParseProgram

parseTerm :: String -> Either String Term
parseTerm s = runAlex s hParseTerm
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
