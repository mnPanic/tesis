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
	| HappyAbsSyn20 ([Term])

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
 action_122 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_49 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,222) ([0,0,96,0,0,32768,1,0,0,96,0,0,0,24,0,0,0,0,0,0,384,0,0,0,0,0,0,0,0,0,0,514,0,0,2048,8,0,0,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,0,0,8192,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,0,0,0,12162,0,0,2048,190,0,0,0,16,0,0,1,0,0,0,0,0,0,32768,1,0,32768,32775,0,0,57472,11,0,0,12162,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,64,0,0,8,0,0,32768,7,0,0,57472,11,0,0,12162,0,0,2048,190,0,0,63520,2,0,0,0,0,0,0,16,0,0,16384,0,0,0,0,0,0,7936,0,0,0,0,47040,21,0,0,0,0,0,256,0,0,0,28144,5,0,8224,0,0,48648,0,0,0,512,2,0,57472,11,0,0,8192,32,0,2048,190,0,0,0,514,0,0,0,512,0,0,64,0,0,0,1,0,0,1024,0,0,0,0,0,0,12162,0,0,2048,190,0,0,1920,0,0,0,30,0,0,0,0,0,0,0,0,0,32768,7,0,0,7680,0,0,0,0,0,0,0,0,8192,0,0,0,32,0,0,4096,0,0,8224,0,0,0,2,0,32768,7,0,0,0,32,0,0,120,2048,0,0,512,0,0,1920,32768,0,0,8192,0,0,0,0,0,0,0,0,0,8192,760,0,0,0,0,0,0,12162,0,0,0,0,0,0,63520,2,0,32768,3040,0,0,0,0,0,0,0,4,0,0,0,1,0,0,0,16,0,12162,32,0,0,384,0,0,0,514,0,0,8192,0,0,0,0,0,0,0,2,0,32768,7,23420,1,128,0,0,0,0,0,0,0,0,0,0,0,514,0,0,30,2,0,30720,0,8,0,480,8192,0,32768,7,0,0,0,0,0,0,0,0,0,0,0,22239,0,0,0,0,0,0,0,0,33280,47,0,0,48648,0,0,32768,7,128,0,7680,61440,1389,0,0,16,0,0,0,0,0,0,0,0,0,2048,8,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","%start_hParseProgram","%start_hParseTerm","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Cases","Case","OptionalBy","Justification","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","iff","not","true","false","forall","exists","dot","id","var","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","equivalently","claim","case","cases","take","':='","consider","st","let","%eof"]
        bit_start = st Prelude.* 58
        bit_end = (st Prelude.+ 1) Prelude.* 58
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..57]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (38) = happyShift action_8
action_0 (39) = happyShift action_9
action_0 (6) = happyGoto action_14
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (38) = happyShift action_8
action_1 (39) = happyShift action_9
action_1 (6) = happyGoto action_13
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (34) = happyShift action_11
action_2 (35) = happyShift action_12
action_2 (19) = happyGoto action_10
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (38) = happyShift action_8
action_3 (39) = happyShift action_9
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 (9) = happyGoto action_6
action_3 (10) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (38) = happyShift action_8
action_5 (39) = happyShift action_9
action_5 (7) = happyGoto action_21
action_5 (8) = happyGoto action_5
action_5 (9) = happyGoto action_6
action_5 (10) = happyGoto action_7
action_5 _ = happyReduce_5

action_6 _ = happyReduce_6

action_7 _ = happyReduce_7

action_8 (34) = happyShift action_18
action_8 (42) = happyShift action_19
action_8 (17) = happyGoto action_20
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (34) = happyShift action_18
action_9 (42) = happyShift action_19
action_9 (17) = happyGoto action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (58) = happyAccept
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (22) = happyShift action_16
action_11 (20) = happyGoto action_15
action_11 _ = happyReduce_46

action_12 _ = happyReduce_44

action_13 (58) = happyAccept
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (58) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_45

action_16 (34) = happyShift action_11
action_16 (35) = happyShift action_12
action_16 (19) = happyGoto action_24
action_16 (21) = happyGoto action_25
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (36) = happyShift action_23
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_31

action_19 _ = happyReduce_32

action_20 (36) = happyShift action_22
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_4

action_22 (22) = happyShift action_29
action_22 (28) = happyShift action_30
action_22 (29) = happyShift action_31
action_22 (30) = happyShift action_32
action_22 (31) = happyShift action_33
action_22 (32) = happyShift action_34
action_22 (34) = happyShift action_35
action_22 (18) = happyGoto action_36
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (22) = happyShift action_29
action_23 (28) = happyShift action_30
action_23 (29) = happyShift action_31
action_23 (30) = happyShift action_32
action_23 (31) = happyShift action_33
action_23 (32) = happyShift action_34
action_23 (34) = happyShift action_35
action_23 (18) = happyGoto action_28
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (37) = happyShift action_27
action_24 _ = happyReduce_48

action_25 (23) = happyShift action_26
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_47

action_27 (34) = happyShift action_11
action_27 (35) = happyShift action_12
action_27 (19) = happyGoto action_24
action_27 (21) = happyGoto action_47
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (24) = happyShift action_37
action_28 (25) = happyShift action_38
action_28 (26) = happyShift action_39
action_28 (27) = happyShift action_40
action_28 (40) = happyShift action_46
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (22) = happyShift action_29
action_29 (28) = happyShift action_30
action_29 (29) = happyShift action_31
action_29 (30) = happyShift action_32
action_29 (31) = happyShift action_33
action_29 (32) = happyShift action_34
action_29 (34) = happyShift action_35
action_29 (18) = happyGoto action_45
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (22) = happyShift action_29
action_30 (28) = happyShift action_30
action_30 (29) = happyShift action_31
action_30 (30) = happyShift action_32
action_30 (31) = happyShift action_33
action_30 (32) = happyShift action_34
action_30 (34) = happyShift action_35
action_30 (18) = happyGoto action_44
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_41

action_32 _ = happyReduce_42

action_33 (35) = happyShift action_43
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (35) = happyShift action_42
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (22) = happyShift action_16
action_35 (20) = happyGoto action_41
action_35 _ = happyReduce_46

action_36 (24) = happyShift action_37
action_36 (25) = happyShift action_38
action_36 (26) = happyShift action_39
action_36 (27) = happyShift action_40
action_36 _ = happyReduce_8

action_37 (22) = happyShift action_29
action_37 (28) = happyShift action_30
action_37 (29) = happyShift action_31
action_37 (30) = happyShift action_32
action_37 (31) = happyShift action_33
action_37 (32) = happyShift action_34
action_37 (34) = happyShift action_35
action_37 (18) = happyGoto action_67
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (22) = happyShift action_29
action_38 (28) = happyShift action_30
action_38 (29) = happyShift action_31
action_38 (30) = happyShift action_32
action_38 (31) = happyShift action_33
action_38 (32) = happyShift action_34
action_38 (34) = happyShift action_35
action_38 (18) = happyGoto action_66
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (22) = happyShift action_29
action_39 (28) = happyShift action_30
action_39 (29) = happyShift action_31
action_39 (30) = happyShift action_32
action_39 (31) = happyShift action_33
action_39 (32) = happyShift action_34
action_39 (34) = happyShift action_35
action_39 (18) = happyGoto action_65
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (22) = happyShift action_29
action_40 (28) = happyShift action_30
action_40 (29) = happyShift action_31
action_40 (30) = happyShift action_32
action_40 (31) = happyShift action_33
action_40 (32) = happyShift action_34
action_40 (34) = happyShift action_35
action_40 (18) = happyGoto action_64
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_33

action_42 (33) = happyShift action_63
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (33) = happyShift action_62
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_38

action_45 (23) = happyShift action_61
action_45 (24) = happyShift action_37
action_45 (25) = happyShift action_38
action_45 (26) = happyShift action_39
action_45 (27) = happyShift action_40
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (43) = happyShift action_50
action_46 (44) = happyShift action_51
action_46 (45) = happyShift action_52
action_46 (46) = happyShift action_53
action_46 (47) = happyShift action_54
action_46 (49) = happyShift action_55
action_46 (50) = happyShift action_56
action_46 (52) = happyShift action_57
action_46 (53) = happyShift action_58
action_46 (55) = happyShift action_59
action_46 (57) = happyShift action_60
action_46 (11) = happyGoto action_48
action_46 (12) = happyGoto action_49
action_46 _ = happyReduce_11

action_47 _ = happyReduce_49

action_48 (41) = happyShift action_83
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (43) = happyShift action_50
action_49 (44) = happyShift action_51
action_49 (45) = happyShift action_52
action_49 (46) = happyShift action_53
action_49 (47) = happyShift action_54
action_49 (49) = happyShift action_55
action_49 (50) = happyShift action_56
action_49 (52) = happyShift action_57
action_49 (53) = happyShift action_58
action_49 (55) = happyShift action_59
action_49 (57) = happyShift action_60
action_49 (11) = happyGoto action_82
action_49 (12) = happyGoto action_49
action_49 _ = happyReduce_11

action_50 (34) = happyShift action_18
action_50 (42) = happyShift action_19
action_50 (17) = happyGoto action_81
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (22) = happyShift action_29
action_51 (28) = happyShift action_30
action_51 (29) = happyShift action_31
action_51 (30) = happyShift action_32
action_51 (31) = happyShift action_33
action_51 (32) = happyShift action_34
action_51 (34) = happyShift action_35
action_51 (18) = happyGoto action_80
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (34) = happyShift action_18
action_52 (42) = happyShift action_19
action_52 (17) = happyGoto action_79
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (22) = happyShift action_29
action_53 (28) = happyShift action_30
action_53 (29) = happyShift action_31
action_53 (30) = happyShift action_32
action_53 (31) = happyShift action_33
action_53 (32) = happyShift action_34
action_53 (34) = happyShift action_35
action_53 (18) = happyGoto action_78
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (34) = happyShift action_18
action_54 (42) = happyShift action_19
action_54 (17) = happyGoto action_77
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (22) = happyShift action_29
action_55 (28) = happyShift action_30
action_55 (29) = happyShift action_31
action_55 (30) = happyShift action_32
action_55 (31) = happyShift action_33
action_55 (32) = happyShift action_34
action_55 (34) = happyShift action_35
action_55 (18) = happyGoto action_76
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (34) = happyShift action_18
action_56 (42) = happyShift action_19
action_56 (17) = happyGoto action_75
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (48) = happyShift action_74
action_57 (15) = happyGoto action_73
action_57 _ = happyReduce_28

action_58 (35) = happyShift action_72
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (35) = happyShift action_71
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (35) = happyShift action_70
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_43

action_62 (22) = happyShift action_29
action_62 (28) = happyShift action_30
action_62 (29) = happyShift action_31
action_62 (30) = happyShift action_32
action_62 (31) = happyShift action_33
action_62 (32) = happyShift action_34
action_62 (34) = happyShift action_35
action_62 (18) = happyGoto action_69
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (22) = happyShift action_29
action_63 (28) = happyShift action_30
action_63 (29) = happyShift action_31
action_63 (30) = happyShift action_32
action_63 (31) = happyShift action_33
action_63 (32) = happyShift action_34
action_63 (34) = happyShift action_35
action_63 (18) = happyGoto action_68
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (24) = happyShift action_37
action_64 (25) = happyShift action_38
action_64 (26) = happyShift action_39
action_64 (27) = happyShift action_40
action_64 _ = happyReduce_37

action_65 (24) = happyShift action_37
action_65 (25) = happyShift action_38
action_65 (26) = happyShift action_39
action_65 (27) = happyShift action_40
action_65 _ = happyReduce_36

action_66 _ = happyReduce_35

action_67 _ = happyReduce_34

action_68 (24) = happyShift action_37
action_68 (25) = happyShift action_38
action_68 (26) = happyShift action_39
action_68 (27) = happyShift action_40
action_68 _ = happyReduce_39

action_69 (24) = happyShift action_37
action_69 (25) = happyShift action_38
action_69 (26) = happyShift action_39
action_69 (27) = happyShift action_40
action_69 _ = happyReduce_40

action_70 _ = happyReduce_21

action_71 (56) = happyShift action_96
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (54) = happyShift action_95
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (51) = happyShift action_94
action_73 (13) = happyGoto action_92
action_73 (14) = happyGoto action_93
action_73 _ = happyReduce_24

action_74 (34) = happyShift action_18
action_74 (42) = happyShift action_19
action_74 (16) = happyGoto action_90
action_74 (17) = happyGoto action_91
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (36) = happyShift action_89
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (24) = happyShift action_37
action_76 (25) = happyShift action_38
action_76 (26) = happyShift action_39
action_76 (27) = happyShift action_40
action_76 _ = happyReduce_17

action_77 (36) = happyShift action_88
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (24) = happyShift action_37
action_78 (25) = happyShift action_38
action_78 (26) = happyShift action_39
action_78 (27) = happyShift action_40
action_78 (48) = happyShift action_74
action_78 (15) = happyGoto action_87
action_78 _ = happyReduce_28

action_79 (36) = happyShift action_86
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (24) = happyShift action_37
action_80 (25) = happyShift action_38
action_80 (26) = happyShift action_39
action_80 (27) = happyShift action_40
action_80 (48) = happyShift action_74
action_80 (15) = happyGoto action_85
action_80 _ = happyReduce_28

action_81 (36) = happyShift action_84
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_10

action_83 _ = happyReduce_9

action_84 (22) = happyShift action_29
action_84 (28) = happyShift action_30
action_84 (29) = happyShift action_31
action_84 (30) = happyShift action_32
action_84 (31) = happyShift action_33
action_84 (32) = happyShift action_34
action_84 (34) = happyShift action_35
action_84 (18) = happyGoto action_108
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_13

action_86 (22) = happyShift action_29
action_86 (28) = happyShift action_30
action_86 (29) = happyShift action_31
action_86 (30) = happyShift action_32
action_86 (31) = happyShift action_33
action_86 (32) = happyShift action_34
action_86 (34) = happyShift action_35
action_86 (18) = happyGoto action_107
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_14

action_88 (22) = happyShift action_29
action_88 (28) = happyShift action_30
action_88 (29) = happyShift action_31
action_88 (30) = happyShift action_32
action_88 (31) = happyShift action_33
action_88 (32) = happyShift action_34
action_88 (34) = happyShift action_35
action_88 (18) = happyGoto action_106
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (22) = happyShift action_29
action_89 (28) = happyShift action_30
action_89 (29) = happyShift action_31
action_89 (30) = happyShift action_32
action_89 (31) = happyShift action_33
action_89 (32) = happyShift action_34
action_89 (34) = happyShift action_35
action_89 (18) = happyGoto action_105
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_27

action_91 (37) = happyShift action_104
action_91 _ = happyReduce_30

action_92 (41) = happyShift action_103
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (51) = happyShift action_94
action_93 (13) = happyGoto action_102
action_93 (14) = happyGoto action_93
action_93 _ = happyReduce_24

action_94 (22) = happyShift action_29
action_94 (28) = happyShift action_30
action_94 (29) = happyShift action_31
action_94 (30) = happyShift action_32
action_94 (31) = happyShift action_33
action_94 (32) = happyShift action_34
action_94 (34) = happyShift action_101
action_94 (42) = happyShift action_19
action_94 (17) = happyGoto action_99
action_94 (18) = happyGoto action_100
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (34) = happyShift action_11
action_95 (35) = happyShift action_12
action_95 (19) = happyGoto action_98
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (34) = happyShift action_18
action_96 (42) = happyShift action_19
action_96 (17) = happyGoto action_97
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (36) = happyShift action_115
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_20

action_99 (36) = happyShift action_114
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (24) = happyShift action_37
action_100 (25) = happyShift action_38
action_100 (26) = happyShift action_39
action_100 (27) = happyShift action_40
action_100 (43) = happyShift action_50
action_100 (44) = happyShift action_51
action_100 (45) = happyShift action_52
action_100 (46) = happyShift action_53
action_100 (47) = happyShift action_54
action_100 (49) = happyShift action_55
action_100 (50) = happyShift action_56
action_100 (52) = happyShift action_57
action_100 (53) = happyShift action_58
action_100 (55) = happyShift action_59
action_100 (57) = happyShift action_60
action_100 (11) = happyGoto action_113
action_100 (12) = happyGoto action_49
action_100 _ = happyReduce_11

action_101 (22) = happyShift action_16
action_101 (36) = happyReduce_31
action_101 (20) = happyGoto action_41
action_101 _ = happyReduce_46

action_102 _ = happyReduce_23

action_103 _ = happyReduce_19

action_104 (34) = happyShift action_18
action_104 (42) = happyShift action_19
action_104 (16) = happyGoto action_112
action_104 (17) = happyGoto action_91
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (24) = happyShift action_37
action_105 (25) = happyShift action_38
action_105 (26) = happyShift action_39
action_105 (27) = happyShift action_40
action_105 (40) = happyShift action_111
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (24) = happyShift action_37
action_106 (25) = happyShift action_38
action_106 (26) = happyShift action_39
action_106 (27) = happyShift action_40
action_106 (48) = happyShift action_74
action_106 (15) = happyGoto action_110
action_106 _ = happyReduce_28

action_107 (24) = happyShift action_37
action_107 (25) = happyShift action_38
action_107 (26) = happyShift action_39
action_107 (27) = happyShift action_40
action_107 (48) = happyShift action_74
action_107 (15) = happyGoto action_109
action_107 _ = happyReduce_28

action_108 (24) = happyShift action_37
action_108 (25) = happyShift action_38
action_108 (26) = happyShift action_39
action_108 (27) = happyShift action_40
action_108 _ = happyReduce_12

action_109 _ = happyReduce_16

action_110 _ = happyReduce_15

action_111 (43) = happyShift action_50
action_111 (44) = happyShift action_51
action_111 (45) = happyShift action_52
action_111 (46) = happyShift action_53
action_111 (47) = happyShift action_54
action_111 (49) = happyShift action_55
action_111 (50) = happyShift action_56
action_111 (52) = happyShift action_57
action_111 (53) = happyShift action_58
action_111 (55) = happyShift action_59
action_111 (57) = happyShift action_60
action_111 (11) = happyGoto action_118
action_111 (12) = happyGoto action_49
action_111 _ = happyReduce_11

action_112 _ = happyReduce_29

action_113 _ = happyReduce_25

action_114 (22) = happyShift action_29
action_114 (28) = happyShift action_30
action_114 (29) = happyShift action_31
action_114 (30) = happyShift action_32
action_114 (31) = happyShift action_33
action_114 (32) = happyShift action_34
action_114 (34) = happyShift action_35
action_114 (18) = happyGoto action_117
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (22) = happyShift action_29
action_115 (28) = happyShift action_30
action_115 (29) = happyShift action_31
action_115 (30) = happyShift action_32
action_115 (31) = happyShift action_33
action_115 (32) = happyShift action_34
action_115 (34) = happyShift action_35
action_115 (18) = happyGoto action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (24) = happyShift action_37
action_116 (25) = happyShift action_38
action_116 (26) = happyShift action_39
action_116 (27) = happyShift action_40
action_116 (48) = happyShift action_121
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (24) = happyShift action_37
action_117 (25) = happyShift action_38
action_117 (26) = happyShift action_39
action_117 (27) = happyShift action_40
action_117 (43) = happyShift action_50
action_117 (44) = happyShift action_51
action_117 (45) = happyShift action_52
action_117 (46) = happyShift action_53
action_117 (47) = happyShift action_54
action_117 (49) = happyShift action_55
action_117 (50) = happyShift action_56
action_117 (52) = happyShift action_57
action_117 (53) = happyShift action_58
action_117 (55) = happyShift action_59
action_117 (57) = happyShift action_60
action_117 (11) = happyGoto action_120
action_117 (12) = happyGoto action_49
action_117 _ = happyReduce_11

action_118 (41) = happyShift action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_18

action_120 _ = happyReduce_26

action_121 (34) = happyShift action_18
action_121 (42) = happyShift action_19
action_121 (16) = happyGoto action_122
action_121 (17) = happyGoto action_91
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_22

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
happyReduction_33 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn18
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  18 happyReduction_34
happyReduction_34 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  18 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (FAnd (FImp happy_var_1 happy_var_3) (FImp happy_var_3 happy_var_1)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  18 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (FNot happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 18 happyReduction_39
happyReduction_39 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 18 happyReduction_40
happyReduction_40 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  18 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn18
		 (FTrue
	)

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn18
		 (FFalse
	)

happyReduce_43 = happySpecReduce_3  18 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  19 happyReduction_44
happyReduction_44 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn19
		 (TVar happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  19 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn19
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  20 happyReduction_46
happyReduction_46  =  HappyAbsSyn20
		 ([]
	)

happyReduce_47 = happySpecReduce_3  20 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  21 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn20
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  21 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1 : happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 58 58 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 22;
	Token _ TokenParenClose -> cont 23;
	Token _ TokenAnd -> cont 24;
	Token _ TokenOr -> cont 25;
	Token _ TokenImp -> cont 26;
	Token _ TokenIff -> cont 27;
	Token _ TokenNot -> cont 28;
	Token _ TokenTrue -> cont 29;
	Token _ TokenFalse -> cont 30;
	Token _ TokenForall -> cont 31;
	Token _ TokenExists -> cont 32;
	Token _ TokenDot -> cont 33;
	Token _ (TokenId happy_dollar_dollar) -> cont 34;
	Token _ (TokenVar happy_dollar_dollar) -> cont 35;
	Token _ TokenDoubleColon -> cont 36;
	Token _ TokenComma -> cont 37;
	Token _ TokenAxiom -> cont 38;
	Token _ TokenTheorem -> cont 39;
	Token _ TokenProof -> cont 40;
	Token _ TokenEnd -> cont 41;
	Token _ (TokenQuotedName happy_dollar_dollar) -> cont 42;
	Token _ TokenSuppose -> cont 43;
	Token _ TokenThus -> cont 44;
	Token _ TokenThen -> cont 45;
	Token _ TokenHence -> cont 46;
	Token _ TokenHave -> cont 47;
	Token _ TokenBy -> cont 48;
	Token _ TokenEquivalently -> cont 49;
	Token _ TokenClaim -> cont 50;
	Token _ TokenCase -> cont 51;
	Token _ TokenCases -> cont 52;
	Token _ TokenTake -> cont 53;
	Token _ TokenAssign -> cont 54;
	Token _ TokenConsider -> cont 55;
	Token _ TokenSuchThat -> cont 56;
	Token _ TokenLet -> cont 57;
	_ -> happyError' (tk, [])
	})

happyError_ explist 58 tk = happyError' (tk, explist)
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
