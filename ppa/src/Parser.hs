{-# OPTIONS_GHC -w #-}
module Parser(parseProgram, parseProgram') where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification, Case )
import Lexer
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
	| HappyAbsSyn4 (Program)
	| HappyAbsSyn5 ([Decl])
	| HappyAbsSyn6 (Decl)
	| HappyAbsSyn9 (TProof)
	| HappyAbsSyn10 (ProofStep)
	| HappyAbsSyn11 ([Case])
	| HappyAbsSyn12 (Case)
	| HappyAbsSyn13 (Justification)
	| HappyAbsSyn15 (String)
	| HappyAbsSyn16 (Form)
	| HappyAbsSyn17 (Term)
	| HappyAbsSyn18 ([Term])

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
 action_115 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
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
 happyReduce_45 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,200) ([0,0,12,0,0,768,0,0,0,0,0,0,48,0,0,0,0,0,0,0,0,0,1028,0,0,256,1,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,61568,5,0,8192,380,0,0,224,16,0,49664,23,0,32768,1520,0,0,0,0,0,0,0,0,0,8192,0,0,0,8,0,8192,0,0,0,224,0,0,49664,23,0,32768,1520,0,0,31776,1,0,0,0,0,0,12288,0,0,0,2,0,0,128,0,0,0,0,0,15360,0,0,0,0,46840,0,0,128,0,0,32768,2927,0,4096,16,0,61568,5,0,0,256,1,0,24328,0,0,0,4112,0,32768,1520,0,0,0,257,0,0,0,16,0,8192,0,0,0,8,0,0,0,0,0,24328,0,0,49664,23,0,0,8192,0,0,64,0,0,2048,0,0,0,0,0,0,3584,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,0,57344,0,0,0,56,0,0,0,0,256,0,0,4096,0,16384,64,0,0,64,0,0,14,0,0,0,4,0,57344,0,16,0,16384,0,0,3584,0,1,0,1024,0,0,0,0,0,0,0,0,32768,1520,0,0,0,0,0,0,16448,0,0,6082,0,0,0,0,0,8192,380,0,0,24328,0,0,0,0,32,0,8192,0,0,0,3,0,0,16448,0,0,0,0,0,0,16,0,0,0,0,0,16384,64,0,0,2048,0,0,0,2048,0,31776,257,0,57344,4096,0,0,56,1024,0,3584,0,1,0,0,0,0,224,0,0,0,0,0,0,0,0,0,0,48640,45,0,256,0,0,56,56288,2,128,0,0,0,0,0,0,0,0,0,0,0,0,32768,1520,0,0,896,16384,0,0,0,0,0,6082,0,0,0,512,0,0,0,0,0,224,28544,11,0,4112,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Cases","Case","OptionalBy","Justification","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","equivalently","claim","case","cases","take","':='","consider","st","%eof"]
        bit_start = st Prelude.* 54
        bit_end = (st Prelude.+ 1) Prelude.* 54
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..53]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (35) = happyShift action_6
action_0 (36) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (35) = happyShift action_6
action_1 (36) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (35) = happyShift action_6
action_3 (36) = happyShift action_7
action_3 (5) = happyGoto action_13
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (31) = happyShift action_10
action_6 (39) = happyShift action_11
action_6 (15) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (31) = happyShift action_10
action_7 (39) = happyShift action_11
action_7 (15) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (54) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (33) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_28

action_11 _ = happyReduce_29

action_12 (33) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_2

action_14 (20) = happyShift action_17
action_14 (25) = happyShift action_18
action_14 (26) = happyShift action_19
action_14 (27) = happyShift action_20
action_14 (28) = happyShift action_21
action_14 (29) = happyShift action_22
action_14 (31) = happyShift action_23
action_14 (16) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (20) = happyShift action_17
action_15 (25) = happyShift action_18
action_15 (26) = happyShift action_19
action_15 (27) = happyShift action_20
action_15 (28) = happyShift action_21
action_15 (29) = happyShift action_22
action_15 (31) = happyShift action_23
action_15 (16) = happyGoto action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (22) = happyShift action_25
action_16 (23) = happyShift action_26
action_16 (24) = happyShift action_27
action_16 (37) = happyShift action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (20) = happyShift action_17
action_17 (25) = happyShift action_18
action_17 (26) = happyShift action_19
action_17 (27) = happyShift action_20
action_17 (28) = happyShift action_21
action_17 (29) = happyShift action_22
action_17 (31) = happyShift action_23
action_17 (16) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (20) = happyShift action_17
action_18 (25) = happyShift action_18
action_18 (26) = happyShift action_19
action_18 (27) = happyShift action_20
action_18 (28) = happyShift action_21
action_18 (29) = happyShift action_22
action_18 (31) = happyShift action_23
action_18 (16) = happyGoto action_32
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_37

action_20 _ = happyReduce_38

action_21 (32) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (32) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (20) = happyShift action_29
action_23 (18) = happyGoto action_28
action_23 _ = happyReduce_42

action_24 (22) = happyShift action_25
action_24 (23) = happyShift action_26
action_24 (24) = happyShift action_27
action_24 _ = happyReduce_6

action_25 (20) = happyShift action_17
action_25 (25) = happyShift action_18
action_25 (26) = happyShift action_19
action_25 (27) = happyShift action_20
action_25 (28) = happyShift action_21
action_25 (29) = happyShift action_22
action_25 (31) = happyShift action_23
action_25 (16) = happyGoto action_56
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (20) = happyShift action_17
action_26 (25) = happyShift action_18
action_26 (26) = happyShift action_19
action_26 (27) = happyShift action_20
action_26 (28) = happyShift action_21
action_26 (29) = happyShift action_22
action_26 (31) = happyShift action_23
action_26 (16) = happyGoto action_55
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (20) = happyShift action_17
action_27 (25) = happyShift action_18
action_27 (26) = happyShift action_19
action_27 (27) = happyShift action_20
action_27 (28) = happyShift action_21
action_27 (29) = happyShift action_22
action_27 (31) = happyShift action_23
action_27 (16) = happyGoto action_54
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_30

action_29 (31) = happyShift action_52
action_29 (32) = happyShift action_53
action_29 (17) = happyGoto action_50
action_29 (19) = happyGoto action_51
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (30) = happyShift action_49
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (30) = happyShift action_48
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_34

action_33 (21) = happyShift action_47
action_33 (22) = happyShift action_25
action_33 (23) = happyShift action_26
action_33 (24) = happyShift action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (40) = happyShift action_37
action_34 (41) = happyShift action_38
action_34 (42) = happyShift action_39
action_34 (43) = happyShift action_40
action_34 (44) = happyShift action_41
action_34 (46) = happyShift action_42
action_34 (47) = happyShift action_43
action_34 (49) = happyShift action_44
action_34 (50) = happyShift action_45
action_34 (52) = happyShift action_46
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_36
action_34 _ = happyReduce_9

action_35 (38) = happyShift action_73
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (40) = happyShift action_37
action_36 (41) = happyShift action_38
action_36 (42) = happyShift action_39
action_36 (43) = happyShift action_40
action_36 (44) = happyShift action_41
action_36 (46) = happyShift action_42
action_36 (47) = happyShift action_43
action_36 (49) = happyShift action_44
action_36 (50) = happyShift action_45
action_36 (52) = happyShift action_46
action_36 (9) = happyGoto action_72
action_36 (10) = happyGoto action_36
action_36 _ = happyReduce_9

action_37 (31) = happyShift action_10
action_37 (39) = happyShift action_11
action_37 (15) = happyGoto action_71
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (20) = happyShift action_17
action_38 (25) = happyShift action_18
action_38 (26) = happyShift action_19
action_38 (27) = happyShift action_20
action_38 (28) = happyShift action_21
action_38 (29) = happyShift action_22
action_38 (31) = happyShift action_23
action_38 (16) = happyGoto action_70
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (31) = happyShift action_10
action_39 (39) = happyShift action_11
action_39 (15) = happyGoto action_69
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (20) = happyShift action_17
action_40 (25) = happyShift action_18
action_40 (26) = happyShift action_19
action_40 (27) = happyShift action_20
action_40 (28) = happyShift action_21
action_40 (29) = happyShift action_22
action_40 (31) = happyShift action_23
action_40 (16) = happyGoto action_68
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (31) = happyShift action_10
action_41 (39) = happyShift action_11
action_41 (15) = happyGoto action_67
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (20) = happyShift action_17
action_42 (25) = happyShift action_18
action_42 (26) = happyShift action_19
action_42 (27) = happyShift action_20
action_42 (28) = happyShift action_21
action_42 (29) = happyShift action_22
action_42 (31) = happyShift action_23
action_42 (16) = happyGoto action_66
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (31) = happyShift action_10
action_43 (39) = happyShift action_11
action_43 (15) = happyGoto action_65
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (45) = happyShift action_64
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (32) = happyShift action_63
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (32) = happyShift action_62
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_39

action_48 (20) = happyShift action_17
action_48 (25) = happyShift action_18
action_48 (26) = happyShift action_19
action_48 (27) = happyShift action_20
action_48 (28) = happyShift action_21
action_48 (29) = happyShift action_22
action_48 (31) = happyShift action_23
action_48 (16) = happyGoto action_61
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (20) = happyShift action_17
action_49 (25) = happyShift action_18
action_49 (26) = happyShift action_19
action_49 (27) = happyShift action_20
action_49 (28) = happyShift action_21
action_49 (29) = happyShift action_22
action_49 (31) = happyShift action_23
action_49 (16) = happyGoto action_60
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (34) = happyShift action_59
action_50 _ = happyReduce_44

action_51 (21) = happyShift action_58
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (20) = happyShift action_29
action_52 (18) = happyGoto action_57
action_52 _ = happyReduce_42

action_53 _ = happyReduce_40

action_54 (22) = happyShift action_25
action_54 (23) = happyShift action_26
action_54 (24) = happyShift action_27
action_54 _ = happyReduce_33

action_55 _ = happyReduce_32

action_56 _ = happyReduce_31

action_57 _ = happyReduce_41

action_58 _ = happyReduce_43

action_59 (31) = happyShift action_52
action_59 (32) = happyShift action_53
action_59 (17) = happyGoto action_50
action_59 (19) = happyGoto action_85
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (22) = happyShift action_25
action_60 (23) = happyShift action_26
action_60 (24) = happyShift action_27
action_60 _ = happyReduce_35

action_61 (22) = happyShift action_25
action_61 (23) = happyShift action_26
action_61 (24) = happyShift action_27
action_61 _ = happyReduce_36

action_62 (53) = happyShift action_84
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (51) = happyShift action_83
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (31) = happyShift action_10
action_64 (39) = happyShift action_11
action_64 (14) = happyGoto action_81
action_64 (15) = happyGoto action_82
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (33) = happyShift action_80
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (22) = happyShift action_25
action_66 (23) = happyShift action_26
action_66 (24) = happyShift action_27
action_66 _ = happyReduce_15

action_67 (33) = happyShift action_79
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (22) = happyShift action_25
action_68 (23) = happyShift action_26
action_68 (24) = happyShift action_27
action_68 (45) = happyShift action_76
action_68 (13) = happyGoto action_78
action_68 _ = happyReduce_25

action_69 (33) = happyShift action_77
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (22) = happyShift action_25
action_70 (23) = happyShift action_26
action_70 (24) = happyShift action_27
action_70 (45) = happyShift action_76
action_70 (13) = happyGoto action_75
action_70 _ = happyReduce_25

action_71 (33) = happyShift action_74
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_8

action_73 _ = happyReduce_7

action_74 (20) = happyShift action_17
action_74 (25) = happyShift action_18
action_74 (26) = happyShift action_19
action_74 (27) = happyShift action_20
action_74 (28) = happyShift action_21
action_74 (29) = happyShift action_22
action_74 (31) = happyShift action_23
action_74 (16) = happyGoto action_96
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_11

action_76 (31) = happyShift action_10
action_76 (39) = happyShift action_11
action_76 (14) = happyGoto action_95
action_76 (15) = happyGoto action_82
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (20) = happyShift action_17
action_77 (25) = happyShift action_18
action_77 (26) = happyShift action_19
action_77 (27) = happyShift action_20
action_77 (28) = happyShift action_21
action_77 (29) = happyShift action_22
action_77 (31) = happyShift action_23
action_77 (16) = happyGoto action_94
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_12

action_79 (20) = happyShift action_17
action_79 (25) = happyShift action_18
action_79 (26) = happyShift action_19
action_79 (27) = happyShift action_20
action_79 (28) = happyShift action_21
action_79 (29) = happyShift action_22
action_79 (31) = happyShift action_23
action_79 (16) = happyGoto action_93
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (20) = happyShift action_17
action_80 (25) = happyShift action_18
action_80 (26) = happyShift action_19
action_80 (27) = happyShift action_20
action_80 (28) = happyShift action_21
action_80 (29) = happyShift action_22
action_80 (31) = happyShift action_23
action_80 (16) = happyGoto action_92
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (48) = happyShift action_91
action_81 (11) = happyGoto action_89
action_81 (12) = happyGoto action_90
action_81 _ = happyReduce_21

action_82 (34) = happyShift action_88
action_82 _ = happyReduce_27

action_83 (31) = happyShift action_52
action_83 (32) = happyShift action_53
action_83 (17) = happyGoto action_87
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (31) = happyShift action_10
action_84 (39) = happyShift action_11
action_84 (15) = happyGoto action_86
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_45

action_86 (33) = happyShift action_106
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_18

action_88 (31) = happyShift action_10
action_88 (39) = happyShift action_11
action_88 (14) = happyGoto action_105
action_88 (15) = happyGoto action_82
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (38) = happyShift action_104
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (48) = happyShift action_91
action_90 (11) = happyGoto action_103
action_90 (12) = happyGoto action_90
action_90 _ = happyReduce_21

action_91 (20) = happyShift action_17
action_91 (25) = happyShift action_18
action_91 (26) = happyShift action_19
action_91 (27) = happyShift action_20
action_91 (28) = happyShift action_21
action_91 (29) = happyShift action_22
action_91 (31) = happyShift action_102
action_91 (39) = happyShift action_11
action_91 (15) = happyGoto action_100
action_91 (16) = happyGoto action_101
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (22) = happyShift action_25
action_92 (23) = happyShift action_26
action_92 (24) = happyShift action_27
action_92 (37) = happyShift action_99
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (22) = happyShift action_25
action_93 (23) = happyShift action_26
action_93 (24) = happyShift action_27
action_93 (45) = happyShift action_76
action_93 (13) = happyGoto action_98
action_93 _ = happyReduce_25

action_94 (22) = happyShift action_25
action_94 (23) = happyShift action_26
action_94 (24) = happyShift action_27
action_94 (45) = happyShift action_76
action_94 (13) = happyGoto action_97
action_94 _ = happyReduce_25

action_95 _ = happyReduce_24

action_96 (22) = happyShift action_25
action_96 (23) = happyShift action_26
action_96 (24) = happyShift action_27
action_96 _ = happyReduce_10

action_97 _ = happyReduce_14

action_98 _ = happyReduce_13

action_99 (40) = happyShift action_37
action_99 (41) = happyShift action_38
action_99 (42) = happyShift action_39
action_99 (43) = happyShift action_40
action_99 (44) = happyShift action_41
action_99 (46) = happyShift action_42
action_99 (47) = happyShift action_43
action_99 (49) = happyShift action_44
action_99 (50) = happyShift action_45
action_99 (52) = happyShift action_46
action_99 (9) = happyGoto action_110
action_99 (10) = happyGoto action_36
action_99 _ = happyReduce_9

action_100 (33) = happyShift action_109
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (22) = happyShift action_25
action_101 (23) = happyShift action_26
action_101 (24) = happyShift action_27
action_101 (40) = happyShift action_37
action_101 (41) = happyShift action_38
action_101 (42) = happyShift action_39
action_101 (43) = happyShift action_40
action_101 (44) = happyShift action_41
action_101 (46) = happyShift action_42
action_101 (47) = happyShift action_43
action_101 (49) = happyShift action_44
action_101 (50) = happyShift action_45
action_101 (52) = happyShift action_46
action_101 (9) = happyGoto action_108
action_101 (10) = happyGoto action_36
action_101 _ = happyReduce_9

action_102 (20) = happyShift action_29
action_102 (33) = happyReduce_28
action_102 (18) = happyGoto action_28
action_102 _ = happyReduce_42

action_103 _ = happyReduce_20

action_104 _ = happyReduce_17

action_105 _ = happyReduce_26

action_106 (20) = happyShift action_17
action_106 (25) = happyShift action_18
action_106 (26) = happyShift action_19
action_106 (27) = happyShift action_20
action_106 (28) = happyShift action_21
action_106 (29) = happyShift action_22
action_106 (31) = happyShift action_23
action_106 (16) = happyGoto action_107
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (22) = happyShift action_25
action_107 (23) = happyShift action_26
action_107 (24) = happyShift action_27
action_107 (45) = happyShift action_113
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_22

action_109 (20) = happyShift action_17
action_109 (25) = happyShift action_18
action_109 (26) = happyShift action_19
action_109 (27) = happyShift action_20
action_109 (28) = happyShift action_21
action_109 (29) = happyShift action_22
action_109 (31) = happyShift action_23
action_109 (16) = happyGoto action_112
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (38) = happyShift action_111
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_16

action_112 (22) = happyShift action_25
action_112 (23) = happyShift action_26
action_112 (24) = happyShift action_27
action_112 (40) = happyShift action_37
action_112 (41) = happyShift action_38
action_112 (42) = happyShift action_39
action_112 (43) = happyShift action_40
action_112 (44) = happyShift action_41
action_112 (46) = happyShift action_42
action_112 (47) = happyShift action_43
action_112 (49) = happyShift action_44
action_112 (50) = happyShift action_45
action_112 (52) = happyShift action_46
action_112 (9) = happyGoto action_115
action_112 (10) = happyGoto action_36
action_112 _ = happyReduce_9

action_113 (31) = happyShift action_10
action_113 (39) = happyShift action_11
action_113 (14) = happyGoto action_114
action_113 (15) = happyGoto action_82
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_19

action_115 _ = happyReduce_23

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAxiom happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DTheorem happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  9 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 ([]
	)

happyReduce_10 = happyReduce 4 10 happyReduction_10
happyReduction_10 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSSuppose happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSThusBy happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSThusBy happy_var_2 (["-"] ++ happy_var_3)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 (["-"] ++ happy_var_5)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSEquiv happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 7 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSClaim happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 10 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSCases happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSTake happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 8 10 happyReduction_19
happyReduction_19 ((HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSConsider happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  11 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  11 happyReduction_21
happyReduction_21  =  HappyAbsSyn11
		 ([]
	)

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (("-", happy_var_2, happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_2, happy_var_4, happy_var_5)
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_2  13 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  13 happyReduction_25
happyReduction_25  =  HappyAbsSyn13
		 ([]
	)

happyReduce_26 = happySpecReduce_3  14 happyReduction_26
happyReduction_26 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  14 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn13
		 ([ happy_var_1 ]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15 happyReduction_29
happyReduction_29 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  16 happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn16
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  16 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  16 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (FNot happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 16 happyReduction_35
happyReduction_35 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 4 16 happyReduction_36
happyReduction_36 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happySpecReduce_1  16 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn16
		 (FTrue
	)

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn16
		 (FFalse
	)

happyReduce_39 = happySpecReduce_3  16 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  17 happyReduction_40
happyReduction_40 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn17
		 (TVar happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  17 happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn17
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_0  18 happyReduction_42
happyReduction_42  =  HappyAbsSyn18
		 ([]
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
happyReduction_44 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  19 happyReduction_45
happyReduction_45 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 54 54 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 20;
	Token _ TokenParenClose -> cont 21;
	Token _ TokenAnd -> cont 22;
	Token _ TokenOr -> cont 23;
	Token _ TokenImp -> cont 24;
	Token _ TokenNot -> cont 25;
	Token _ TokenTrue -> cont 26;
	Token _ TokenFalse -> cont 27;
	Token _ TokenForall -> cont 28;
	Token _ TokenExists -> cont 29;
	Token _ TokenDot -> cont 30;
	Token _ (TokenId happy_dollar_dollar) -> cont 31;
	Token _ (TokenVar happy_dollar_dollar) -> cont 32;
	Token _ TokenDoubleColon -> cont 33;
	Token _ TokenComma -> cont 34;
	Token _ TokenAxiom -> cont 35;
	Token _ TokenTheorem -> cont 36;
	Token _ TokenProof -> cont 37;
	Token _ TokenEnd -> cont 38;
	Token _ (TokenQuotedName happy_dollar_dollar) -> cont 39;
	Token _ TokenSuppose -> cont 40;
	Token _ TokenThus -> cont 41;
	Token _ TokenThen -> cont 42;
	Token _ TokenHence -> cont 43;
	Token _ TokenHave -> cont 44;
	Token _ TokenBy -> cont 45;
	Token _ TokenEquivalently -> cont 46;
	Token _ TokenClaim -> cont 47;
	Token _ TokenCase -> cont 48;
	Token _ TokenCases -> cont 49;
	Token _ TokenTake -> cont 50;
	Token _ TokenAssign -> cont 51;
	Token _ TokenConsider -> cont 52;
	Token _ TokenSuchThat -> cont 53;
	_ -> happyError' (tk, [])
	})

happyError_ explist 54 tk = happyError' (tk, explist)
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
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

parseError :: (Token, [String]) -> Alex a
parseError ((Token p t), next) =
        alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:")
        --alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:" ++ trace ("a" ++ (show $ null next)) (show next))

parseProgram' :: FilePath -> String -> Either String Program
parseProgram' = runAlex' parse

parseProgram :: String -> Either String Program
parseProgram s = runAlex s parse
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
