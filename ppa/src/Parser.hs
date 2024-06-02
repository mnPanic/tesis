{-# OPTIONS_GHC -w #-}
module Parser(parseProgram, parseProgram') where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification, Case, VarRename )
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
	| HappyAbsSyn11 (VarRename)
	| HappyAbsSyn12 ([Case])
	| HappyAbsSyn13 (Case)
	| HappyAbsSyn14 (Justification)
	| HappyAbsSyn16 (String)
	| HappyAbsSyn17 (Form)
	| HappyAbsSyn18 (Term)
	| HappyAbsSyn19 ([Term])

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
 action_119 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,207) ([0,0,24,0,0,6144,0,0,0,0,0,0,6144,0,0,0,0,0,0,0,0,0,32768,128,0,0,32896,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,48656,0,0,4096,190,0,0,448,32,0,4096,190,0,0,48656,0,0,0,0,0,0,0,0,0,0,256,0,0,0,1,0,4096,0,0,0,448,0,0,4096,190,0,0,48656,0,0,4096,190,0,0,0,0,0,0,384,0,0,16384,0,0,0,64,0,0,0,0,0,57344,1,0,0,0,57088,86,0,16384,0,0,0,57088,86,0,32896,0,0,48656,0,0,0,32896,0,0,48656,0,0,0,32896,0,0,48656,0,0,0,32896,0,0,0,8192,0,0,256,0,0,0,1,0,0,256,0,0,0,0,0,4096,190,0,0,48656,0,0,0,1024,0,0,32,0,0,4096,0,0,0,0,0,0,49152,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,1,0,49152,1,0,0,448,0,0,0,0,0,0,0,0,8,0,0,8192,0,0,0,8,0,0,256,0,32768,128,0,0,512,0,0,448,0,0,0,512,0,0,448,8192,0,0,512,0,0,448,8192,0,0,512,0,0,0,0,0,0,0,0,0,48656,0,0,0,0,0,0,48656,0,0,0,0,0,0,48656,0,0,4096,190,0,0,0,0,0,0,1024,0,0,0,64,0,0,0,256,0,48656,128,0,0,384,0,0,32768,128,0,0,256,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2,0,49152,1,22239,0,16,0,0,0,0,0,0,0,0,0,0,32896,0,0,448,32,0,49152,1,32,0,448,8192,0,49152,1,0,0,0,0,0,0,0,0,0,0,57088,86,0,0,0,0,0,0,0,4096,190,0,0,48656,0,0,49152,1,32,0,448,57088,86,0,16384,0,0,0,0,0,0,0,0,0,32768,128,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","OptVarRename","Cases","Case","OptionalBy","Justification","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","equivalently","claim","case","cases","take","':='","consider","st","let","%eof"]
        bit_start = st Prelude.* 56
        bit_end = (st Prelude.+ 1) Prelude.* 56
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..55]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (36) = happyShift action_6
action_0 (37) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (36) = happyShift action_6
action_1 (37) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (36) = happyShift action_6
action_3 (37) = happyShift action_7
action_3 (5) = happyGoto action_13
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (32) = happyShift action_10
action_6 (40) = happyShift action_11
action_6 (16) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (32) = happyShift action_10
action_7 (40) = happyShift action_11
action_7 (16) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (56) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (34) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_31

action_11 _ = happyReduce_32

action_12 (34) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_2

action_14 (21) = happyShift action_17
action_14 (26) = happyShift action_18
action_14 (27) = happyShift action_19
action_14 (28) = happyShift action_20
action_14 (29) = happyShift action_21
action_14 (30) = happyShift action_22
action_14 (32) = happyShift action_23
action_14 (17) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (21) = happyShift action_17
action_15 (26) = happyShift action_18
action_15 (27) = happyShift action_19
action_15 (28) = happyShift action_20
action_15 (29) = happyShift action_21
action_15 (30) = happyShift action_22
action_15 (32) = happyShift action_23
action_15 (17) = happyGoto action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (23) = happyShift action_25
action_16 (24) = happyShift action_26
action_16 (25) = happyShift action_27
action_16 (38) = happyShift action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_17
action_17 (26) = happyShift action_18
action_17 (27) = happyShift action_19
action_17 (28) = happyShift action_20
action_17 (29) = happyShift action_21
action_17 (30) = happyShift action_22
action_17 (32) = happyShift action_23
action_17 (17) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (21) = happyShift action_17
action_18 (26) = happyShift action_18
action_18 (27) = happyShift action_19
action_18 (28) = happyShift action_20
action_18 (29) = happyShift action_21
action_18 (30) = happyShift action_22
action_18 (32) = happyShift action_23
action_18 (17) = happyGoto action_32
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_40

action_20 _ = happyReduce_41

action_21 (33) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (33) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (21) = happyShift action_29
action_23 (19) = happyGoto action_28
action_23 _ = happyReduce_45

action_24 (23) = happyShift action_25
action_24 (24) = happyShift action_26
action_24 (25) = happyShift action_27
action_24 _ = happyReduce_6

action_25 (21) = happyShift action_17
action_25 (26) = happyShift action_18
action_25 (27) = happyShift action_19
action_25 (28) = happyShift action_20
action_25 (29) = happyShift action_21
action_25 (30) = happyShift action_22
action_25 (32) = happyShift action_23
action_25 (17) = happyGoto action_57
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (21) = happyShift action_17
action_26 (26) = happyShift action_18
action_26 (27) = happyShift action_19
action_26 (28) = happyShift action_20
action_26 (29) = happyShift action_21
action_26 (30) = happyShift action_22
action_26 (32) = happyShift action_23
action_26 (17) = happyGoto action_56
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (21) = happyShift action_17
action_27 (26) = happyShift action_18
action_27 (27) = happyShift action_19
action_27 (28) = happyShift action_20
action_27 (29) = happyShift action_21
action_27 (30) = happyShift action_22
action_27 (32) = happyShift action_23
action_27 (17) = happyGoto action_55
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_33

action_29 (32) = happyShift action_53
action_29 (33) = happyShift action_54
action_29 (18) = happyGoto action_51
action_29 (20) = happyGoto action_52
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (31) = happyShift action_50
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_49
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_37

action_33 (22) = happyShift action_48
action_33 (23) = happyShift action_25
action_33 (24) = happyShift action_26
action_33 (25) = happyShift action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (41) = happyShift action_37
action_34 (42) = happyShift action_38
action_34 (43) = happyShift action_39
action_34 (44) = happyShift action_40
action_34 (45) = happyShift action_41
action_34 (47) = happyShift action_42
action_34 (48) = happyShift action_43
action_34 (50) = happyShift action_44
action_34 (51) = happyShift action_45
action_34 (53) = happyShift action_46
action_34 (55) = happyShift action_47
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_36
action_34 _ = happyReduce_9

action_35 (39) = happyShift action_77
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (41) = happyShift action_37
action_36 (42) = happyShift action_38
action_36 (43) = happyShift action_39
action_36 (44) = happyShift action_40
action_36 (45) = happyShift action_41
action_36 (47) = happyShift action_42
action_36 (48) = happyShift action_43
action_36 (50) = happyShift action_44
action_36 (51) = happyShift action_45
action_36 (53) = happyShift action_46
action_36 (55) = happyShift action_47
action_36 (9) = happyGoto action_76
action_36 (10) = happyGoto action_36
action_36 _ = happyReduce_9

action_37 (32) = happyShift action_10
action_37 (40) = happyShift action_11
action_37 (16) = happyGoto action_75
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (21) = happyShift action_17
action_38 (26) = happyShift action_18
action_38 (27) = happyShift action_19
action_38 (28) = happyShift action_20
action_38 (29) = happyShift action_21
action_38 (30) = happyShift action_22
action_38 (32) = happyShift action_23
action_38 (17) = happyGoto action_74
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (32) = happyShift action_10
action_39 (40) = happyShift action_11
action_39 (16) = happyGoto action_73
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (21) = happyShift action_17
action_40 (26) = happyShift action_18
action_40 (27) = happyShift action_19
action_40 (28) = happyShift action_20
action_40 (29) = happyShift action_21
action_40 (30) = happyShift action_22
action_40 (32) = happyShift action_23
action_40 (17) = happyGoto action_72
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (32) = happyShift action_10
action_41 (40) = happyShift action_11
action_41 (16) = happyGoto action_71
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (21) = happyShift action_17
action_42 (26) = happyShift action_18
action_42 (27) = happyShift action_19
action_42 (28) = happyShift action_20
action_42 (29) = happyShift action_21
action_42 (30) = happyShift action_22
action_42 (32) = happyShift action_23
action_42 (17) = happyGoto action_70
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (32) = happyShift action_10
action_43 (40) = happyShift action_11
action_43 (16) = happyGoto action_69
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (46) = happyShift action_68
action_44 (14) = happyGoto action_67
action_44 _ = happyReduce_28

action_45 (33) = happyShift action_66
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (33) = happyShift action_64
action_46 (11) = happyGoto action_65
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (33) = happyShift action_64
action_47 (11) = happyGoto action_63
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_42

action_49 (21) = happyShift action_17
action_49 (26) = happyShift action_18
action_49 (27) = happyShift action_19
action_49 (28) = happyShift action_20
action_49 (29) = happyShift action_21
action_49 (30) = happyShift action_22
action_49 (32) = happyShift action_23
action_49 (17) = happyGoto action_62
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (21) = happyShift action_17
action_50 (26) = happyShift action_18
action_50 (27) = happyShift action_19
action_50 (28) = happyShift action_20
action_50 (29) = happyShift action_21
action_50 (30) = happyShift action_22
action_50 (32) = happyShift action_23
action_50 (17) = happyGoto action_61
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (35) = happyShift action_60
action_51 _ = happyReduce_47

action_52 (22) = happyShift action_59
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (21) = happyShift action_29
action_53 (19) = happyGoto action_58
action_53 _ = happyReduce_45

action_54 _ = happyReduce_43

action_55 (23) = happyShift action_25
action_55 (24) = happyShift action_26
action_55 (25) = happyShift action_27
action_55 _ = happyReduce_36

action_56 _ = happyReduce_35

action_57 _ = happyReduce_34

action_58 _ = happyReduce_44

action_59 _ = happyReduce_46

action_60 (32) = happyShift action_53
action_60 (33) = happyShift action_54
action_60 (18) = happyGoto action_51
action_60 (20) = happyGoto action_92
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (23) = happyShift action_25
action_61 (24) = happyShift action_26
action_61 (25) = happyShift action_27
action_61 _ = happyReduce_38

action_62 (23) = happyShift action_25
action_62 (24) = happyShift action_26
action_62 (25) = happyShift action_27
action_62 _ = happyReduce_39

action_63 _ = happyReduce_19

action_64 (52) = happyShift action_91
action_64 _ = happyReduce_21

action_65 (54) = happyShift action_90
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (52) = happyShift action_89
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (49) = happyShift action_88
action_67 (12) = happyGoto action_86
action_67 (13) = happyGoto action_87
action_67 _ = happyReduce_24

action_68 (32) = happyShift action_10
action_68 (40) = happyShift action_11
action_68 (15) = happyGoto action_84
action_68 (16) = happyGoto action_85
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (34) = happyShift action_83
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (23) = happyShift action_25
action_70 (24) = happyShift action_26
action_70 (25) = happyShift action_27
action_70 _ = happyReduce_15

action_71 (34) = happyShift action_82
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (23) = happyShift action_25
action_72 (24) = happyShift action_26
action_72 (25) = happyShift action_27
action_72 (46) = happyShift action_68
action_72 (14) = happyGoto action_81
action_72 _ = happyReduce_28

action_73 (34) = happyShift action_80
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (23) = happyShift action_25
action_74 (24) = happyShift action_26
action_74 (25) = happyShift action_27
action_74 (46) = happyShift action_68
action_74 (14) = happyGoto action_79
action_74 _ = happyReduce_28

action_75 (34) = happyShift action_78
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_8

action_77 _ = happyReduce_7

action_78 (21) = happyShift action_17
action_78 (26) = happyShift action_18
action_78 (27) = happyShift action_19
action_78 (28) = happyShift action_20
action_78 (29) = happyShift action_21
action_78 (30) = happyShift action_22
action_78 (32) = happyShift action_23
action_78 (17) = happyGoto action_105
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_11

action_80 (21) = happyShift action_17
action_80 (26) = happyShift action_18
action_80 (27) = happyShift action_19
action_80 (28) = happyShift action_20
action_80 (29) = happyShift action_21
action_80 (30) = happyShift action_22
action_80 (32) = happyShift action_23
action_80 (17) = happyGoto action_104
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_12

action_82 (21) = happyShift action_17
action_82 (26) = happyShift action_18
action_82 (27) = happyShift action_19
action_82 (28) = happyShift action_20
action_82 (29) = happyShift action_21
action_82 (30) = happyShift action_22
action_82 (32) = happyShift action_23
action_82 (17) = happyGoto action_103
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (21) = happyShift action_17
action_83 (26) = happyShift action_18
action_83 (27) = happyShift action_19
action_83 (28) = happyShift action_20
action_83 (29) = happyShift action_21
action_83 (30) = happyShift action_22
action_83 (32) = happyShift action_23
action_83 (17) = happyGoto action_102
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_27

action_85 (35) = happyShift action_101
action_85 _ = happyReduce_30

action_86 (39) = happyShift action_100
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (49) = happyShift action_88
action_87 (12) = happyGoto action_99
action_87 (13) = happyGoto action_87
action_87 _ = happyReduce_24

action_88 (21) = happyShift action_17
action_88 (26) = happyShift action_18
action_88 (27) = happyShift action_19
action_88 (28) = happyShift action_20
action_88 (29) = happyShift action_21
action_88 (30) = happyShift action_22
action_88 (32) = happyShift action_98
action_88 (40) = happyShift action_11
action_88 (16) = happyGoto action_96
action_88 (17) = happyGoto action_97
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (32) = happyShift action_53
action_89 (33) = happyShift action_54
action_89 (18) = happyGoto action_95
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (32) = happyShift action_10
action_90 (40) = happyShift action_11
action_90 (16) = happyGoto action_94
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (33) = happyShift action_93
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_48

action_93 _ = happyReduce_22

action_94 (34) = happyShift action_112
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_18

action_96 (34) = happyShift action_111
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (23) = happyShift action_25
action_97 (24) = happyShift action_26
action_97 (25) = happyShift action_27
action_97 (41) = happyShift action_37
action_97 (42) = happyShift action_38
action_97 (43) = happyShift action_39
action_97 (44) = happyShift action_40
action_97 (45) = happyShift action_41
action_97 (47) = happyShift action_42
action_97 (48) = happyShift action_43
action_97 (50) = happyShift action_44
action_97 (51) = happyShift action_45
action_97 (53) = happyShift action_46
action_97 (55) = happyShift action_47
action_97 (9) = happyGoto action_110
action_97 (10) = happyGoto action_36
action_97 _ = happyReduce_9

action_98 (21) = happyShift action_29
action_98 (34) = happyReduce_31
action_98 (19) = happyGoto action_28
action_98 _ = happyReduce_45

action_99 _ = happyReduce_23

action_100 _ = happyReduce_17

action_101 (32) = happyShift action_10
action_101 (40) = happyShift action_11
action_101 (15) = happyGoto action_109
action_101 (16) = happyGoto action_85
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (23) = happyShift action_25
action_102 (24) = happyShift action_26
action_102 (25) = happyShift action_27
action_102 (38) = happyShift action_108
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (23) = happyShift action_25
action_103 (24) = happyShift action_26
action_103 (25) = happyShift action_27
action_103 (46) = happyShift action_68
action_103 (14) = happyGoto action_107
action_103 _ = happyReduce_28

action_104 (23) = happyShift action_25
action_104 (24) = happyShift action_26
action_104 (25) = happyShift action_27
action_104 (46) = happyShift action_68
action_104 (14) = happyGoto action_106
action_104 _ = happyReduce_28

action_105 (23) = happyShift action_25
action_105 (24) = happyShift action_26
action_105 (25) = happyShift action_27
action_105 _ = happyReduce_10

action_106 _ = happyReduce_14

action_107 _ = happyReduce_13

action_108 (41) = happyShift action_37
action_108 (42) = happyShift action_38
action_108 (43) = happyShift action_39
action_108 (44) = happyShift action_40
action_108 (45) = happyShift action_41
action_108 (47) = happyShift action_42
action_108 (48) = happyShift action_43
action_108 (50) = happyShift action_44
action_108 (51) = happyShift action_45
action_108 (53) = happyShift action_46
action_108 (55) = happyShift action_47
action_108 (9) = happyGoto action_115
action_108 (10) = happyGoto action_36
action_108 _ = happyReduce_9

action_109 _ = happyReduce_29

action_110 _ = happyReduce_25

action_111 (21) = happyShift action_17
action_111 (26) = happyShift action_18
action_111 (27) = happyShift action_19
action_111 (28) = happyShift action_20
action_111 (29) = happyShift action_21
action_111 (30) = happyShift action_22
action_111 (32) = happyShift action_23
action_111 (17) = happyGoto action_114
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (21) = happyShift action_17
action_112 (26) = happyShift action_18
action_112 (27) = happyShift action_19
action_112 (28) = happyShift action_20
action_112 (29) = happyShift action_21
action_112 (30) = happyShift action_22
action_112 (32) = happyShift action_23
action_112 (17) = happyGoto action_113
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (23) = happyShift action_25
action_113 (24) = happyShift action_26
action_113 (25) = happyShift action_27
action_113 (46) = happyShift action_118
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (23) = happyShift action_25
action_114 (24) = happyShift action_26
action_114 (25) = happyShift action_27
action_114 (41) = happyShift action_37
action_114 (42) = happyShift action_38
action_114 (43) = happyShift action_39
action_114 (44) = happyShift action_40
action_114 (45) = happyShift action_41
action_114 (47) = happyShift action_42
action_114 (48) = happyShift action_43
action_114 (50) = happyShift action_44
action_114 (51) = happyShift action_45
action_114 (53) = happyShift action_46
action_114 (55) = happyShift action_47
action_114 (9) = happyGoto action_117
action_114 (10) = happyGoto action_36
action_114 _ = happyReduce_9

action_115 (39) = happyShift action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_16

action_117 _ = happyReduce_26

action_118 (32) = happyShift action_10
action_118 (40) = happyShift action_11
action_118 (15) = happyGoto action_119
action_118 (16) = happyGoto action_85
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_20

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
happyReduction_6 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAxiom happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
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
happyReduction_10 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSSuppose happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn14  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSThusBy happy_var_2 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyAbsSyn14  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSThusBy happy_var_2 (["-"] ++ happy_var_3)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn14  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 (["-"] ++ happy_var_5)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSEquiv happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 7 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSClaim happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 10 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSCases happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSTake happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  10 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSLet happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happyReduce 8 10 happyReduction_20
happyReduction_20 ((HappyAbsSyn14  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSConsider happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_1)
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyTerminal (Token _ (TokenVar happy_var_3)))
	_
	(HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn11
		 ((happy_var_1, happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  12 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  12 happyReduction_24
happyReduction_24  =  HappyAbsSyn12
		 ([]
	)

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (("-", happy_var_2, happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 13 happyReduction_26
happyReduction_26 ((HappyAbsSyn9  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((happy_var_2, happy_var_4, happy_var_5)
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  14 happyReduction_28
happyReduction_28  =  HappyAbsSyn14
		 ([]
	)

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  15 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 ([ happy_var_1 ]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  16 happyReduction_31
happyReduction_31 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  16 happyReduction_32
happyReduction_32 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  17 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn17
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  17 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  17 happyReduction_35
happyReduction_35 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  17 happyReduction_37
happyReduction_37 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (FNot happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 4 17 happyReduction_38
happyReduction_38 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 17 happyReduction_39
happyReduction_39 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  17 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn17
		 (FTrue
	)

happyReduce_41 = happySpecReduce_1  17 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn17
		 (FFalse
	)

happyReduce_42 = happySpecReduce_3  17 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  18 happyReduction_43
happyReduction_43 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn18
		 (TVar happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  18 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn18
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  19 happyReduction_45
happyReduction_45  =  HappyAbsSyn19
		 ([]
	)

happyReduce_46 = happySpecReduce_3  19 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  20 happyReduction_47
happyReduction_47 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  20 happyReduction_48
happyReduction_48 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 56 56 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 21;
	Token _ TokenParenClose -> cont 22;
	Token _ TokenAnd -> cont 23;
	Token _ TokenOr -> cont 24;
	Token _ TokenImp -> cont 25;
	Token _ TokenNot -> cont 26;
	Token _ TokenTrue -> cont 27;
	Token _ TokenFalse -> cont 28;
	Token _ TokenForall -> cont 29;
	Token _ TokenExists -> cont 30;
	Token _ TokenDot -> cont 31;
	Token _ (TokenId happy_dollar_dollar) -> cont 32;
	Token _ (TokenVar happy_dollar_dollar) -> cont 33;
	Token _ TokenDoubleColon -> cont 34;
	Token _ TokenComma -> cont 35;
	Token _ TokenAxiom -> cont 36;
	Token _ TokenTheorem -> cont 37;
	Token _ TokenProof -> cont 38;
	Token _ TokenEnd -> cont 39;
	Token _ (TokenQuotedName happy_dollar_dollar) -> cont 40;
	Token _ TokenSuppose -> cont 41;
	Token _ TokenThus -> cont 42;
	Token _ TokenThen -> cont 43;
	Token _ TokenHence -> cont 44;
	Token _ TokenHave -> cont 45;
	Token _ TokenBy -> cont 46;
	Token _ TokenEquivalently -> cont 47;
	Token _ TokenClaim -> cont 48;
	Token _ TokenCase -> cont 49;
	Token _ TokenCases -> cont 50;
	Token _ TokenTake -> cont 51;
	Token _ TokenAssign -> cont 52;
	Token _ TokenConsider -> cont 53;
	Token _ TokenSuchThat -> cont 54;
	Token _ TokenLet -> cont 55;
	_ -> happyError' (tk, [])
	})

happyError_ explist 56 tk = happyError' (tk, explist)
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
