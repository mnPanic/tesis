{-# OPTIONS_GHC -w #-}
module Parser(parseProgram, parseProgram') where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification )
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
	| HappyAbsSyn11 (Justification)
	| HappyAbsSyn12 (String)
	| HappyAbsSyn14 (Form)
	| HappyAbsSyn15 (Term)
	| HappyAbsSyn16 ([Term])

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
 action_84 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_37 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,142) ([0,0,6,0,32768,1,0,0,0,0,6144,0,0,0,0,0,0,0,0,513,0,16384,128,0,0,0,0,8192,0,0,0,0,0,0,0,0,128,0,0,0,0,31776,1,0,24328,0,0,56,8,32768,1520,0,8192,380,0,0,0,0,0,0,0,0,8,0,0,2,0,8,0,0,56,0,32768,1520,0,8192,380,0,2048,95,0,0,0,0,0,12,0,32768,0,0,8192,0,0,0,0,0,15,0,0,0,380,0,16384,0,0,64,0,0,2052,0,31776,1,0,16384,128,0,6082,0,0,1024,8,8192,380,0,0,0,0,49664,23,0,61568,5,0,0,16,0,16,0,0,2,0,0,0,0,32768,3,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,0,0,56,0,0,14,0,32768,3,0,0,512,0,14336,0,8,0,32,0,896,32768,0,0,2,0,0,6080,0,0,0,0,0,0,2048,95,0,0,8208,0,61568,5,0,0,513,0,24328,0,0,0,0,0,14,512,0,0,0,0,1024,0,14336,0,8,0,0,0,896,0,0,16384,128,0,4096,32,0,1024,8,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Justification","OptionalHyp","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","';'","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","equivalently","%eof"]
        bit_start = st Prelude.* 46
        bit_end = (st Prelude.+ 1) Prelude.* 46
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..45]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (34) = happyShift action_6
action_0 (35) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (34) = happyShift action_6
action_1 (35) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (34) = happyShift action_6
action_3 (35) = happyShift action_7
action_3 (5) = happyGoto action_13
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (29) = happyShift action_10
action_6 (38) = happyShift action_11
action_6 (13) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (29) = happyShift action_10
action_7 (38) = happyShift action_11
action_7 (13) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (46) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (32) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_20

action_11 _ = happyReduce_21

action_12 (32) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_2

action_14 (18) = happyShift action_17
action_14 (23) = happyShift action_18
action_14 (24) = happyShift action_19
action_14 (25) = happyShift action_20
action_14 (26) = happyShift action_21
action_14 (27) = happyShift action_22
action_14 (29) = happyShift action_23
action_14 (14) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (18) = happyShift action_17
action_15 (23) = happyShift action_18
action_15 (24) = happyShift action_19
action_15 (25) = happyShift action_20
action_15 (26) = happyShift action_21
action_15 (27) = happyShift action_22
action_15 (29) = happyShift action_23
action_15 (14) = happyGoto action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (20) = happyShift action_25
action_16 (21) = happyShift action_26
action_16 (22) = happyShift action_27
action_16 (36) = happyShift action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (18) = happyShift action_17
action_17 (23) = happyShift action_18
action_17 (24) = happyShift action_19
action_17 (25) = happyShift action_20
action_17 (26) = happyShift action_21
action_17 (27) = happyShift action_22
action_17 (29) = happyShift action_23
action_17 (14) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (18) = happyShift action_17
action_18 (23) = happyShift action_18
action_18 (24) = happyShift action_19
action_18 (25) = happyShift action_20
action_18 (26) = happyShift action_21
action_18 (27) = happyShift action_22
action_18 (29) = happyShift action_23
action_18 (14) = happyGoto action_32
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_29

action_20 _ = happyReduce_30

action_21 (30) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (30) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_29
action_23 (16) = happyGoto action_28
action_23 _ = happyReduce_34

action_24 (20) = happyShift action_25
action_24 (21) = happyShift action_26
action_24 (22) = happyShift action_27
action_24 _ = happyReduce_6

action_25 (18) = happyShift action_17
action_25 (23) = happyShift action_18
action_25 (24) = happyShift action_19
action_25 (25) = happyShift action_20
action_25 (26) = happyShift action_21
action_25 (27) = happyShift action_22
action_25 (29) = happyShift action_23
action_25 (14) = happyGoto action_52
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (18) = happyShift action_17
action_26 (23) = happyShift action_18
action_26 (24) = happyShift action_19
action_26 (25) = happyShift action_20
action_26 (26) = happyShift action_21
action_26 (27) = happyShift action_22
action_26 (29) = happyShift action_23
action_26 (14) = happyGoto action_51
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (18) = happyShift action_17
action_27 (23) = happyShift action_18
action_27 (24) = happyShift action_19
action_27 (25) = happyShift action_20
action_27 (26) = happyShift action_21
action_27 (27) = happyShift action_22
action_27 (29) = happyShift action_23
action_27 (14) = happyGoto action_50
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_22

action_29 (29) = happyShift action_48
action_29 (30) = happyShift action_49
action_29 (15) = happyGoto action_46
action_29 (17) = happyGoto action_47
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_45
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (28) = happyShift action_44
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_26

action_33 (19) = happyShift action_43
action_33 (20) = happyShift action_25
action_33 (21) = happyShift action_26
action_33 (22) = happyShift action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (39) = happyShift action_37
action_34 (40) = happyShift action_38
action_34 (41) = happyShift action_39
action_34 (42) = happyShift action_40
action_34 (43) = happyShift action_41
action_34 (45) = happyShift action_42
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (37) = happyShift action_65
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (31) = happyShift action_64
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (29) = happyShift action_10
action_37 (38) = happyShift action_11
action_37 (13) = happyGoto action_63
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (18) = happyShift action_17
action_38 (23) = happyShift action_18
action_38 (24) = happyShift action_19
action_38 (25) = happyShift action_20
action_38 (26) = happyShift action_21
action_38 (27) = happyShift action_22
action_38 (29) = happyShift action_23
action_38 (14) = happyGoto action_62
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (29) = happyShift action_10
action_39 (38) = happyShift action_11
action_39 (13) = happyGoto action_61
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (18) = happyShift action_17
action_40 (23) = happyShift action_18
action_40 (24) = happyShift action_19
action_40 (25) = happyShift action_20
action_40 (26) = happyShift action_21
action_40 (27) = happyShift action_22
action_40 (29) = happyShift action_23
action_40 (14) = happyGoto action_60
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (29) = happyShift action_10
action_41 (38) = happyShift action_11
action_41 (13) = happyGoto action_59
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (18) = happyShift action_17
action_42 (23) = happyShift action_18
action_42 (24) = happyShift action_19
action_42 (25) = happyShift action_20
action_42 (26) = happyShift action_21
action_42 (27) = happyShift action_22
action_42 (29) = happyShift action_23
action_42 (14) = happyGoto action_58
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_31

action_44 (18) = happyShift action_17
action_44 (23) = happyShift action_18
action_44 (24) = happyShift action_19
action_44 (25) = happyShift action_20
action_44 (26) = happyShift action_21
action_44 (27) = happyShift action_22
action_44 (29) = happyShift action_23
action_44 (14) = happyGoto action_57
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (18) = happyShift action_17
action_45 (23) = happyShift action_18
action_45 (24) = happyShift action_19
action_45 (25) = happyShift action_20
action_45 (26) = happyShift action_21
action_45 (27) = happyShift action_22
action_45 (29) = happyShift action_23
action_45 (14) = happyGoto action_56
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (33) = happyShift action_55
action_46 _ = happyReduce_36

action_47 (19) = happyShift action_54
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (18) = happyShift action_29
action_48 (16) = happyGoto action_53
action_48 _ = happyReduce_34

action_49 _ = happyReduce_32

action_50 (20) = happyShift action_25
action_50 (21) = happyShift action_26
action_50 (22) = happyShift action_27
action_50 _ = happyReduce_25

action_51 _ = happyReduce_24

action_52 _ = happyReduce_23

action_53 _ = happyReduce_33

action_54 _ = happyReduce_35

action_55 (29) = happyShift action_48
action_55 (30) = happyShift action_49
action_55 (15) = happyGoto action_46
action_55 (17) = happyGoto action_72
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (20) = happyShift action_25
action_56 (21) = happyShift action_26
action_56 (22) = happyShift action_27
action_56 _ = happyReduce_27

action_57 (20) = happyShift action_25
action_57 (21) = happyShift action_26
action_57 (22) = happyShift action_27
action_57 _ = happyReduce_28

action_58 (20) = happyShift action_25
action_58 (21) = happyShift action_26
action_58 (22) = happyShift action_27
action_58 _ = happyReduce_15

action_59 (32) = happyShift action_71
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (20) = happyShift action_25
action_60 (21) = happyShift action_26
action_60 (22) = happyShift action_27
action_60 (44) = happyShift action_70
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (32) = happyShift action_69
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (20) = happyShift action_25
action_62 (21) = happyShift action_26
action_62 (22) = happyShift action_27
action_62 (44) = happyShift action_68
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (32) = happyShift action_67
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (39) = happyShift action_37
action_64 (40) = happyShift action_38
action_64 (41) = happyShift action_39
action_64 (42) = happyShift action_40
action_64 (43) = happyShift action_41
action_64 (45) = happyShift action_42
action_64 (9) = happyGoto action_66
action_64 (10) = happyGoto action_36
action_64 _ = happyReduce_9

action_65 _ = happyReduce_7

action_66 _ = happyReduce_8

action_67 (18) = happyShift action_17
action_67 (23) = happyShift action_18
action_67 (24) = happyShift action_19
action_67 (25) = happyShift action_20
action_67 (26) = happyShift action_21
action_67 (27) = happyShift action_22
action_67 (29) = happyShift action_23
action_67 (14) = happyGoto action_78
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (29) = happyShift action_10
action_68 (38) = happyShift action_11
action_68 (11) = happyGoto action_77
action_68 (13) = happyGoto action_75
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (18) = happyShift action_17
action_69 (23) = happyShift action_18
action_69 (24) = happyShift action_19
action_69 (25) = happyShift action_20
action_69 (26) = happyShift action_21
action_69 (27) = happyShift action_22
action_69 (29) = happyShift action_23
action_69 (14) = happyGoto action_76
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (29) = happyShift action_10
action_70 (38) = happyShift action_11
action_70 (11) = happyGoto action_74
action_70 (13) = happyGoto action_75
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (18) = happyShift action_17
action_71 (23) = happyShift action_18
action_71 (24) = happyShift action_19
action_71 (25) = happyShift action_20
action_71 (26) = happyShift action_21
action_71 (27) = happyShift action_22
action_71 (29) = happyShift action_23
action_71 (14) = happyGoto action_73
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_37

action_73 (20) = happyShift action_25
action_73 (21) = happyShift action_26
action_73 (22) = happyShift action_27
action_73 (44) = happyShift action_81
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_12

action_75 (33) = happyShift action_80
action_75 _ = happyReduce_17

action_76 (20) = happyShift action_25
action_76 (21) = happyShift action_26
action_76 (22) = happyShift action_27
action_76 (44) = happyShift action_79
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_11

action_78 (20) = happyShift action_25
action_78 (21) = happyShift action_26
action_78 (22) = happyShift action_27
action_78 _ = happyReduce_10

action_79 (29) = happyShift action_10
action_79 (38) = happyShift action_11
action_79 (11) = happyGoto action_84
action_79 (13) = happyGoto action_75
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (29) = happyShift action_10
action_80 (38) = happyShift action_11
action_80 (11) = happyGoto action_83
action_80 (13) = happyGoto action_75
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (29) = happyShift action_10
action_81 (38) = happyShift action_11
action_81 (11) = happyGoto action_82
action_81 (13) = happyGoto action_75
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_13

action_83 _ = happyReduce_16

action_84 _ = happyReduce_14

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
happyReduction_6 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAxiom happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DTheorem happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([ happy_var_1 ]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 10 happyReduction_10
happyReduction_10 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSSuppose happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 10 happyReduction_11
happyReduction_11 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSThusBy happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 10 happyReduction_12
happyReduction_12 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHenceBy happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 6 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSThenBy happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSEquiv happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  12 happyReduction_18
happyReduction_18  =  HappyAbsSyn12
		 (""
	)

happyReduce_19 = happySpecReduce_2  12 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  13 happyReduction_21
happyReduction_21 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  14 happyReduction_22
happyReduction_22 (HappyAbsSyn16  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn14
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  14 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (FNot happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 14 happyReduction_27
happyReduction_27 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 4 14 happyReduction_28
happyReduction_28 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn14
		 (FTrue
	)

happyReduce_30 = happySpecReduce_1  14 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn14
		 (FFalse
	)

happyReduce_31 = happySpecReduce_3  14 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn15
		 (TVar happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  15 happyReduction_33
happyReduction_33 (HappyAbsSyn16  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn15
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_0  16 happyReduction_34
happyReduction_34  =  HappyAbsSyn16
		 ([]
	)

happyReduce_35 = happySpecReduce_3  16 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  17 happyReduction_36
happyReduction_36 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  17 happyReduction_37
happyReduction_37 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 46 46 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 18;
	Token _ TokenParenClose -> cont 19;
	Token _ TokenAnd -> cont 20;
	Token _ TokenOr -> cont 21;
	Token _ TokenImp -> cont 22;
	Token _ TokenNot -> cont 23;
	Token _ TokenTrue -> cont 24;
	Token _ TokenFalse -> cont 25;
	Token _ TokenForall -> cont 26;
	Token _ TokenExists -> cont 27;
	Token _ TokenDot -> cont 28;
	Token _ (TokenId happy_dollar_dollar) -> cont 29;
	Token _ (TokenVar happy_dollar_dollar) -> cont 30;
	Token _ TokenSemicolon -> cont 31;
	Token _ TokenDoubleColon -> cont 32;
	Token _ TokenComma -> cont 33;
	Token _ TokenAxiom -> cont 34;
	Token _ TokenTheorem -> cont 35;
	Token _ TokenProof -> cont 36;
	Token _ TokenEnd -> cont 37;
	Token _ (TokenQuotedName happy_dollar_dollar) -> cont 38;
	Token _ TokenSuppose -> cont 39;
	Token _ TokenThus -> cont 40;
	Token _ TokenThen -> cont 41;
	Token _ TokenHence -> cont 42;
	Token _ TokenHave -> cont 43;
	Token _ TokenBy -> cont 44;
	Token _ TokenEquivalently -> cont 45;
	_ -> happyError' (tk, [])
	})

happyError_ explist 46 tk = happyError' (tk, explist)
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
