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
	| HappyAbsSyn12 (Justification)
	| HappyAbsSyn13 (String)
	| HappyAbsSyn15 (Form)
	| HappyAbsSyn16 (Term)
	| HappyAbsSyn17 ([Term])

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
 action_93 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_40 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,157) ([0,0,12,0,0,12,0,0,0,0,0,12,0,0,0,0,0,0,0,8192,64,0,8192,64,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,12164,0,0,12164,0,0,112,16,0,12164,0,0,12164,0,0,0,0,0,0,0,0,16384,0,0,16384,0,0,4,0,0,112,0,0,12164,0,0,12164,0,0,12164,0,0,0,0,0,24576,0,0,4096,0,0,4096,0,0,0,0,0,120,0,0,0,28544,0,0,32,0,32768,0,0,0,28544,0,8192,64,0,12164,0,0,8192,64,0,12164,0,0,8192,64,0,12164,0,0,8192,64,0,0,0,0,12164,0,0,12164,0,0,0,2,0,8,0,0,4,0,0,0,0,0,112,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,0,0,112,0,0,112,0,0,0,1,0,112,0,0,0,1,0,112,4096,0,0,1,0,112,4096,0,0,1,0,0,0,0,0,28544,0,0,0,0,0,0,0,12164,0,0,8192,64,0,12164,0,0,8192,64,0,12164,0,0,12164,0,0,0,0,0,112,16,0,112,4096,0,0,0,0,0,2,0,112,4096,0,0,0,0,112,0,0,8192,64,0,8192,64,0,8192,64,0,0,28544,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","ProofStepBlock","Justification","OptionalHyp","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","';'","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","equivalently","claim","%eof"]
        bit_start = st Prelude.* 48
        bit_end = (st Prelude.+ 1) Prelude.* 48
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..47]
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

action_6 (30) = happyShift action_10
action_6 (39) = happyShift action_11
action_6 (14) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (30) = happyShift action_10
action_7 (39) = happyShift action_11
action_7 (14) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (48) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (33) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_23

action_11 _ = happyReduce_24

action_12 (33) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_2

action_14 (19) = happyShift action_17
action_14 (24) = happyShift action_18
action_14 (25) = happyShift action_19
action_14 (26) = happyShift action_20
action_14 (27) = happyShift action_21
action_14 (28) = happyShift action_22
action_14 (30) = happyShift action_23
action_14 (15) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (19) = happyShift action_17
action_15 (24) = happyShift action_18
action_15 (25) = happyShift action_19
action_15 (26) = happyShift action_20
action_15 (27) = happyShift action_21
action_15 (28) = happyShift action_22
action_15 (30) = happyShift action_23
action_15 (15) = happyGoto action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (21) = happyShift action_25
action_16 (22) = happyShift action_26
action_16 (23) = happyShift action_27
action_16 (37) = happyShift action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (19) = happyShift action_17
action_17 (24) = happyShift action_18
action_17 (25) = happyShift action_19
action_17 (26) = happyShift action_20
action_17 (27) = happyShift action_21
action_17 (28) = happyShift action_22
action_17 (30) = happyShift action_23
action_17 (15) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (19) = happyShift action_17
action_18 (24) = happyShift action_18
action_18 (25) = happyShift action_19
action_18 (26) = happyShift action_20
action_18 (27) = happyShift action_21
action_18 (28) = happyShift action_22
action_18 (30) = happyShift action_23
action_18 (15) = happyGoto action_32
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_32

action_20 _ = happyReduce_33

action_21 (31) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (31) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (19) = happyShift action_29
action_23 (17) = happyGoto action_28
action_23 _ = happyReduce_37

action_24 (21) = happyShift action_25
action_24 (22) = happyShift action_26
action_24 (23) = happyShift action_27
action_24 _ = happyReduce_6

action_25 (19) = happyShift action_17
action_25 (24) = happyShift action_18
action_25 (25) = happyShift action_19
action_25 (26) = happyShift action_20
action_25 (27) = happyShift action_21
action_25 (28) = happyShift action_22
action_25 (30) = happyShift action_23
action_25 (15) = happyGoto action_54
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (19) = happyShift action_17
action_26 (24) = happyShift action_18
action_26 (25) = happyShift action_19
action_26 (26) = happyShift action_20
action_26 (27) = happyShift action_21
action_26 (28) = happyShift action_22
action_26 (30) = happyShift action_23
action_26 (15) = happyGoto action_53
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (19) = happyShift action_17
action_27 (24) = happyShift action_18
action_27 (25) = happyShift action_19
action_27 (26) = happyShift action_20
action_27 (27) = happyShift action_21
action_27 (28) = happyShift action_22
action_27 (30) = happyShift action_23
action_27 (15) = happyGoto action_52
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_25

action_29 (30) = happyShift action_50
action_29 (31) = happyShift action_51
action_29 (16) = happyGoto action_48
action_29 (18) = happyGoto action_49
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (29) = happyShift action_47
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (29) = happyShift action_46
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_29

action_33 (20) = happyShift action_45
action_33 (21) = happyShift action_25
action_33 (22) = happyShift action_26
action_33 (23) = happyShift action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (40) = happyShift action_38
action_34 (41) = happyShift action_39
action_34 (42) = happyShift action_40
action_34 (43) = happyShift action_41
action_34 (44) = happyShift action_42
action_34 (46) = happyShift action_43
action_34 (47) = happyShift action_44
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_36
action_34 (11) = happyGoto action_37
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (38) = happyShift action_69
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (32) = happyShift action_68
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (40) = happyShift action_38
action_37 (41) = happyShift action_39
action_37 (42) = happyShift action_40
action_37 (43) = happyShift action_41
action_37 (44) = happyShift action_42
action_37 (46) = happyShift action_43
action_37 (47) = happyShift action_44
action_37 (9) = happyGoto action_67
action_37 (10) = happyGoto action_36
action_37 (11) = happyGoto action_37
action_37 _ = happyReduce_11

action_38 (30) = happyShift action_10
action_38 (39) = happyShift action_11
action_38 (14) = happyGoto action_66
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (19) = happyShift action_17
action_39 (24) = happyShift action_18
action_39 (25) = happyShift action_19
action_39 (26) = happyShift action_20
action_39 (27) = happyShift action_21
action_39 (28) = happyShift action_22
action_39 (30) = happyShift action_23
action_39 (15) = happyGoto action_65
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (30) = happyShift action_10
action_40 (39) = happyShift action_11
action_40 (14) = happyGoto action_64
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (19) = happyShift action_17
action_41 (24) = happyShift action_18
action_41 (25) = happyShift action_19
action_41 (26) = happyShift action_20
action_41 (27) = happyShift action_21
action_41 (28) = happyShift action_22
action_41 (30) = happyShift action_23
action_41 (15) = happyGoto action_63
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (30) = happyShift action_10
action_42 (39) = happyShift action_11
action_42 (14) = happyGoto action_62
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (19) = happyShift action_17
action_43 (24) = happyShift action_18
action_43 (25) = happyShift action_19
action_43 (26) = happyShift action_20
action_43 (27) = happyShift action_21
action_43 (28) = happyShift action_22
action_43 (30) = happyShift action_23
action_43 (15) = happyGoto action_61
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (30) = happyShift action_10
action_44 (39) = happyShift action_11
action_44 (14) = happyGoto action_60
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_34

action_46 (19) = happyShift action_17
action_46 (24) = happyShift action_18
action_46 (25) = happyShift action_19
action_46 (26) = happyShift action_20
action_46 (27) = happyShift action_21
action_46 (28) = happyShift action_22
action_46 (30) = happyShift action_23
action_46 (15) = happyGoto action_59
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (19) = happyShift action_17
action_47 (24) = happyShift action_18
action_47 (25) = happyShift action_19
action_47 (26) = happyShift action_20
action_47 (27) = happyShift action_21
action_47 (28) = happyShift action_22
action_47 (30) = happyShift action_23
action_47 (15) = happyGoto action_58
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (34) = happyShift action_57
action_48 _ = happyReduce_39

action_49 (20) = happyShift action_56
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (19) = happyShift action_29
action_50 (17) = happyGoto action_55
action_50 _ = happyReduce_37

action_51 _ = happyReduce_35

action_52 (21) = happyShift action_25
action_52 (22) = happyShift action_26
action_52 (23) = happyShift action_27
action_52 _ = happyReduce_28

action_53 _ = happyReduce_27

action_54 _ = happyReduce_26

action_55 _ = happyReduce_36

action_56 _ = happyReduce_38

action_57 (30) = happyShift action_50
action_57 (31) = happyShift action_51
action_57 (16) = happyGoto action_48
action_57 (18) = happyGoto action_77
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (21) = happyShift action_25
action_58 (22) = happyShift action_26
action_58 (23) = happyShift action_27
action_58 _ = happyReduce_30

action_59 (21) = happyShift action_25
action_59 (22) = happyShift action_26
action_59 (23) = happyShift action_27
action_59 _ = happyReduce_31

action_60 (33) = happyShift action_76
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (21) = happyShift action_25
action_61 (22) = happyShift action_26
action_61 (23) = happyShift action_27
action_61 _ = happyReduce_17

action_62 (33) = happyShift action_75
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (21) = happyShift action_25
action_63 (22) = happyShift action_26
action_63 (23) = happyShift action_27
action_63 (45) = happyShift action_74
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (33) = happyShift action_73
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (21) = happyShift action_25
action_65 (22) = happyShift action_26
action_65 (23) = happyShift action_27
action_65 (45) = happyShift action_72
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (33) = happyShift action_71
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_10

action_68 (40) = happyShift action_38
action_68 (41) = happyShift action_39
action_68 (42) = happyShift action_40
action_68 (43) = happyShift action_41
action_68 (44) = happyShift action_42
action_68 (46) = happyShift action_43
action_68 (47) = happyShift action_44
action_68 (9) = happyGoto action_70
action_68 (10) = happyGoto action_36
action_68 (11) = happyGoto action_37
action_68 _ = happyReduce_9

action_69 _ = happyReduce_7

action_70 _ = happyReduce_8

action_71 (19) = happyShift action_17
action_71 (24) = happyShift action_18
action_71 (25) = happyShift action_19
action_71 (26) = happyShift action_20
action_71 (27) = happyShift action_21
action_71 (28) = happyShift action_22
action_71 (30) = happyShift action_23
action_71 (15) = happyGoto action_84
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (30) = happyShift action_10
action_72 (39) = happyShift action_11
action_72 (12) = happyGoto action_83
action_72 (14) = happyGoto action_81
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (19) = happyShift action_17
action_73 (24) = happyShift action_18
action_73 (25) = happyShift action_19
action_73 (26) = happyShift action_20
action_73 (27) = happyShift action_21
action_73 (28) = happyShift action_22
action_73 (30) = happyShift action_23
action_73 (15) = happyGoto action_82
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (30) = happyShift action_10
action_74 (39) = happyShift action_11
action_74 (12) = happyGoto action_80
action_74 (14) = happyGoto action_81
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (19) = happyShift action_17
action_75 (24) = happyShift action_18
action_75 (25) = happyShift action_19
action_75 (26) = happyShift action_20
action_75 (27) = happyShift action_21
action_75 (28) = happyShift action_22
action_75 (30) = happyShift action_23
action_75 (15) = happyGoto action_79
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (19) = happyShift action_17
action_76 (24) = happyShift action_18
action_76 (25) = happyShift action_19
action_76 (26) = happyShift action_20
action_76 (27) = happyShift action_21
action_76 (28) = happyShift action_22
action_76 (30) = happyShift action_23
action_76 (15) = happyGoto action_78
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_40

action_78 (21) = happyShift action_25
action_78 (22) = happyShift action_26
action_78 (23) = happyShift action_27
action_78 (37) = happyShift action_88
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (21) = happyShift action_25
action_79 (22) = happyShift action_26
action_79 (23) = happyShift action_27
action_79 (45) = happyShift action_87
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_14

action_81 (34) = happyShift action_86
action_81 _ = happyReduce_20

action_82 (21) = happyShift action_25
action_82 (22) = happyShift action_26
action_82 (23) = happyShift action_27
action_82 (45) = happyShift action_85
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_13

action_84 (21) = happyShift action_25
action_84 (22) = happyShift action_26
action_84 (23) = happyShift action_27
action_84 _ = happyReduce_12

action_85 (30) = happyShift action_10
action_85 (39) = happyShift action_11
action_85 (12) = happyGoto action_92
action_85 (14) = happyGoto action_81
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (30) = happyShift action_10
action_86 (39) = happyShift action_11
action_86 (12) = happyGoto action_91
action_86 (14) = happyGoto action_81
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (30) = happyShift action_10
action_87 (39) = happyShift action_11
action_87 (12) = happyGoto action_90
action_87 (14) = happyGoto action_81
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (40) = happyShift action_38
action_88 (41) = happyShift action_39
action_88 (42) = happyShift action_40
action_88 (43) = happyShift action_41
action_88 (44) = happyShift action_42
action_88 (46) = happyShift action_43
action_88 (47) = happyShift action_44
action_88 (9) = happyGoto action_89
action_88 (10) = happyGoto action_36
action_88 (11) = happyGoto action_37
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (38) = happyShift action_93
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_15

action_91 _ = happyReduce_19

action_92 _ = happyReduce_16

action_93 _ = happyReduce_18

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
happyReduction_6 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAxiom happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
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

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ([ happy_var_1 ]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 10 happyReduction_12
happyReduction_12 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSSuppose happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 10 happyReduction_13
happyReduction_13 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSThusBy happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSThusBy happy_var_2 (["-"] ++ happy_var_4)
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 6 10 happyReduction_15
happyReduction_15 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSHaveBy happy_var_2 happy_var_4 (["-"] ++ happy_var_6)
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PSEquiv happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 7 11 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSClaim happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([ happy_var_1 ]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_0  13 happyReduction_21
happyReduction_21  =  HappyAbsSyn13
		 (""
	)

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  14 happyReduction_24
happyReduction_24 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  15 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn15
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  15 happyReduction_28
happyReduction_28 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  15 happyReduction_29
happyReduction_29 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (FNot happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 4 15 happyReduction_30
happyReduction_30 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 4 15 happyReduction_31
happyReduction_31 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn15
		 (FTrue
	)

happyReduce_33 = happySpecReduce_1  15 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn15
		 (FFalse
	)

happyReduce_34 = happySpecReduce_3  15 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16 happyReduction_35
happyReduction_35 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn16
		 (TVar happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_2  16 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn16
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  17 happyReduction_37
happyReduction_37  =  HappyAbsSyn17
		 ([]
	)

happyReduce_38 = happySpecReduce_3  17 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  18 happyReduction_39
happyReduction_39 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  18 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 48 48 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 19;
	Token _ TokenParenClose -> cont 20;
	Token _ TokenAnd -> cont 21;
	Token _ TokenOr -> cont 22;
	Token _ TokenImp -> cont 23;
	Token _ TokenNot -> cont 24;
	Token _ TokenTrue -> cont 25;
	Token _ TokenFalse -> cont 26;
	Token _ TokenForall -> cont 27;
	Token _ TokenExists -> cont 28;
	Token _ TokenDot -> cont 29;
	Token _ (TokenId happy_dollar_dollar) -> cont 30;
	Token _ (TokenVar happy_dollar_dollar) -> cont 31;
	Token _ TokenSemicolon -> cont 32;
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
	_ -> happyError' (tk, [])
	})

happyError_ explist 48 tk = happyError' (tk, explist)
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
