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
 action_82 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_36 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,139) ([0,0,6,0,49152,0,0,0,0,0,768,0,0,0,0,0,0,0,1024,8,0,128,1,0,0,0,0,16,0,0,0,0,0,0,0,2048,0,0,0,0,61568,5,0,48656,0,0,56,8,16384,760,0,2048,95,0,0,0,0,0,0,0,16384,0,0,2048,0,4096,0,0,14336,0,0,63552,2,0,24328,0,0,3041,0,0,0,0,0,96,0,0,2,0,16384,0,0,0,0,32768,7,0,0,0,31,0,2048,0,0,4,0,8192,64,32768,1520,0,0,128,1,49664,23,0,0,1026,0,0,0,0,3041,0,8192,380,0,0,512,0,256,0,0,16,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,0,3072,0,49152,1,0,14336,0,0,0,16,0,224,8192,0,16384,0,32768,3,128,0,256,0,0,61440,1,0,0,0,0,0,16384,760,0,0,32832,0,57600,11,0,0,513,0,12164,0,0,0,0,49152,1,64,0,0,0,0,32,0,224,8192,0,0,0,32768,3,0,0,16416,0,0,2052,0,32768,256,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Justification","OptionalHyp","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","';'","':'","','","axiom","theorem","proof","end","name","suppose","thus","then","hence","have","by","%eof"]
        bit_start = st Prelude.* 45
        bit_end = (st Prelude.+ 1) Prelude.* 45
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..44]
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

action_8 (45) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (32) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_19

action_11 _ = happyReduce_20

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

action_19 _ = happyReduce_28

action_20 _ = happyReduce_29

action_21 (30) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (30) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_29
action_23 (16) = happyGoto action_28
action_23 _ = happyReduce_33

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
action_25 (14) = happyGoto action_51
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (18) = happyShift action_17
action_26 (23) = happyShift action_18
action_26 (24) = happyShift action_19
action_26 (25) = happyShift action_20
action_26 (26) = happyShift action_21
action_26 (27) = happyShift action_22
action_26 (29) = happyShift action_23
action_26 (14) = happyGoto action_50
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (18) = happyShift action_17
action_27 (23) = happyShift action_18
action_27 (24) = happyShift action_19
action_27 (25) = happyShift action_20
action_27 (26) = happyShift action_21
action_27 (27) = happyShift action_22
action_27 (29) = happyShift action_23
action_27 (14) = happyGoto action_49
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_21

action_29 (29) = happyShift action_47
action_29 (30) = happyShift action_48
action_29 (15) = happyGoto action_45
action_29 (17) = happyGoto action_46
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_44
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (28) = happyShift action_43
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_25

action_33 (19) = happyShift action_42
action_33 (20) = happyShift action_25
action_33 (21) = happyShift action_26
action_33 (22) = happyShift action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (39) = happyShift action_37
action_34 (40) = happyShift action_38
action_34 (41) = happyShift action_39
action_34 (42) = happyShift action_40
action_34 (43) = happyShift action_41
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (37) = happyShift action_63
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (31) = happyShift action_62
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (29) = happyShift action_10
action_37 (38) = happyShift action_11
action_37 (13) = happyGoto action_61
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (18) = happyShift action_17
action_38 (23) = happyShift action_18
action_38 (24) = happyShift action_19
action_38 (25) = happyShift action_20
action_38 (26) = happyShift action_21
action_38 (27) = happyShift action_22
action_38 (29) = happyShift action_23
action_38 (14) = happyGoto action_60
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (29) = happyShift action_10
action_39 (38) = happyShift action_11
action_39 (13) = happyGoto action_59
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (18) = happyShift action_17
action_40 (23) = happyShift action_18
action_40 (24) = happyShift action_19
action_40 (25) = happyShift action_20
action_40 (26) = happyShift action_21
action_40 (27) = happyShift action_22
action_40 (29) = happyShift action_23
action_40 (14) = happyGoto action_58
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (29) = happyShift action_10
action_41 (38) = happyShift action_11
action_41 (13) = happyGoto action_57
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_30

action_43 (18) = happyShift action_17
action_43 (23) = happyShift action_18
action_43 (24) = happyShift action_19
action_43 (25) = happyShift action_20
action_43 (26) = happyShift action_21
action_43 (27) = happyShift action_22
action_43 (29) = happyShift action_23
action_43 (14) = happyGoto action_56
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (18) = happyShift action_17
action_44 (23) = happyShift action_18
action_44 (24) = happyShift action_19
action_44 (25) = happyShift action_20
action_44 (26) = happyShift action_21
action_44 (27) = happyShift action_22
action_44 (29) = happyShift action_23
action_44 (14) = happyGoto action_55
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (33) = happyShift action_54
action_45 _ = happyReduce_35

action_46 (19) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (18) = happyShift action_29
action_47 (16) = happyGoto action_52
action_47 _ = happyReduce_33

action_48 _ = happyReduce_31

action_49 (20) = happyShift action_25
action_49 (21) = happyShift action_26
action_49 (22) = happyShift action_27
action_49 _ = happyReduce_24

action_50 _ = happyReduce_23

action_51 _ = happyReduce_22

action_52 _ = happyReduce_32

action_53 _ = happyReduce_34

action_54 (29) = happyShift action_47
action_54 (30) = happyShift action_48
action_54 (15) = happyGoto action_45
action_54 (17) = happyGoto action_70
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (20) = happyShift action_25
action_55 (21) = happyShift action_26
action_55 (22) = happyShift action_27
action_55 _ = happyReduce_26

action_56 (20) = happyShift action_25
action_56 (21) = happyShift action_26
action_56 (22) = happyShift action_27
action_56 _ = happyReduce_27

action_57 (32) = happyShift action_69
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (20) = happyShift action_25
action_58 (21) = happyShift action_26
action_58 (22) = happyShift action_27
action_58 (44) = happyShift action_68
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (32) = happyShift action_67
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (20) = happyShift action_25
action_60 (21) = happyShift action_26
action_60 (22) = happyShift action_27
action_60 (44) = happyShift action_66
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (32) = happyShift action_65
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (39) = happyShift action_37
action_62 (40) = happyShift action_38
action_62 (41) = happyShift action_39
action_62 (42) = happyShift action_40
action_62 (43) = happyShift action_41
action_62 (9) = happyGoto action_64
action_62 (10) = happyGoto action_36
action_62 _ = happyReduce_9

action_63 _ = happyReduce_7

action_64 _ = happyReduce_8

action_65 (18) = happyShift action_17
action_65 (23) = happyShift action_18
action_65 (24) = happyShift action_19
action_65 (25) = happyShift action_20
action_65 (26) = happyShift action_21
action_65 (27) = happyShift action_22
action_65 (29) = happyShift action_23
action_65 (14) = happyGoto action_76
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (29) = happyShift action_10
action_66 (38) = happyShift action_11
action_66 (11) = happyGoto action_75
action_66 (13) = happyGoto action_73
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (18) = happyShift action_17
action_67 (23) = happyShift action_18
action_67 (24) = happyShift action_19
action_67 (25) = happyShift action_20
action_67 (26) = happyShift action_21
action_67 (27) = happyShift action_22
action_67 (29) = happyShift action_23
action_67 (14) = happyGoto action_74
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (29) = happyShift action_10
action_68 (38) = happyShift action_11
action_68 (11) = happyGoto action_72
action_68 (13) = happyGoto action_73
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (18) = happyShift action_17
action_69 (23) = happyShift action_18
action_69 (24) = happyShift action_19
action_69 (25) = happyShift action_20
action_69 (26) = happyShift action_21
action_69 (27) = happyShift action_22
action_69 (29) = happyShift action_23
action_69 (14) = happyGoto action_71
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_36

action_71 (20) = happyShift action_25
action_71 (21) = happyShift action_26
action_71 (22) = happyShift action_27
action_71 (44) = happyShift action_79
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_12

action_73 (33) = happyShift action_78
action_73 _ = happyReduce_16

action_74 (20) = happyShift action_25
action_74 (21) = happyShift action_26
action_74 (22) = happyShift action_27
action_74 (44) = happyShift action_77
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_11

action_76 (20) = happyShift action_25
action_76 (21) = happyShift action_26
action_76 (22) = happyShift action_27
action_76 _ = happyReduce_10

action_77 (29) = happyShift action_10
action_77 (38) = happyShift action_11
action_77 (11) = happyGoto action_82
action_77 (13) = happyGoto action_73
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (29) = happyShift action_10
action_78 (38) = happyShift action_11
action_78 (11) = happyGoto action_81
action_78 (13) = happyGoto action_73
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (29) = happyShift action_10
action_79 (38) = happyShift action_11
action_79 (11) = happyGoto action_80
action_79 (13) = happyGoto action_73
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_13

action_81 _ = happyReduce_15

action_82 _ = happyReduce_14

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

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  11 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  12 happyReduction_17
happyReduction_17  =  HappyAbsSyn12
		 (""
	)

happyReduce_18 = happySpecReduce_2  12 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  13 happyReduction_19
happyReduction_19 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn14
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (FNot happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 4 14 happyReduction_26
happyReduction_26 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 14 happyReduction_27
happyReduction_27 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_1  14 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn14
		 (FTrue
	)

happyReduce_29 = happySpecReduce_1  14 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn14
		 (FFalse
	)

happyReduce_30 = happySpecReduce_3  14 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  15 happyReduction_31
happyReduction_31 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn15
		 (TVar happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  15 happyReduction_32
happyReduction_32 (HappyAbsSyn16  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn15
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  16 happyReduction_33
happyReduction_33  =  HappyAbsSyn16
		 ([]
	)

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1 : happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 45 45 tk (HappyState action) sts stk;
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
	_ -> happyError' (tk, [])
	})

happyError_ explist 45 tk = happyError' (tk, explist)
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
