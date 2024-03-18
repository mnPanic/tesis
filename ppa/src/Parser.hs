{-# OPTIONS_GHC -w #-}
module Parser(parseProgram) where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification )
import Lexer
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
	| HappyAbsSyn13 (Form)
	| HappyAbsSyn14 (Term)
	| HappyAbsSyn15 ([Term])

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
 action_66 :: () => Prelude.Int -> ({-HappyReduction (Alex) = -}
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
 happyReduce_31 :: () => ({-HappyReduction (Alex) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (Alex) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Alex) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,107) ([0,0,3,0,1536,0,0,0,0,6144,0,0,0,0,0,0,0,1026,0,1024,8,0,0,0,32768,0,0,0,0,0,0,0,1024,0,0,0,16384,760,0,61568,5,0,28,4,49664,23,0,12164,0,0,0,0,0,0,0,512,0,0,4,32768,0,0,7168,0,0,6082,0,33792,47,0,24328,0,0,0,0,0,3,0,256,0,0,2,0,0,0,15360,0,0,0,384,0,16384,0,0,2,0,256,2,63552,2,0,0,0,57600,11,0,6082,0,0,512,0,16,0,4096,0,0,0,0,0,7,0,0,0,0,0,0,0,0,0,0,0,0,192,0,448,0,32768,3,0,1792,8192,0,8192,0,0,24576,0,0,0,0,0,0,24328,0,0,128,1,0,0,0,0,0,0,64,0,28,0,0,8208,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Justification","Name","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","';'","':'","','","axiom","theorem","proof","end","name","suppose","thus","by","%eof"]
        bit_start = st Prelude.* 41
        bit_end = (st Prelude.+ 1) Prelude.* 41
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..40]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (33) = happyShift action_6
action_0 (34) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (33) = happyShift action_6
action_1 (34) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (33) = happyShift action_6
action_3 (34) = happyShift action_7
action_3 (5) = happyGoto action_13
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (28) = happyShift action_10
action_6 (37) = happyShift action_11
action_6 (12) = happyGoto action_12
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (28) = happyShift action_10
action_7 (37) = happyShift action_11
action_7 (12) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (41) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (31) = happyShift action_15
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_14

action_11 _ = happyReduce_15

action_12 (31) = happyShift action_14
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_2

action_14 (17) = happyShift action_17
action_14 (22) = happyShift action_18
action_14 (23) = happyShift action_19
action_14 (24) = happyShift action_20
action_14 (25) = happyShift action_21
action_14 (26) = happyShift action_22
action_14 (28) = happyShift action_23
action_14 (13) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (17) = happyShift action_17
action_15 (22) = happyShift action_18
action_15 (23) = happyShift action_19
action_15 (24) = happyShift action_20
action_15 (25) = happyShift action_21
action_15 (26) = happyShift action_22
action_15 (28) = happyShift action_23
action_15 (13) = happyGoto action_16
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (19) = happyShift action_25
action_16 (20) = happyShift action_26
action_16 (21) = happyShift action_27
action_16 (35) = happyShift action_34
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (17) = happyShift action_17
action_17 (22) = happyShift action_18
action_17 (23) = happyShift action_19
action_17 (24) = happyShift action_20
action_17 (25) = happyShift action_21
action_17 (26) = happyShift action_22
action_17 (28) = happyShift action_23
action_17 (13) = happyGoto action_33
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (17) = happyShift action_17
action_18 (22) = happyShift action_18
action_18 (23) = happyShift action_19
action_18 (24) = happyShift action_20
action_18 (25) = happyShift action_21
action_18 (26) = happyShift action_22
action_18 (28) = happyShift action_23
action_18 (13) = happyGoto action_32
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_23

action_20 _ = happyReduce_24

action_21 (29) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (29) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (17) = happyShift action_29
action_23 (15) = happyGoto action_28
action_23 _ = happyReduce_28

action_24 (19) = happyShift action_25
action_24 (20) = happyShift action_26
action_24 (21) = happyShift action_27
action_24 _ = happyReduce_6

action_25 (17) = happyShift action_17
action_25 (22) = happyShift action_18
action_25 (23) = happyShift action_19
action_25 (24) = happyShift action_20
action_25 (25) = happyShift action_21
action_25 (26) = happyShift action_22
action_25 (28) = happyShift action_23
action_25 (13) = happyGoto action_48
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (17) = happyShift action_17
action_26 (22) = happyShift action_18
action_26 (23) = happyShift action_19
action_26 (24) = happyShift action_20
action_26 (25) = happyShift action_21
action_26 (26) = happyShift action_22
action_26 (28) = happyShift action_23
action_26 (13) = happyGoto action_47
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (17) = happyShift action_17
action_27 (22) = happyShift action_18
action_27 (23) = happyShift action_19
action_27 (24) = happyShift action_20
action_27 (25) = happyShift action_21
action_27 (26) = happyShift action_22
action_27 (28) = happyShift action_23
action_27 (13) = happyGoto action_46
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_16

action_29 (28) = happyShift action_44
action_29 (29) = happyShift action_45
action_29 (14) = happyGoto action_42
action_29 (16) = happyGoto action_43
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (27) = happyShift action_41
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (27) = happyShift action_40
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_20

action_33 (18) = happyShift action_39
action_33 (19) = happyShift action_25
action_33 (20) = happyShift action_26
action_33 (21) = happyShift action_27
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (38) = happyShift action_37
action_34 (39) = happyShift action_38
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (36) = happyShift action_57
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (30) = happyShift action_56
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (28) = happyShift action_10
action_37 (37) = happyShift action_11
action_37 (12) = happyGoto action_55
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (17) = happyShift action_17
action_38 (22) = happyShift action_18
action_38 (23) = happyShift action_19
action_38 (24) = happyShift action_20
action_38 (25) = happyShift action_21
action_38 (26) = happyShift action_22
action_38 (28) = happyShift action_23
action_38 (13) = happyGoto action_54
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_25

action_40 (17) = happyShift action_17
action_40 (22) = happyShift action_18
action_40 (23) = happyShift action_19
action_40 (24) = happyShift action_20
action_40 (25) = happyShift action_21
action_40 (26) = happyShift action_22
action_40 (28) = happyShift action_23
action_40 (13) = happyGoto action_53
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (17) = happyShift action_17
action_41 (22) = happyShift action_18
action_41 (23) = happyShift action_19
action_41 (24) = happyShift action_20
action_41 (25) = happyShift action_21
action_41 (26) = happyShift action_22
action_41 (28) = happyShift action_23
action_41 (13) = happyGoto action_52
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (32) = happyShift action_51
action_42 _ = happyReduce_30

action_43 (18) = happyShift action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (17) = happyShift action_29
action_44 (15) = happyGoto action_49
action_44 _ = happyReduce_28

action_45 _ = happyReduce_26

action_46 (19) = happyShift action_25
action_46 (20) = happyShift action_26
action_46 (21) = happyShift action_27
action_46 _ = happyReduce_19

action_47 _ = happyReduce_18

action_48 _ = happyReduce_17

action_49 _ = happyReduce_27

action_50 _ = happyReduce_29

action_51 (28) = happyShift action_44
action_51 (29) = happyShift action_45
action_51 (14) = happyGoto action_42
action_51 (16) = happyGoto action_61
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (19) = happyShift action_25
action_52 (20) = happyShift action_26
action_52 (21) = happyShift action_27
action_52 _ = happyReduce_21

action_53 (19) = happyShift action_25
action_53 (20) = happyShift action_26
action_53 (21) = happyShift action_27
action_53 _ = happyReduce_22

action_54 (19) = happyShift action_25
action_54 (20) = happyShift action_26
action_54 (21) = happyShift action_27
action_54 (40) = happyShift action_60
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (31) = happyShift action_59
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (38) = happyShift action_37
action_56 (39) = happyShift action_38
action_56 (9) = happyGoto action_58
action_56 (10) = happyGoto action_36
action_56 _ = happyReduce_9

action_57 _ = happyReduce_7

action_58 _ = happyReduce_8

action_59 (17) = happyShift action_17
action_59 (22) = happyShift action_18
action_59 (23) = happyShift action_19
action_59 (24) = happyShift action_20
action_59 (25) = happyShift action_21
action_59 (26) = happyShift action_22
action_59 (28) = happyShift action_23
action_59 (13) = happyGoto action_64
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (28) = happyShift action_10
action_60 (37) = happyShift action_11
action_60 (11) = happyGoto action_62
action_60 (12) = happyGoto action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_31

action_62 _ = happyReduce_11

action_63 (32) = happyShift action_65
action_63 _ = happyReduce_13

action_64 (19) = happyShift action_25
action_64 (20) = happyShift action_26
action_64 (21) = happyShift action_27
action_64 _ = happyReduce_10

action_65 (28) = happyShift action_10
action_65 (37) = happyShift action_11
action_65 (11) = happyGoto action_66
action_65 (12) = happyGoto action_63
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_12

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
happyReduction_6 ((HappyAbsSyn13  happy_var_4) `HappyStk`
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
	(HappyAbsSyn13  happy_var_4) `HappyStk`
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
happyReduction_10 ((HappyAbsSyn13  happy_var_4) `HappyStk`
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
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSThusBy happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([ happy_var_1 ]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  12 happyReduction_14
happyReduction_14 (HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  12 happyReduction_15
happyReduction_15 (HappyTerminal (Token _ (TokenQuotedName happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  13 happyReduction_16
happyReduction_16 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn13
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  13 happyReduction_20
happyReduction_20 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (FNot happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 13 happyReduction_21
happyReduction_21 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenVar happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  13 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn13
		 (FTrue
	)

happyReduce_24 = happySpecReduce_1  13 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn13
		 (FFalse
	)

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn14
		 (TVar happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (Token _ (TokenId happy_var_1)))
	 =  HappyAbsSyn14
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  15 happyReduction_28
happyReduction_28  =  HappyAbsSyn15
		 ([]
	)

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  16 happyReduction_31
happyReduction_31 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 : happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 41 41 tk (HappyState action) sts stk;
	Token _ TokenParenOpen -> cont 17;
	Token _ TokenParenClose -> cont 18;
	Token _ TokenAnd -> cont 19;
	Token _ TokenOr -> cont 20;
	Token _ TokenImp -> cont 21;
	Token _ TokenNot -> cont 22;
	Token _ TokenTrue -> cont 23;
	Token _ TokenFalse -> cont 24;
	Token _ TokenForall -> cont 25;
	Token _ TokenExists -> cont 26;
	Token _ TokenDot -> cont 27;
	Token _ (TokenId happy_dollar_dollar) -> cont 28;
	Token _ (TokenVar happy_dollar_dollar) -> cont 29;
	Token _ TokenSemicolon -> cont 30;
	Token _ TokenDoubleColon -> cont 31;
	Token _ TokenComma -> cont 32;
	Token _ TokenAxiom -> cont 33;
	Token _ TokenTheorem -> cont 34;
	Token _ TokenProof -> cont 35;
	Token _ TokenEnd -> cont 36;
	Token _ (TokenQuotedName happy_dollar_dollar) -> cont 37;
	Token _ TokenSuppose -> cont 38;
	Token _ TokenThus -> cont 39;
	Token _ TokenBy -> cont 40;
	_ -> happyError' (tk, [])
	})

happyError_ explist 41 tk = happyError' (tk, explist)
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
  alexError' p ("parse error at token '" ++ unLex t ++ "', possible tokens:" ++ show next)

parseProgram :: FilePath -> String -> Either String Program
parseProgram = runAlex' parse
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
