{-# OPTIONS_GHC -w #-}
module Parser(parseExp) where

import ND ( Form(..), Term(..) )
import PPA ( TProof, ProofStep(..), Program(..), Decl(..), Justification )
import Lexer ( Token(..) )
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
	| HappyAbsSyn12 (Form)
	| HappyAbsSyn13 (Term)
	| HappyAbsSyn14 ([Term])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
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
 action_64 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

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
 happyReduce_29 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,107) ([0,32768,1,0,384,0,0,0,0,384,0,0,0,0,0,0,0,8,0,2048,0,0,0,0,32,0,8192,0,0,0,32768,1520,0,61568,5,0,14,2,61568,5,32768,1520,0,0,0,0,0,0,0,8,0,2048,0,128,0,0,14,0,61568,5,32768,1520,0,61568,5,0,0,0,0,12,0,512,0,0,2,0,0,0,3840,0,0,0,48,0,1024,0,4096,0,0,2048,32768,1520,0,0,0,32768,1520,0,61568,5,0,16384,0,256,0,32768,0,0,0,0,0,14,0,0,0,0,0,0,0,0,0,0,0,0,12,0,14,0,3584,0,0,14,64,0,32,0,0,48,0,0,0,0,0,61568,5,0,0,8,0,0,0,0,0,0,64,0,14,0,0,2048,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExp","Prog","Declarations","Declaration","Axiom","Theorem","Proof","ProofStep","Justification","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","id","var","';'","':'","','","axiom","theorem","proof","qed","name","assume","thus","by","%eof"]
        bit_start = st Prelude.* 40
        bit_end = (st Prelude.+ 1) Prelude.* 40
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..39]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (32) = happyShift action_6
action_0 (33) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (32) = happyShift action_6
action_1 (33) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (32) = happyShift action_6
action_3 (33) = happyShift action_7
action_3 (5) = happyGoto action_11
action_3 (6) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_3

action_4 _ = happyReduce_4

action_5 _ = happyReduce_5

action_6 (36) = happyShift action_10
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (36) = happyShift action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (40) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (30) = happyShift action_13
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (30) = happyShift action_12
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_2

action_12 (16) = happyShift action_15
action_12 (21) = happyShift action_16
action_12 (22) = happyShift action_17
action_12 (23) = happyShift action_18
action_12 (24) = happyShift action_19
action_12 (25) = happyShift action_20
action_12 (27) = happyShift action_21
action_12 (12) = happyGoto action_22
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (16) = happyShift action_15
action_13 (21) = happyShift action_16
action_13 (22) = happyShift action_17
action_13 (23) = happyShift action_18
action_13 (24) = happyShift action_19
action_13 (25) = happyShift action_20
action_13 (27) = happyShift action_21
action_13 (12) = happyGoto action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (18) = happyShift action_23
action_14 (19) = happyShift action_24
action_14 (20) = happyShift action_25
action_14 (34) = happyShift action_32
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (16) = happyShift action_15
action_15 (21) = happyShift action_16
action_15 (22) = happyShift action_17
action_15 (23) = happyShift action_18
action_15 (24) = happyShift action_19
action_15 (25) = happyShift action_20
action_15 (27) = happyShift action_21
action_15 (12) = happyGoto action_31
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (16) = happyShift action_15
action_16 (21) = happyShift action_16
action_16 (22) = happyShift action_17
action_16 (23) = happyShift action_18
action_16 (24) = happyShift action_19
action_16 (25) = happyShift action_20
action_16 (27) = happyShift action_21
action_16 (12) = happyGoto action_30
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_21

action_18 _ = happyReduce_22

action_19 (28) = happyShift action_29
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (28) = happyShift action_28
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (16) = happyShift action_27
action_21 (14) = happyGoto action_26
action_21 _ = happyReduce_26

action_22 (18) = happyShift action_23
action_22 (19) = happyShift action_24
action_22 (20) = happyShift action_25
action_22 _ = happyReduce_6

action_23 (16) = happyShift action_15
action_23 (21) = happyShift action_16
action_23 (22) = happyShift action_17
action_23 (23) = happyShift action_18
action_23 (24) = happyShift action_19
action_23 (25) = happyShift action_20
action_23 (27) = happyShift action_21
action_23 (12) = happyGoto action_46
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (16) = happyShift action_15
action_24 (21) = happyShift action_16
action_24 (22) = happyShift action_17
action_24 (23) = happyShift action_18
action_24 (24) = happyShift action_19
action_24 (25) = happyShift action_20
action_24 (27) = happyShift action_21
action_24 (12) = happyGoto action_45
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (16) = happyShift action_15
action_25 (21) = happyShift action_16
action_25 (22) = happyShift action_17
action_25 (23) = happyShift action_18
action_25 (24) = happyShift action_19
action_25 (25) = happyShift action_20
action_25 (27) = happyShift action_21
action_25 (12) = happyGoto action_44
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_14

action_27 (27) = happyShift action_42
action_27 (28) = happyShift action_43
action_27 (13) = happyGoto action_40
action_27 (15) = happyGoto action_41
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (26) = happyShift action_39
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (26) = happyShift action_38
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_18

action_31 (17) = happyShift action_37
action_31 (18) = happyShift action_23
action_31 (19) = happyShift action_24
action_31 (20) = happyShift action_25
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (37) = happyShift action_35
action_32 (38) = happyShift action_36
action_32 (9) = happyGoto action_33
action_32 (10) = happyGoto action_34
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (35) = happyShift action_55
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (29) = happyShift action_54
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (36) = happyShift action_53
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (16) = happyShift action_15
action_36 (21) = happyShift action_16
action_36 (22) = happyShift action_17
action_36 (23) = happyShift action_18
action_36 (24) = happyShift action_19
action_36 (25) = happyShift action_20
action_36 (27) = happyShift action_21
action_36 (12) = happyGoto action_52
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_23

action_38 (16) = happyShift action_15
action_38 (21) = happyShift action_16
action_38 (22) = happyShift action_17
action_38 (23) = happyShift action_18
action_38 (24) = happyShift action_19
action_38 (25) = happyShift action_20
action_38 (27) = happyShift action_21
action_38 (12) = happyGoto action_51
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (16) = happyShift action_15
action_39 (21) = happyShift action_16
action_39 (22) = happyShift action_17
action_39 (23) = happyShift action_18
action_39 (24) = happyShift action_19
action_39 (25) = happyShift action_20
action_39 (27) = happyShift action_21
action_39 (12) = happyGoto action_50
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (31) = happyShift action_49
action_40 _ = happyReduce_28

action_41 (17) = happyShift action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (16) = happyShift action_27
action_42 (14) = happyGoto action_47
action_42 _ = happyReduce_26

action_43 _ = happyReduce_24

action_44 (18) = happyShift action_23
action_44 (19) = happyShift action_24
action_44 (20) = happyShift action_25
action_44 _ = happyReduce_17

action_45 _ = happyReduce_16

action_46 _ = happyReduce_15

action_47 _ = happyReduce_25

action_48 _ = happyReduce_27

action_49 (27) = happyShift action_42
action_49 (28) = happyShift action_43
action_49 (13) = happyGoto action_40
action_49 (15) = happyGoto action_59
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (18) = happyShift action_23
action_50 (19) = happyShift action_24
action_50 (20) = happyShift action_25
action_50 _ = happyReduce_19

action_51 (18) = happyShift action_23
action_51 (19) = happyShift action_24
action_51 (20) = happyShift action_25
action_51 _ = happyReduce_20

action_52 (18) = happyShift action_23
action_52 (19) = happyShift action_24
action_52 (20) = happyShift action_25
action_52 (39) = happyShift action_58
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (30) = happyShift action_57
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (37) = happyShift action_35
action_54 (38) = happyShift action_36
action_54 (9) = happyGoto action_56
action_54 (10) = happyGoto action_34
action_54 _ = happyReduce_9

action_55 _ = happyReduce_7

action_56 _ = happyReduce_8

action_57 (16) = happyShift action_15
action_57 (21) = happyShift action_16
action_57 (22) = happyShift action_17
action_57 (23) = happyShift action_18
action_57 (24) = happyShift action_19
action_57 (25) = happyShift action_20
action_57 (27) = happyShift action_21
action_57 (12) = happyGoto action_62
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (36) = happyShift action_61
action_58 (11) = happyGoto action_60
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_29

action_60 _ = happyReduce_11

action_61 (31) = happyShift action_63
action_61 _ = happyReduce_13

action_62 (18) = happyShift action_23
action_62 (19) = happyShift action_24
action_62 (20) = happyShift action_25
action_62 _ = happyReduce_10

action_63 (36) = happyShift action_61
action_63 (11) = happyGoto action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_12

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
happyReduction_6 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenQuotedName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAxiom happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 7 8 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenQuotedName happy_var_2)) `HappyStk`
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
happyReduction_10 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenQuotedName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSAssume happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 10 happyReduction_11
happyReduction_11 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PSThusBy happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TokenQuotedName happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyTerminal (TokenQuotedName happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn12
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  12 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  12 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (FNot happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 12 happyReduction_19
happyReduction_19 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 12 happyReduction_20
happyReduction_20 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn12
		 (FTrue
	)

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn12
		 (FFalse
	)

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  13 happyReduction_24
happyReduction_24 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn13
		 (TVar happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  13 happyReduction_25
happyReduction_25 (HappyAbsSyn14  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn13
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  14 happyReduction_26
happyReduction_26  =  HappyAbsSyn14
		 ([]
	)

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 40 40 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenParenOpen -> cont 16;
	TokenParenClose -> cont 17;
	TokenAnd -> cont 18;
	TokenOr -> cont 19;
	TokenImp -> cont 20;
	TokenNot -> cont 21;
	TokenTrue -> cont 22;
	TokenFalse -> cont 23;
	TokenForall -> cont 24;
	TokenExists -> cont 25;
	TokenDot -> cont 26;
	TokenId happy_dollar_dollar -> cont 27;
	TokenVar happy_dollar_dollar -> cont 28;
	TokenSemicolon -> cont 29;
	TokenDoubleColon -> cont 30;
	TokenComma -> cont 31;
	TokenAxiom -> cont 32;
	TokenTheorem -> cont 33;
	TokenProof -> cont 34;
	TokenQED -> cont 35;
	TokenQuotedName happy_dollar_dollar -> cont 36;
	TokenAssume -> cont 37;
	TokenThus -> cont 38;
	TokenBy -> cont 39;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 40 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. parseError
parseExp tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: ([Token], [String]) -> a
parseError (r, n) = error (
        "Parse error on " ++ show r ++ "\npossible tokens: " ++ show n)
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
