{-# OPTIONS_GHC -w #-}
module Parser(parseExp) where

import Prover ( Form(..), Term(..) )
import Theory ( TProof, ProofStep(..), Theorem(..), Program(..) )
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
	| HappyAbsSyn5 (Theorem)
	| HappyAbsSyn6 (TProof)
	| HappyAbsSyn7 (ProofStep)
	| HappyAbsSyn8 (Form)
	| HappyAbsSyn9 (Term)
	| HappyAbsSyn10 ([Term])

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
 action_54 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_23 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,97) ([2048,2207,0,16384,0,0,0,0,128,0,0,28672,0,8192,636,0,5089,0,0,0,0,0,0,64,0,512,32768,0,0,0,0,0,6,0,4,0,32,0,0,0,60,0,15888,1,61568,9,33792,79,0,4096,0,5089,0,224,0,0,0,0,0,0,0,32768,2544,0,20356,0,0,1,512,0,2048,0,0,0,0,0,0,0,0,0,24,28672,0,32768,3,0,28,2,0,384,0,0,0,2048,0,1024,0,0,4,20356,0,896,2048,0,128,0,32768,1,0,0,0,0,15888,1,0,1024,0,0,32768,3,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseExp","Prog","Theorem","Proof","ProofStep","Form","Term","TermArgs","Terms","'('","')'","and","or","imp","not","true","false","forall","exists","dot","comma","id","var","';'","':'","theorem","proof","qed","name","assume","thus","by","%eof"]
        bit_start = st Prelude.* 35
        bit_end = (st Prelude.+ 1) Prelude.* 35
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..34]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (12) = happyShift action_6
action_0 (17) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (19) = happyShift action_9
action_0 (20) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (24) = happyShift action_12
action_0 (28) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 (8) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (28) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (31) = happyShift action_22
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (35) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (14) = happyShift action_19
action_5 (15) = happyShift action_20
action_5 (16) = happyShift action_21
action_5 _ = happyReduce_2

action_6 (12) = happyShift action_6
action_6 (17) = happyShift action_7
action_6 (18) = happyShift action_8
action_6 (19) = happyShift action_9
action_6 (20) = happyShift action_10
action_6 (21) = happyShift action_11
action_6 (24) = happyShift action_12
action_6 (8) = happyGoto action_18
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (12) = happyShift action_6
action_7 (17) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (19) = happyShift action_9
action_7 (20) = happyShift action_10
action_7 (21) = happyShift action_11
action_7 (24) = happyShift action_12
action_7 (8) = happyGoto action_17
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_15

action_9 _ = happyReduce_16

action_10 (25) = happyShift action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (25) = happyShift action_15
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (12) = happyShift action_14
action_12 (10) = happyGoto action_13
action_12 _ = happyReduce_20

action_13 _ = happyReduce_8

action_14 (24) = happyShift action_32
action_14 (25) = happyShift action_33
action_14 (9) = happyGoto action_30
action_14 (11) = happyGoto action_31
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (22) = happyShift action_29
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (22) = happyShift action_28
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_12

action_18 (13) = happyShift action_27
action_18 (14) = happyShift action_19
action_18 (15) = happyShift action_20
action_18 (16) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (12) = happyShift action_6
action_19 (17) = happyShift action_7
action_19 (18) = happyShift action_8
action_19 (19) = happyShift action_9
action_19 (20) = happyShift action_10
action_19 (21) = happyShift action_11
action_19 (24) = happyShift action_12
action_19 (8) = happyGoto action_26
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (12) = happyShift action_6
action_20 (17) = happyShift action_7
action_20 (18) = happyShift action_8
action_20 (19) = happyShift action_9
action_20 (20) = happyShift action_10
action_20 (21) = happyShift action_11
action_20 (24) = happyShift action_12
action_20 (8) = happyGoto action_25
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (12) = happyShift action_6
action_21 (17) = happyShift action_7
action_21 (18) = happyShift action_8
action_21 (19) = happyShift action_9
action_21 (20) = happyShift action_10
action_21 (21) = happyShift action_11
action_21 (24) = happyShift action_12
action_21 (8) = happyGoto action_24
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (27) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (12) = happyShift action_6
action_23 (17) = happyShift action_7
action_23 (18) = happyShift action_8
action_23 (19) = happyShift action_9
action_23 (20) = happyShift action_10
action_23 (21) = happyShift action_11
action_23 (24) = happyShift action_12
action_23 (8) = happyGoto action_39
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (14) = happyShift action_19
action_24 (15) = happyShift action_20
action_24 (16) = happyShift action_21
action_24 _ = happyReduce_11

action_25 _ = happyReduce_10

action_26 _ = happyReduce_9

action_27 _ = happyReduce_17

action_28 (12) = happyShift action_6
action_28 (17) = happyShift action_7
action_28 (18) = happyShift action_8
action_28 (19) = happyShift action_9
action_28 (20) = happyShift action_10
action_28 (21) = happyShift action_11
action_28 (24) = happyShift action_12
action_28 (8) = happyGoto action_38
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (12) = happyShift action_6
action_29 (17) = happyShift action_7
action_29 (18) = happyShift action_8
action_29 (19) = happyShift action_9
action_29 (20) = happyShift action_10
action_29 (21) = happyShift action_11
action_29 (24) = happyShift action_12
action_29 (8) = happyGoto action_37
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (23) = happyShift action_36
action_30 _ = happyReduce_22

action_31 (13) = happyShift action_35
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (12) = happyShift action_14
action_32 (10) = happyGoto action_34
action_32 _ = happyReduce_20

action_33 _ = happyReduce_18

action_34 _ = happyReduce_19

action_35 _ = happyReduce_21

action_36 (24) = happyShift action_32
action_36 (25) = happyShift action_33
action_36 (9) = happyGoto action_30
action_36 (11) = happyGoto action_41
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (14) = happyShift action_19
action_37 (15) = happyShift action_20
action_37 (16) = happyShift action_21
action_37 _ = happyReduce_13

action_38 (14) = happyShift action_19
action_38 (15) = happyShift action_20
action_38 (16) = happyShift action_21
action_38 _ = happyReduce_14

action_39 (14) = happyShift action_19
action_39 (15) = happyShift action_20
action_39 (16) = happyShift action_21
action_39 (29) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (32) = happyShift action_44
action_40 (33) = happyShift action_45
action_40 (6) = happyGoto action_42
action_40 (7) = happyGoto action_43
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_23

action_42 (30) = happyShift action_49
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (26) = happyShift action_48
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (31) = happyShift action_47
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (12) = happyShift action_6
action_45 (17) = happyShift action_7
action_45 (18) = happyShift action_8
action_45 (19) = happyShift action_9
action_45 (20) = happyShift action_10
action_45 (21) = happyShift action_11
action_45 (24) = happyShift action_12
action_45 (8) = happyGoto action_46
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (14) = happyShift action_19
action_46 (15) = happyShift action_20
action_46 (16) = happyShift action_21
action_46 (34) = happyShift action_52
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (27) = happyShift action_51
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (32) = happyShift action_44
action_48 (33) = happyShift action_45
action_48 (6) = happyGoto action_50
action_48 (7) = happyGoto action_43
action_48 _ = happyReduce_5

action_49 _ = happyReduce_3

action_50 _ = happyReduce_4

action_51 (12) = happyShift action_6
action_51 (17) = happyShift action_7
action_51 (18) = happyShift action_8
action_51 (19) = happyShift action_9
action_51 (20) = happyShift action_10
action_51 (21) = happyShift action_11
action_51 (24) = happyShift action_12
action_51 (8) = happyGoto action_54
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (31) = happyShift action_53
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_7

action_54 (14) = happyShift action_19
action_54 (15) = happyShift action_20
action_54 (16) = happyShift action_21
action_54 _ = happyReduce_6

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (ProgramT happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (ProgramF happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happyReduce 7 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenQuotedName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Theorem happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([ happy_var_1 ]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happyReduce 4 7 happyReduction_6
happyReduction_6 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenQuotedName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PSAssume happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 ((HappyTerminal (TokenQuotedName happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (PSThus happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn8
		 (FPred happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (FAnd happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (FOr happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (FImp happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (FNot happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (FExists happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (FForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  8 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn8
		 (FTrue
	)

happyReduce_16 = happySpecReduce_1  8 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn8
		 (FFalse
	)

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 (TVar happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn9
		 (TFun happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  10 happyReduction_20
happyReduction_20  =  HappyAbsSyn10
		 ([]
	)

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  11 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenParenOpen -> cont 12;
	TokenParenClose -> cont 13;
	TokenAnd -> cont 14;
	TokenOr -> cont 15;
	TokenImp -> cont 16;
	TokenNot -> cont 17;
	TokenTrue -> cont 18;
	TokenFalse -> cont 19;
	TokenForall -> cont 20;
	TokenExists -> cont 21;
	TokenDot -> cont 22;
	TokenComma -> cont 23;
	TokenId happy_dollar_dollar -> cont 24;
	TokenVar happy_dollar_dollar -> cont 25;
	TokenSemicolon -> cont 26;
	TokenDoubleColon -> cont 27;
	TokenTheorem -> cont 28;
	TokenProof -> cont 29;
	TokenQED -> cont 30;
	TokenQuotedName happy_dollar_dollar -> cont 31;
	TokenAssume -> cont 32;
	TokenThus -> cont 33;
	TokenBy -> cont 34;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35 tk tks = happyError' (tks, explist)
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
