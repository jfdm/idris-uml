-- --------------------------------------------------------------- [ Model.idr ]
-- Module      : UML.Sequence.Model
-- Description : A model for sequence diagrams.
-- Copyright   : (c) Jan de Muijnck-Hughes
-- License     : see LICENSE
-- --------------------------------------------------------------------- [ EOH ]
module UML.Sequence.Model

%access public

data ElemTy = MODEL | STEP | SEND | LOOP | SWITCH | CHOICE

data OkElem : ElemTy -> Type where
  OkSend : OkElem SEND
  OkLoop : OkElem LOOP
  OkSwitch  : OkElem SWITCH

data SequenceModel : ElemTy -> Type where
  ||| Construct a step.
  |||
  ||| @from The sending entity.
  ||| @to The receiving entity.
  ||| @ms The messages being sent.
  Send : (from : String)
       -> (to : String)
       -> (ms : List String) -> SequenceModel SEND
  Choice : (condition : String) -> (List (SequenceModel SEND)) -> SequenceModel CHOICE
  Loop : (condition : String) -> (List (SequenceModel SEND)) -> SequenceModel LOOP
  Opt  : SequenceModel CHOICE -> SequenceModel CHOICE -> SequenceModel SWITCH
  Alt  : List (SequenceModel CHOICE) -> SequenceModel SWITCH
  Nil  : SequenceModel STEP
  (::) : (SequenceModel a)
       -> {auto prf : OkElem a}
       -> SequenceModel STEP
       -> SequenceModel STEP
  MkSeqModel : SequenceModel STEP -> SequenceModel MODEL

mkStep : List (SequenceModel a) -> {auto prf : OkElem a} -> SequenceModel STEP
mkStep Nil       = Model.Nil
mkStep (x :: xs) = Model.(::) x (mkStep xs)
-- ---------------------------------------------------------------------- [ Eq ]

instance Eq ElemTy where
  (==) MODEL  MODEL  = True
  (==) STEP   STEP   = True
  (==) SEND   SEND   = True
  (==) LOOP   LOOP   = True
  (==) SWITCH SWITCH = True
  (==) CHOICE CHOICE = True
  (==) _      _      = False

mutual

  %assert_total
  eqSeqModel : (SequenceModel a) -> (SequenceModel b) -> Bool
  eqSeqModel (Send af at as) (Send bf bt bs) = af == bf && at == bt && as == bs
  eqSeqModel (Choice a as)   (Choice b bs)   = a == b && as == bs
  eqSeqModel (Loop a as)     (Loop b bs)     = a == b && as == bs
  eqSeqModel (Opt ax ay)     (Opt bx by)     = ax == bx && ay == by
  eqSeqModel (Alt as)        (Alt bs)        = as == bs
  eqSeqModel Nil             Nil             = True
  eqSeqModel (x::xs) (y::ys)                 = eqSeqModel x y && eqSeqModel xs ys
  eqSeqModel (MkSeqModel a)  (MkSeqModel b)  = a == b
  eqSeqModel _               _               = False

  instance Eq (SequenceModel x) where
    (==) = eqSeqModel

-- -------------------------------------------------------------------- [ Show ]

instance Show ElemTy where
  show MODEL  = "MODEL"
  show STEP   = "STEP"
  show SEND   = "SEND"
  show LOOP   = "LOOP"
  show SWITCH = "SWITCH"
  show CHOICE = "CHOICE"

instance Show (SequenceModel x) where
  show (Send a b ms) = unwords ["[", a, "->", b, ":", show ms, "]\n"]
  show (Choice d ss) = unwords ["[Choice ", show d, "[", show ss, "]", "]\n"]
  show (Loop d ss)   = unwords ["[Loop ", show d, "[", show ss, "]", "]\n"]
  show (Opt x y)     = unwords ["[Opt ", show x, show y, "]\n"]
  show (Alt as)      = unwords ["[Alt ", show as, " ]\n"]
  show Nil           = ""
  show (x::xs)       = unwords ["[Steps ", showXS (x::xs), "]\n"]
    where
      showXS : SequenceModel STEP -> String
      showXS (x::xs) = show x ++ "," ++ showXS xs
  show (MkSeqModel m)  = unwords ["[Seq ", show m, "]\n"]

-- --------------------------------------------------------------------- [ EOF ]
