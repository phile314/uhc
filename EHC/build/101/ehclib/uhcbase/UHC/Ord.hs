{-# LANGUAGE NoImplicitPrelude, CPP #-}

module UHC.Ord
where

import UHC.Base
import UHC.Eq

#include "TupleInstance.h"


instance Ord () where
    compare () () = EQ


TUPLE2_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)
TUPLE3_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)
TUPLE4_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)
TUPLE5_BINOP1_INSTANCE(Ord,`compare`,`compareTupleElt`,,$,$ EQ)

compareTupleElt :: Ord x => x -> x -> Ordering -> Ordering
compareTupleElt x1 x2 = case x1 `compare` x2 of
                          EQ -> id
                          o  -> const o
