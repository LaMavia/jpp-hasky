{-# LANGUAGE PatternSynonyms #-}

module Runtime.RTConsts where
import           Runtime.RTEval (RTVal (RTConstr))


pattern RTCFalse :: RTVal
pattern RTCFalse <- RTConstr "Bool" "False" []

pattern RTCTrue :: RTVal
pattern RTCTrue <- RTConstr "Bool" "True" []

rtcFalse :: RTVal
rtcFalse = RTConstr "Bool" "False" []

rtcTrue :: RTVal
rtcTrue = RTConstr "Bool" "True" []

rtOfBool :: Bool -> RTVal
rtOfBool True  = rtcTrue
rtOfBool False = rtcFalse

