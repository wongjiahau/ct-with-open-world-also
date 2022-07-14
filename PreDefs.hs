module PreDefs where

import Assump
import Id
import Pat
import Pred
import Type
import SimpleType
import Lit



defs = [  nilC,
          consC, tup2C, tup3C, tup4C, tup5C, tup7C,
          toid "." :>: (CW, Forall ([] :=> ((TGen 0 `fn` TGen 1) `fn` (TGen 2 `fn` TGen 0) `fn` TGen 2 `fn` TGen 1))),
          toid "()" :>: (CW, Forall ([] :=> tUnit)),
          toid "primPlusInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` tInt))),
          toid "primPlusInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInteger `fn` tInteger))),
          toid "primPlusFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat `fn` tFloat))),
          toid "primPlusDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble `fn` tDouble))),
          toid "primMinusInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` tInt))),
          toid "primMinusInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInt `fn` tInteger))),
          toid "primMinusFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat `fn` tFloat))),
          toid "primMinusDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble `fn` tDouble))),
          toid "primMulInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` tInt))),
          toid "primMulInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInteger `fn` tInteger))),
          toid "primMulFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat `fn` tFloat))),
          toid "primMulDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble `fn` tDouble))),
          toid "primDivInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` tInt))),
          toid "primDivInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInteger `fn` tInteger))),
          toid "primDivFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat `fn` tFloat))),
          toid "primDivDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble `fn` tDouble))),
          toid "primEqChar" :>: (CW, Forall ([] :=> (tChar `fn` tChar `fn` tBool))),
          toid "primEqInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` tBool))),
          toid "primEqInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInteger `fn` tBool))),
	  toid "primEqFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat `fn` tBool))),
          toid "primEqDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble `fn` tBool))),
          toid "primEqBool" :>: (CW, Forall ([] :=> (tBool `fn` tBool `fn` tBool))),
          toid "primCmpChar" :>: (CW, Forall ([] :=> (tChar `fn` tChar `fn` tOrdering))),
          toid "primCmpInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` tOrdering))),
          toid "primCmpInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInteger `fn` tOrdering))),
          toid "primCmpFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat `fn` tOrdering))),
          toid "primExpFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primLogFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primSinFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primCosFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primTanFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primAsinFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primAcosFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primAtanFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primSqrtFloat" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "primCmpDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble `fn` tOrdering))),
          toid "primExpDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primLogDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primSinDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primCosDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primTanDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primAsinDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primAcosDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primAtanDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primSqrtDouble" :>: (CW, Forall ([] :=> (tDouble `fn` tDouble))),
          toid "primFloatRadix" :>: (CW, Forall ([] :=> (tInteger))),
          toid "primFloatDigits" :>: (CW, Forall ([] :=> (tInt))),
          toid "primFloatMaxExp" :>: (CW, Forall ([] :=> (tInt))),
          toid "primFloatMinExp" :>: (CW, Forall ([] :=> (tInt))),
          toid "primRationalToFloat" :>: (CW, Forall ([] :=> ((TAp tRatio tInteger) `fn` (tFloat)))),
          toid "primRationalToDouble" :>: (CW, Forall ([] :=> ((TAp tRatio tInteger) `fn` (tDouble)))),
          toid "primFloatEncode" :>: (CW, Forall ([] :=> (tInteger `fn` tInt`fn` tFloat))),
          toid "primFloatDecode" :>: (CW, Forall ([] :=> (tFloat `fn` (TAp (TAp tTuple2  tInteger) tInt) ))),
          toid "primQrmInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` (TAp (TAp tTuple2  tInt) tInt) ))),
          toid "primQrmInteger" :>: (CW, Forall ([] :=> (tInteger `fn` tInteger `fn` (TAp (TAp tTuple2  tInteger) tInteger) ))),         
          toid "$!" :>: (CW, Forall ([] :=> (((TGen 0) `fn` (TGen 1)) `fn` (TGen 0) `fn` (TGen 1)))),
          toid "coerce" :>: (CW, Forall ([] :=> (tInt `fn` tInt))),          
          toid "coerce" :>: (CW, Forall ([] :=> (tInt `fn` tFloat))),          
          toid "coerce" :>: (CW, Forall ([] :=> (tInt `fn` tInteger))),
          toid "coerce" :>: (CW, Forall ([] :=> (tInt `fn` tDouble))),
          toid "coerce" :>: (CW, Forall ([] :=> (tFloat `fn` tDouble))),
          toid "coerce" :>: (CW, Forall ([] :=> (tFloat `fn` tFloat))),
          toid "unsafeCoerce" :>: (CW, Forall ([] :=> ((TGen 0) `fn` (TGen 1)))),
          toid "throw" :>: (CW, Forall ([] :=> (tException `fn` (TGen 0)))),
          toid "primIntToFloat" :>: (CW, Forall ([] :=> (tInt `fn` tFloat))),                    
          toid "primIntToChar" :>: (CW, Forall ([] :=> (tInt `fn` tChar))),
          toid "primIntToInteger" :>: (CW, Forall ([] :=> (tInt `fn` tInteger))),
          toid "primIntegerToFloat" :>: (CW, Forall ([] :=> (tInteger `fn` tFloat))),
          toid "primIntegerToInt" :>: (CW, Forall ([] :=> (tInteger `fn` tInt))),
          toid "primCharToInt" :>: (CW, Forall ([] :=> (tChar `fn` tInt))),
          toid "readChar" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` tChar))),
          --toid "readInt" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` tInt))),
          --toid "readInteger" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` tInteger))),
          --toid "readFloat" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` tFloat))),
          toid "readDouble" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` tDouble))),
          toid "primShowsInteger" :>: (CW, Forall ([] :=> (tInt `fn` tInteger `fn` (TAp tList tChar) `fn` (TAp tList tChar)))),
          toid "primShowsInt" :>: (CW, Forall ([] :=> (tInt `fn` tInt `fn` (TAp tList tChar) `fn` (TAp tList tChar)))),
          toid "primShowsFloat":>: (CW, Forall ([] :=> (tInt `fn` tFloat `fn` (TAp tList tChar) `fn` (TAp tList tChar)))),
          toid "primShowsDouble" :>: (CW, Forall ([] :=> (tInt `fn` tDouble `fn` (TAp tList tChar) `fn` (TAp tList tChar)))),
          toid "getChar" :>: (CW, Forall ([] :=> (TAp tIO tChar))),
          toid "putChar" :>: (CW, Forall ([] :=> (tChar `fn`TAp tIO tUnit))), 
          toid "putStr" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn`TAp tIO tUnit ))),
	  toid "primMinInt" :>: (CW, Forall ([] :=> tInt)),
          toid "primMaxInt" :>: (CW, Forall ([] :=> tInt)),
          toid "primbindIO" :>: (CW, Forall ([] :=> (TAp tIO (TGen 0) `fn` ((TGen 0) `fn` (TAp tIO (TGen 1))) `fn` (TAp tIO (TGen 1))))),
          toid "primretIO" :>: (CW, Forall ([] :=> ((TGen 0) `fn` TAp tIO (TGen 0)))),
          toid "IO" :>: (CW, Forall ([] :=> ((((TGen 0) `fn` tIOResult) `fn` tIOResult) `fn` TAp tIO (TGen 0)))),
          toid "readFile" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` (TAp tIO (TAp tList tChar))))),
          toid "appendFile" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` (TAp tList tChar) `fn` (TAp tIO tUnit)))),
          toid "writeFile" :>: (CW, Forall ([] :=> ((TAp tList tChar) `fn` (TAp tList tChar) `fn` (TAp tIO tUnit)))),
          toid "getContents" :>: (CW, Forall ([] :=> TAp tIO (TAp tList tChar))),
          toid "||" :>: (CW, Forall ([] :=> (tBool `fn` tBool `fn` tBool))),
          toid "False" :>: (CW, (Forall ([] :=> tBool))),
          toid "True"  :>: (CW, (Forall ([] :=> tBool))),
          toid "uncurry" :>: (CW, Forall ([]:=> ((TGen 0 `fn` TGen 1 `fn` TGen 2) `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1) `fn` TGen 2))),
          toid "_concatMap" :>: (CW, (Forall ([] :=> ((TGen 0 `fn` TAp tList (TGen 1)) `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 1)))))]
          

---------------------------------------------------------------------------------------------
-- (StdPred) Mover para outro arquivo
----------------------------------------------------------------------------------------------
tup2C  		= toid "(,)" :>: (CW, (Forall ([] :=> (TGen 0 `fn` TGen 1 `fn` TAp (TAp tTuple2 (TGen 0)) (TGen 1)))))

tup3C 		= toid "(,,)" :>: (CW, (Forall ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TAp (TAp (TAp tTuple3 (TGen 0)) (TGen 1)) (TGen 2)))))

tup4C 		= toid "(,,,)" :>: (CW, (Forall ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TAp (TAp (TAp (TAp tTuple4 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)))))

tup5C 		= toid "(,,,,)" :>: (CW, (Forall ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TAp (TAp (TAp (TAp (TAp tTuple5 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)))))

tup6C 		= toid "(,,,,,)" :>: (CW, (Forall ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TAp (TAp (TAp (TAp (TAp (TAp tTuple6 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)))))

tup7C 		= toid "(,,,,,,)" :>: (CW, (Forall ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 2 `fn` TGen 3 `fn` TGen 4 `fn` TGen 5 `fn` TGen 6 `fn` TAp (TAp (TAp (TAp (TAp (TAp (TAp tTuple7 (TGen 0)) (TGen 1)) (TGen 2)) (TGen 3)) (TGen 4)) (TGen 5)) (TGen 6)))))

falseC	 	= toid "False" :>: (CW, (Forall ([] :=> tBool)))
trueC	  	= toid "True"  :>: (CW, (Forall ([] :=> tBool)))

------------------------------------------------------------------------------------------------

pNil            = PCon nilC []
pCons x y       = PCon consC [x,y]

nilC		= toid "[]" :>: (CW, Forall ([] :=> (TAp tList (TGen 0))))
consC		= toid ":"  :>: (CW, Forall ([] :=> (TGen 0 `fn` TAp tList (TGen 0) `fn` TAp tList (TGen 0))))

returnM 	= toid "return" :>: (OW, Forall ([]  :=> (TGen 1 `fn` TAp (TGen 0) (TGen 1))))
bindM 	 	= toid ">>=" :>: (OW, (Forall ([]  :=> (TAp (TGen 0) (TGen 1) `fn` (TGen 1 `fn` TAp (TGen 0) (TGen 2)) `fn` TAp (TGen 0) (TGen 2)))))
thenM 		= toid ">>" :>: (OW, Forall ([] :=> (TAp (TGen 0) (TGen 1) `fn` TAp (TGen 0) (TGen 2) `fn` TAp (TGen 0) (TGen 2))))
failM 		= toid "fail" :>: (OW, Forall ([] :=> (tString `fn` TAp (TGen 0) (TGen 1))))
