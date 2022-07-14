{
data Reg a = A {r1::Int, r2 :: Bool} | B { r3:: String, r4::a} | C {r5, r6::(a,a), r7::Bool, r8::(Int, a)} ;

data Reg' = R {r, r'::Char}; 

f x = r4 x;

g s b = r4 (B s b); 
}


