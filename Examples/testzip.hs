{

assume zip:: [a] -> [b] -> c;

fst (x, y) = x;

head (x:xs) = x;

fst (a, b, c) = a;

zip = zip2;

zip2 (x:xs) (y:ys) = (x,y):zip2 xs ys;

zip as  bs cs = zip (zip2 as bs) cs;

zip3 as bs cs = zip as (zip bs cs);


g = zip [1,2,3] [True, False];  

h = head g;

i = fst h; 

}