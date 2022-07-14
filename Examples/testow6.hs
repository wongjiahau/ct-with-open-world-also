{


assume read:: [Char] -> a;

assume show:: a -> [Char];


read "1" = 1;

show 1 = "1";
 

read "a" = 'a';

show 'a' = "a"; 


f = show (read "teste");


}