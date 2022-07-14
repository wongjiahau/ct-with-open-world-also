{

data Nat = Zero | Succ Nat; 

--soma :: Nat -> Nat -> Nat;
soma m Zero = m;
soma m (Succ n) = Succ (soma m n)

}
