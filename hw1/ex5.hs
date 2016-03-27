-- Ex 5 (peep at internet)
type Peg    = String
type Source = Peg
type Dest   = Peg
type Spare  = Peg
type Move   = (Peg, Peg)
hanoi :: Integer -> Source -> Dest -> Spare -> [Move]
hanoi 0 source dest _     = []
hanoi n source dest spare = hanoi (n-1) source spare dest ++ [(source, dest)] ++ hanoi (n-1) spare dest source
