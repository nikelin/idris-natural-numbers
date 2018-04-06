module Functions

public export
data Natural =
     NaturalZero
     | Successor Natural
     | NaN

export next: Natural -> Natural
next prev = Successor prev

export prev: Natural -> Natural
prev (Successor k) = k
prev NaturalZero = NaN
prev NaN = NaN

export intToNat: Int -> Natural
intToNat 0 = NaturalZero
intToNat x = intToNat0(x)
    where
        intToNat0: Int -> Natural
        intToNat0 0 = NaturalZero
        intToNat0 x = (Successor (intToNat0 (x - 1)))

export total natToInt: Natural -> Int
natToInt NaturalZero = 0
natToInt NaN = -1
natToInt (Successor k) = 1 + (natToInt k)

export Show Natural where
    show natVal = (show (natToInt natVal))

export Eq Natural where
    (==) NaN _ = False
    (==) _ NaN = False
    (==) (Successor k) (Successor j) = k == j
    (==) NaturalZero NaturalZero = True
    (==) NaturalZero (Successor k) = False
    (==) (Successor k) NaturalZero = False

export Ord Natural where
    compare NaN _ = LT
    compare _ NaN = GT
    compare NaturalZero NaturalZero = EQ
    compare NaturalZero (Successor _) = LT
    compare (Successor _) NaturalZero = GT
    compare (Successor k) (Successor j) = compare k j

export nat_one: Natural
nat_one = (next NaturalZero)

export nat_two: Natural
nat_two = (next nat_one)

export nat_three: Natural
nat_three = (next nat_two)

export nat_four: Natural
nat_four = (next nat_three)

export nat_five: Natural
nat_five = (next nat_four)

export nat_six: Natural
nat_six = (next nat_five)

export nat_seven: Natural
nat_seven = (next nat_six)

export plus: Natural -> Natural -> Natural
plus NaN _ = NaN
plus _ NaN = NaN
plus NaturalZero right = right
plus left NaturalZero = left
plus left right = plus (prev left) (next right)

export minus: Natural -> Natural -> Natural
minus NaN _ = NaN
minus _ NaN = NaN
minus NaturalZero right = right
minus left NaturalZero = NaN
minus left right = minus (prev left) (prev right)

export mult: Natural -> Natural -> Natural
mult NaN _ = NaN
mult _ NaN = NaN
mult left right = mult0 left right right
    where
        mult0: Natural -> Natural -> Natural -> Natural
        mult0 (Successor NaturalZero) right result = result
        mult0 left (Successor NaturalZero) result = result
        mult0 NaturalZero _ _ = NaturalZero
        mult0 _ NaturalZero _ = NaturalZero
        mult0 left right result = mult0 (prev left) right (plus right result)

export total
getOrElse : (Maybe a) -> a -> a  

getOrElse x y = case x of
        Just x => x
        Nothing => y