
class Group a where
    zero :: a
    add  :: a -> a -> a
    inv  :: a -> a
{- Laws:
 -  add zero x = add x zero = x
 -  add x (add y z) = add (add x y) z
 -  add x (inv x) = add (inv x) x = zero
 -}

-- | The group on a circle
data Circle = Polar Double
-- a `Circle` can be taken to the
-- unit circle by (Circle theta) -> (realPart exp(i*theta), imagPart exp(i*theta))

instance Group Circle where
    zero = Polar 0
    add (Polar x) (Polar y) = Polar (x+y)
    inv (Polar x) = Polar (-x)

{- Adherence to the laws follows from
 - the group structure of the real numbers
 - with respect to + and 0.
 -}

-- | The group on the parabola y = x^2
data Parabola = Square Double
-- a `Parabola` can be taken to the
-- parabola y = x^2 by (Square x) -> (x, x^2)

instance Group Parabola where
    zero = Square 0
    add (Square x) (Square y) = Square (x+y)
    inv (Square x) = Square (-x)

{- Adherence to the laws follows from
 - the group structure of the real numbers
 - with respect to + and 0.
 -}

-- | The group on the hyperbola y = 1 / x
data Hyperbola = Inverse Double
-- a `Hyperbola` can be taken to
-- the hyperbola y = 1/x by
-- (Inverse x) = (x, 1/x) where (x `elem` R\{0})

instance Group Hyperbola where
    zero = Inverse 1
    add (Inverse x) (Inverse y) = Inverse (x*y)
    inv (Inverse x) = Inverse (1/x)

{- Adherence to the laws follows from
 - the group structure of the real numbers
 - with respect to * and 1.
 -}
