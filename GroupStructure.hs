
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
data Circle = Param Double
-- a `Circle` can be taken to the
-- unit circle by (Circle h) -> ((1-h^2)/(1+h^2), 2*h/(1+h^2))
--
-- This parameterization can be derived from
-- observing the line that goes from
-- (-1,0) -> (x,y) -> (0,h) where (x,y) on the circle.

instance Group Circle where
    zero = Param 0
    add (Param h1) (Param h2) = Param $ (h1+h2)/(1-h1*h2)
    inv (Param h) = Param (-h)

{- Proving the laws for the group
 - is left as an excercise for the reader...
 -
 - This group interpertation is dervied
 - from the idea of parallel lines between
 - two points. The idea is that it is
 - isomorphic to the e^(i*theta) interpertation
 - (but it works with only rational numbers,
 - if you like that)
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

{- All the groups in this document are _commutative_ groups! -}
