module Math.MoreMath.Fraction where

infixr 5 :%
data Fraction a = (:%)
    { numerator   :: a
    , denominator :: a
    } deriving (Eq, Ord, Show)

(%) :: Num a => a -> a -> Fraction a
(%) = (:%)

instance Functor Fraction where
    fmap f (n :% d) = f n :% f d

instance (Num a, Eq a) => Num (Fraction a) where
    abs           = fmap abs
    negate        = fmap negate
    signum        = fmap signum
    fromInteger x = fromInteger x :% 1

    f1@(n1 :% d1) + f2@(n2 :% d2)
        | d1 == d2  = n1 + n2 :% d1
        | otherwise =
            let (f1_, f2_) = toSameDenominator f1 f2
            in f1_ + f2_

    f1@(n1 :% d1) - f2@(n2 :% d2)
        | d1 == d2  = n1 - n2 :% d1
        | otherwise =
            let (f1_, f2_) = toSameDenominator f1 f2
            in f1_ - f2_

    (n1 :% d1) * (n2 :% d2) = n1 * n2 :% d1 * d2

toSameDenominator :: Num a => Fraction a -> Fraction a -> (Fraction a, Fraction a)
toSameDenominator (n1 :% d1) (n2 :% d2) = (n1 * d2 :% d1 * d2, n2 * d1 :% d2 * d1)
