module MaybeArithmeticOperators where
import Prelude hiding (mod)
add :: (Num a) => a -> a -> Maybe a
add a b = Just (a+b)

minus :: (Num a) => a -> a -> Maybe a
minus a b = Just (a - b)

divide :: (Fractional a) => a -> a -> Maybe a
divide a b =  Just (a / b )

multiply :: (Num a ) => a -> a -> Maybe a
multiply a b = Just (a * b)

mod :: (RealFrac a) => a -> a -> Maybe a
mod a b = Just (a - ((fromIntegral $ truncate $ (a / b) ) * b))

power :: (Floating a) => a -> a -> Maybe a
power a b = Just (a ** b)

myLogBase a b = Just (logBase a b)

