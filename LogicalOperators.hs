module LogicalOperators where
import Prelude hiding ((&&), (||))
-- Implies operator
(-->) :: Bool -> Bool -> Maybe Bool
(-->) False _ = Just True
(-->) _ False = Just False
(-->) _ _ =Just True

-- And
myAnd :: Bool -> Bool -> Maybe Bool
myAnd True True = Just True
myAnd _ _ = Just False

--Maybe Or
--
myOr :: Bool -> Bool -> Maybe Bool
myOr False False = Just False
myOr _ _ = Just True


-- Double implies operator
(<-->) :: Bool -> Bool -> Maybe Bool
(<-->) p q = do
    res1 <- (p --> q)
    res2 <- (q --> p)
    res1 `myAnd` res2


-- xor implementation
xor :: Bool -> Bool ->Maybe Bool
xor False False = Just False
xor True True = Just False
xor _ _ = Just True 

(^) = xor

-- nor gate implementation
nor :: Bool -> Bool -> Maybe Bool
nor p q = let a = (p `myOr` q) in
              case a of
                  Just r -> Just (not r)
                  Nothing -> Nothing

-- nand gate implementation
nand :: Bool -> Bool ->Maybe Bool
nand p q = let a = (p `myAnd` q) in
               case a of
                   Just r -> Just (not r)
                   Nothing -> Nothing

-- xnor implementation
xnor :: Bool -> Bool -> Maybe Bool       
xnor p q = let a = (p `xor` q) in
               case a of 
                   Just r -> Just  (not r)
                   Nothing -> Nothing


-- word equivalents of --> and <-->
implies = (-->)
doubleImplies = (<-->)


