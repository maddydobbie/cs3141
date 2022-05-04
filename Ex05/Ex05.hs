module Ex05 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise x = mapM parseToken (words x)

newtype Calc a = C ([Int] -> Maybe ([Int], a))

pop :: Calc Int
pop = C popIt
  where 
    popIt :: [a] -> Maybe ([a], a)
    popIt [] = Nothing
    popIt (x:xs) = Just (xs,x)

push :: Int -> Calc ()
push i = C (\xs -> Just(i:xs, ()))


instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
evaluate ts = evaluateIt ts >> pop
  where
   evaluateIt :: [Token] -> Calc Int
   evaluateIt [] = pure 0
   evaluateIt (Number i:ts) = push i >> evaluateIt ts
   evaluateIt (Operator o:ts) = do
     x <- pop
     y <- pop
     push (o x y) >> evaluateIt ts


readC :: Calc Int -> Maybe ([Int], Int)
readC c = unwrap c []
  where unwrap (C a) = a

help :: Maybe (Maybe ([Int], Int)) -> Maybe Int
help (Just (Just(_, i))) = Just i
help _ = Nothing

calculate :: String -> Maybe Int
calculate s = help $ readC . evaluate <$> tokenise s



