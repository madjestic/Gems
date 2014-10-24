{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

class Experiment a b where
      experiment :: a -> b -> IO ()
instance Experiment String Int where
         experiment _ _ = print "String -> Int"
instance Experiment String Char where
         experiment _ _ = print "String -> Char"

class Experiment1 a where
      experiment1 :: a -> IO ()
instance Experiment1 String where
         experiment1 = print
instance Experiment1 Int where
         experiment1 = print

main = do putStrLn "multiparam typeclasses"
          undefined
