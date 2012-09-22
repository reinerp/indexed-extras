{-

Samples from "Polymorphic Delimited Continuations" (Kenichi Asai and Yukiyoshi Kameyama)

-}
{-# LANGUAGE PackageImports #-}
module Asai where

import "indexed-extras" Control.Monad.Indexed.Cont 
import "indexed" Control.Monad.Indexed
import Control.Monad.Identity

visit :: IxMonadCont m => [a] -> m [b] b [a]
visit []      = shift (\h -> ireturn [])
visit (a : r) = shift (\k -> k [] >>>= \ph -> reset (visit r >>>= k ) 
                              >>>= \pt -> ireturn (ph : pt)
                        ) >>>= ireturn . (a:)

prefix :: IxMonadCont m => [a] -> m j j [[a]] 
prefix = reset . visit

sh :: (Show b, IxMonadCont m) => m (b -> m i i a) a String
sh = shift (\k -> ireturn (\x -> k (show x)))

str :: IxMonadCont m => m (String -> m i i a) a String
str = shift (\k -> ireturn (\x -> k x))

lit :: IxMonadCont m => String -> m i i String
lit = ireturn

printf :: IxMonadCont m => m a String String -> m i i a
printf = reset

iapj :: IxMonad m => m i j (a -> m j k b) -> a -> m i k b
iapj l r = l >>>= \f -> f r

(+++) :: IxMonad m => m i j [a] -> m j k [a] -> m i k [a]
l +++ r =  ireturn (++) `iap` l `iap` r

printfTest = runIdentity $ runIxContT_ $
                  printf (lit "Hello, " +++ str +++ lit ", at " +++ sh) 
                                     `iapj` "World" `iapj` 10
