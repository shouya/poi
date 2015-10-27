module Sh where

import Turtle

type ShResult = IO (String, String, ExitCode)

(.&&.) :: ShResult -> ShResult -> ShResult
r1 .&&. r2 = do
  (o1, e1, c1) <- r1
  case c1 of
   ExitSuccessful
