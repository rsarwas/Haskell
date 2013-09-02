-- Project Euler in Haskell problems 41..60

import ProjectEuler

-- 48
-- Self Powers: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-- Answer: 9110846700 (0.04 secs, 5809448 bytes)
pe48 = (sum [x^x | x <- [1..999]]) `mod` 10^10
