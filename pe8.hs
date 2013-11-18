-- Project Euler in Haskell problems 141..160

-- 144
-- Investigating multiple reflections of a laser beam.
-- How many times does the beam hit the internal surface of the white cell before exiting?
-- The specific white cell we will be considering is an ellipse with the equation 4x2 + y2 = 100
-- The section corresponding to -0.01 <= x <= 0.01 at the top is missing, allowing the light to enter and exit through the hole.
-- The slope m of the tangent line at any point (x,y) of the given ellipse is: m = 4x/y
-- Answer: 354 (0.01 secs, 4707384 bytes)

-- a vector is a tuple of points (pt1, pt2), where each point is a coordinate pair (x,y)
v1 = ((0.0,10.1),(1.4,(-9.6)))
nextVector ((p1x,p1y),(p2x,p2y)) =
  let angleOfTangent = atan2 ((-4)*p2x) p2y
      angleOfInVector = atan2 (p2y - p1y) (p2x - p1x)
      angleOfOutVector = 2*angleOfTangent - angleOfInVector
      -- y = mx + b' is the equation of the line of the reflected vector
      m = tan angleOfOutVector
      b' = p2y - m * p2x
      -- find intersection of reflected vector and ellipse by squaring y = mx +b' and sub into ellipse equation
      -- to get a quadratic in x: ax^2 + bx + c == 0
      a = 4 + m^2
      b = 2*b'*m
      c = b'^2 -100
      -- solve for x using quadratic equation
      xp = (-b + sqrt (b^2 -4*a*c)) / (2*a)
      xm = (-b - sqrt (b^2 -4*a*c)) / (2*a)
      -- xp or xm will equal p2x, take the other one.
      x = if (abs (p2x - xp) < 0.0001) then xm else xp
      -- use equation of reflected vector to get the y value for x.
      y = m*x + b'
      in ((p2x,p2y),(x,y))

pe144 = length $ takeWhile trapped (iterate nextVector v1)
  where trapped ((p1x,p1y),(p2x,p2y)) =
                not (p2y > 0 && (-0.01) <= p2x && p2x <= 0.01)
                
-- 145
-- How many reversible numbers are there below one-billion?
-- Answer: 608720 (0.00 secs, 2064392 bytes)
-- xy is reversible if x+y < 10, x+y is odd; if limit y>x then only getting half
reversibleNoCarry = sum [2 | x <- [1..4], y <- [(x+1),(x+3)..(9-x)]]
reversibleNoCarryWithZero = sum [2 | x <- [0..4], y <- [(x+1),(x+3)..(9-x)]]
reversibleWithCarry = sum [2 | x <- [6..9], z <- [(x-1),(x-3)..(11-x)]]
evenNoCarry = 5 + sum [2 | a <-[0..9], b <-[(a+1)..(9-a)], even (a+b)]
-- abc is reversible if b = 0..4, and  a+c > 9 and a+c is odd.
-- b+b will always be even for any b, so middle term must be b+b+1, which requires c+a to be greater than 9.
-- if b+b were greater than 9, then most significant digit(s) would be a+c+1, which will be even, since c+a
-- must be odd (for the least significant digit). therefore, b+b must be less than 10.
-- given abcd is reversible. then  d+a must be odd, therefore, a+d is odd, if b+c > 10, then there is a 1 carried
-- to a+d, which would make the first term even.  therefore b+c must be less than 10 and odd.  if b+c is odd then
-- c+b is odd. the second term would then be even if d+a > 10.  therefore d+a < 10.  The logic extends to any even
-- number of terms.  the inner terms can have a zeros.  with 4 terms, the total number of reversible numbers
-- is the number of reversible pairs on the inside times the number of reversible pairs on the inside.
-- consider abcde, for this to be reversible, c+c will be even for any c, so d+b must be greater than 9 to have a carry
-- bit, since d+b is greater than 9, b+d will also have a carry bit, which means that a+e will need to be even, so it
-- will be odd when a 1 is added, however, e+a must be odd for the right term to be odd.  Therefore, there are no
-- reversible numbers with 5 digits.
-- what about 7 digits - abcdefg? d+d is even, so e+c > 9 => b+f is even (to make b+f+1 odd) => g+a > 9 and odd
--  similar to 3 digits, d+d must be less than 10.  the outer pair (ag) and inner pair (ec) are reversible with carry,
--  the pair (bf) must be even, and less than 10 (otherwise a+g+1 will be even (since a+g must be odd)
-- numbers with 1,5,9,13... have zero reversible numbers
-- numbers with 3,7,11,15.. have 5 times the product of the pairs   
reversible n
  |  n < 1 = 0
  | even n = product (take (n `div` 2) (reversibleNoCarry:(repeat reversibleNoCarryWithZero)))
  | n `mod` 4 == 1 = 0
  | n `mod` 4 == 3 = 5 * (product (take ((n-1) `div` 2) (concat $ repeat [reversibleWithCarry, evenNoCarry])))
pe145 = sum [ reversible n | n <- [1..9]]