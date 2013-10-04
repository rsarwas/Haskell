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