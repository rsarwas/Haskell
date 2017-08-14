-- Project Euler in Haskell problems 581..600

import ProjectEuler

-- 587
-- Convex Triangles:
{-
Find Number of adjacent circles required to create a convex triangle (below the
left circle and the LL to UR diagonal of the bounding box) where the ratio is
less than 0.1%
Answer: 2240, time 0.0 (some work done manually)
Analysis:
I started by noting that the area of the lower triangle (including the circular
segment) is Rh/2, where R is the base and the radius of the circle.  Since the
bisector has a slope of 1/n, the height of the triangle is hn, so the area (w/
circ. seg.) is Rhn/2, and the ratio is Rh/2 / (Rhn/2 + Rh/2) or 1/(n+1). Therefore
with n = 1000, the ratio is 1/1001 or ~ 0.1%. However removing the circular segment
whose area is R^2/2(theta - sin theta) w/ theta in radians, requires finding the
angle of the point on the circle hit by the bisector, and finding h, since we
can't cancel it out anymore ratio = Area small tri. - small circ. segment over
area of whole convex triangle = R^2 - pi*R^2/4

I started over by using a unit circle at 1,1 and calculating the intersection
of the bisector line which goes through the origin with slope 1/n. and the circle.
thetaß
-}
-- intersection of the unit circle at 1,1 and the bisector
x θ = 1 - sin θ
y θ = 1 - cos θ
-- number of circles required to put intersection at θ, the angle from the center
-- of the circle in radians CW with 0 in the negative y direction.
-- since the slope from 0,0 to x,y is n/1 =>  n/1 = x/y
numberOfCirclesFor θ = x θ / y θ
-- Ratio created for a given angle to the intersection
areaWholeConvexTriangle = 1 - (pi/4)
areaLowerTriangle θ = (1 - cos θ) / 2
areaLowerCircularSegment θ = (θ - sin θ) / 2
areaLowerConvexTriangle = areaLowerTriangle θ - areaLowerCircularSegment θ
ratioFrom θ = areaLowerConvexTriangle θ / areaWholeConvexTriangle
-- At this point, I can manually converge the ratio to 0.001 (.1%) by choosing
-- various theta, and then find the number of circles for theta
-- ratio 0.0294445 => 0.0009999996698369213
-- ratio 0.0294446 => 0.0010000064281286412
-- n 0.0294445 => 2239.11
-- n 0.0294446 => 2239.09
-- Therefore 2239 is insufficent to drop below 0.1%, but 2240 will be < 0.1%
-- I should try writing a converger that takes a function, an initial guess,
-- and an epsilon.  I can use that to converge θ to a ratio of 0.001
