-- Project Euler in Haskell problems 601..620

import ProjectEuler

-- 607
-- Marsh Crossing
{-
Find fastest route across a progressively more difficult marsh
Answer:
Analysis:
assume one marsh section followed by the easy section.  The shortest distance
is 10*root(2) + (100 - 50*root(2))/2.  It is trivial to calc the time for this
The fastest time through the marsh is traveling perpendicular to the marsh.
This distance is 10.  Now the distance in the easy section is the opposite leg
of a Side-Angle-Side triangle, where the first side is 10, angle is 135, and the
other side is (100 - 50*root(2))/2.  It is easy to see that this takes longer
than the straight line route. If we call x the distance along the length
of the marsh from the straight line route that is the target when traveling
across the marsh, we can see that it varies from 0 to 10. and that the length of
the leg across the marsh is also given by a SAS (10*root(2), 45deg, x).  If
we assume that they act on the total independently, we can do this for each class
of marsh separately
-}

angleToEasy = 135*pi/180
easyDistance = 50 - 25*sqrt 2
angleFromHard = 45*pi/180
hardDistance = 10*sqrt 2
-- law of cosine is used to calculate the opposite side length of a SAS triangle
lawOfCosines b c θ = sqrt (b*b + c*c -2*b*c*cos θ)
easyLength offset = lawOfCosines offset easyDistance angleToEasy
hardLength offset = lawOfCosines offset hardDistance angleFromHard
easyTime offset = easyLength offset / 10
marshTime marshSpeed offset = hardLength offset / marshSpeed
comboTime marshSpeed offset = easyTime offset + marshTime marshSpeed offset

-- for testing, we can try to find an minimum in a range of offsets
minComboTime marshSpeed offsets =
  minimum [(comboTime marshSpeed offset, offset) | offset <- offsets]
--minComboTime 9 [0,0.1..10] --> 1.1
--minComboTime 9 [1.0,1.001..1.2] --> 1.058
--minComboTime 9 [1.057,1.05701..1.059] --> 1.05849
-- We see similar results for other marsh segments
-- 9 -> ~1.1, 8 -> ~2.2, 7 -> ~ 3.3, 6 -> ~4.4, 5 -> ~5.5
-- so we will write a general converger function

--converger finds a minimum or maximum (fOpt) of the value of f(x) in the
-- range x = min'..max',  It recursively reduces the range until the gap between
-- to consecutive values of f(x) < epsilon (eps)
converger fOpt f min' max' eps
  | abs (f (newGuess - delta) - f (newGuess+ delta)) < eps = newGuess
  | otherwise = converger fOpt f newMin newMax eps
  where
     newGuess = snd $ fOpt [(f x, x) | x <-[min',min'+delta..max']]
     delta = (max' - min') / 100
     newMin = newGuess - delta
     newMax = newGuess + delta

-- How should we split the offset for the easy legs?
--   evenly, all on one side, or ??
bestEasySplitCoarse = minimum [(easyTime (16-delta) + easyTime delta, delta) | delta <- [1,2..16]]
bestEasySplitFine = minimum [(easyTime (16-delta) + easyTime delta, delta) | delta <- [7.9999,7.999901..8.0001]]
-- best easy split is 50% on each side

-- Find the optimal offset for each marshSpeed
offset marshSpeed = converger minimum (comboTime marshSpeed) 0 10 1e-14
offsetTotal = sum [offset marshSpeed | marshSpeed <- [5..9]]
totalEasyTime = 2 * easyTime (offsetTotal/2)
totalMarshTime = sum [marshTime marshSpeed (offset marshSpeed) | marshSpeed <- [5..9]]
totalTime = totalEasyTime + totalMarshTime
--totalTime = 13.1526444174 20557  (0.04 secs, 9,352,608 bytes) which is WRONG

-- I'm guessing that the combination of the offsets in marshes creates a total
-- offset in the easy part that is so great, it would pay to walk a more direct
-- route across the marsh, but it is unclear how to optimize the 5 sections
-- simultaneously 
