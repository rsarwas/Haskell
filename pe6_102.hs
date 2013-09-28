-- pe102
-- Triangle Containment: find the number of triangles (from the 1000 in the file provided) for which the interior contains the origin.
-- Analysis: The cross product of two vectors determines if the rotation is positive ro negative.
--           use the cross product to determine if the origin is on the left or right of each leg
--           if the origin is on the same side of each leg then it is within the triangle.
--           We first filter out the trivial triangles - those are entirely on one side or the other of the origin

-- Answer: 228  (time: real 0m0.046s)


import ProjectEuler ( wordsWhen )


-- make a list of lists (list of lines, where a line is a list of words, and each word is an int).
makeTris :: String -> [[Int]]
makeTris s = [map read (wordsWhen (==',') x)::[Int] | x <- lines s]

crossesAxis [ax,ay,bx,by,cx,cy] = 
   not (threeSimilar ax bx cx || (threeSimilar ay by cy))

contains00 [ax,ay,bx,by,cx,cy] =
  let cross1 = cross (ax,ay) (bx,by) (0,0)
      cross2 = cross (bx,by) (cx,cy) (0,0)
      cross3 = cross (cx,cy) (ax,ay) (0,0)
  in threeSimilar cross1 cross2 cross3

threeSimilar a b c = (a<0 && b<0 && c<0) || (a>0 && b>0 && c>0)

cross (ox,oy) (ax,ay) (bx,by) =
  let x1 = ax - ox
      x2 = bx - ox
      y1 = ay - oy
      y2 = by - oy
   in x1*y2 - y1*x2

main = do
  tris <- getContents
  print $ length (filter contains00 (filter crossesAxis (makeTris tris)))
