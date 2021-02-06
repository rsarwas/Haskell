-- investigation #1
-- assuming a non-direct (i.e. closer to perpendicular or a SE direction) across the marsh,
-- one or both of the non-marsh segments will need to go in the NE direction.  Does it matter
-- how this difference is split.  My intution is that it doesn't matter.  However, if the
-- marsh crossing is too far north or south, the distance will increase the farther north/south
-- you go, indicating there is a curve.
f x a = sqrt (a*a + (x+a)*(x+a))
c = (100 - 50 * sqrt 2) / 2
g = f c
g' x = g x + g (1-x)
a = map g' [0.0, 0.1..1.0]
a2 = minimum [(g' x, x) | x <- [0.0, 0.01..1.0]]
-- a2 = (30.305824853181935,0.5)
-- This shows that it does matter, and the difference should be split evenly to minimize the distance

-- exploration #2
-- how does varying the angle across the marsh effect the total time.
r2 = sqrt 2
--py :: Floating a => a -> a -> a
py a b = sqrt (a^2 + b^2)
h' = [0.0,0.1..(25/r2)]
base = (100 - 50*r2) / 2.0
dl h = py (base + h) h
dml h = py (50*r2 - 2*h) (4*h)
vl = 10
t vm h = (dml h)/vm + (dl h)/vl
b = map (t 5) h'
b2 v = minimum [(t v x, x) | x <- h']


tl x y = 2*(dl x) / y
basem = 10*r2
dm h = py (basem - 2*h) (2*h)
tm x y = (dm x) / y
hmax = basem/4
tt h1 h2 h3 h4 h5 = (tl (h1+h2+h3+h4+h5) 10) + (tm h1 9) + (tm h2 8) + (tm h3 7) + (tm h4 6) + (tm h5 5)

mintt1 n = minimum [(tt h1 h2 h3 h4 h5, h1, h2, h3, h4, h5) | 
    let hmin = -1.2, let hmax = 2.0, let d = (hmax -hmin)/n,
    h1 <- [hmin,hmin+d..hmax],
    h2 <- [h1,h1+d..hmax],
    h3 <- [h2,h2+d..hmax],
    h4 <- [h3,h3+d..hmax],
    h5 <- [h4,h4+d..hmax]]

mintt x1 x2 x3 x4 x5 e = minimum [(tt h1 h2 h3 h4 h5, h1, h2, h3, h4, h5) |
    let d = 5*e,
    h1 <- [x1-d,(x1-d+e)..(x1+d)],
    h2 <- [x2-d,(x2-d+e)..x2+d],
    h3 <- [x3-d,(x3-d+e)..x3+d],
    h4 <- [x4-d,(x4-d+e)..x4+d],
    h5 <- [x5-d,(x5-d+e)..x5+d]]


-- Luckily, This converges to the correct solutions:    
-- mintt (-0.571) 0.312 0.977 1.44 1.94 0.1
-- (13.126672623827275,-0.5710000000000001,0.312,0.9769999999999999,1.4400000000000004,1.9400000000000004)
-- mintt (-0.571) 0.312 0.977 1.44 1.94 0.01
-- (13.126512829149739,-0.5609999999999999,0.31200000000000006,0.957,1.47,1.9)
-- mintt (-0.562) 0.313 0.958 1.471 1.901 0.01
-- (13.126513283477173,-0.562,0.31300000000000006,0.958,1.471,1.901)
-- mintt (-0.562) 0.313 0.958 1.471 1.901 0.001
-- (13.126510864403262,-0.5640000000000001,0.318,0.959,1.468,1.8960000000000001)
-- mintt (-0.564) 0.318 0.959 1.468 1.896 0.0001
-- (13.126510858755143,-0.5635,0.3177,0.9589,1.4679,1.896)
-- mintt (-0.5635) 0.3177 0.9589 1.4679 1.8960 0.00001
-- (13.126510858560119,-0.5635000000000002,0.31768,0.9589499999999995,1.4679000000000002,1.8960400000000004)
-- mintt (-0.56350) 0.31768 0.95895 1.46790 1.89604 0.000001
-- (13.12651085855852,-0.5634969999999998,0.3176809999999999,0.9589449999999999,1.4678999999999995,1.8960429999999993)
--
-- However, this does not, it finds a close by local minimum, but not the global minimum
-- mintt1 19
-- (13.126836266807002,-0.5263157894736838,0.31578947368421073,0.9894736842105264,1.4947368421052631,1.8315789473684212)
-- mintt (-0.5) 0.3 1.0 1.5 1.8 0.1
-- (13.12663159741505,-0.6000000000000001,0.3,0.9999999999999999,1.5000000000000004,1.9000000000000006)
-- mintt (-0.6) 0.3 1.0 1.5 1.9 0.01
-- (13.126512674946635,-0.57,0.32000000000000006,0.96,1.47,1.9)
-- mintt (-0.57) 0.32 0.96 1.47 1.90 0.001
-- (13.126510892735652,-0.565,0.318,0.959,1.4679999999999997,1.896)
-- mintt (-0.565) 0.318 0.959 1.468 1.896 0.0001
-- (13.126510873396589,-0.5645,0.31779999999999997,0.959,1.468,1.8961)
-- mintt (-0.5645) 0.3180 0.9590 1.4680 1.8961 0.00001
-- (13.12651087244803,-0.5644500000000005,0.31795,0.9589999999999997,1.4679499999999999,1.89607)
-- mintt (-0.56445) 0.31795 0.95900 1.46795 1.89607 0.000001
-- (13.12651087227287,-0.5644449999999998,0.31794500000000003,0.9590010000000001,1.467945,1.8960719999999993)
