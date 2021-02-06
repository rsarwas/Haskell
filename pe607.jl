pythag(a,b) = sqrt(a^2 + b^2)
r2 = sqrt(2)
base = (100 - 50*r2)/2.0
dl(b,h) = pythag(h,b+h)
dl_offset(h) = lm(base, h) + lm(base, 1-h)
offset = 0:0.025:1
plot(offset,[dl_offset(x) for x in offset])

pythag(a,b) = sqrt(a^2 + b^2)
r2 = sqrt(2)
base = (100 - 50*r2)/2.0
dl(b,h) = pythag(h,b+h)
dlt(h) = 2*dl(base,h/2.0)
tl(h,v) = dlt(h)/v

dm(h, base) = pythag(2*h, base*r2 - 2*h)
tm(h,base,v) = dm(h,base)/v
tt(h1,h2,b1,b2,v1,v2,v3) = tm(h1,b1,v1) + tm(h2,b2,v2) + tl(h1+h2,v3)
surf(h1,h2) = tt(h1,h2,10,40,9,5,10)

h10max = 10.0/(2.0*r2)
h40max = 40.0/(2.0*r2)
dh1 = 0:0.1:h10max
dh2 = 0:0.1:h40max
plot(dh1,dh2,surf,st=:surface,camera=(-30,30))
