// Project Euler Problem #88
//Answer: 7587457
//Compiled with swiftc pe88.swift
//time 0m0.046s unix real time

import Darwin  //for sqrt()

// n is the number of terms that may be non-one. This must match the depth of
// the for loops, i.e the number of terms that are being added together
// 2^14 = 16384 => k=16370, so we will never have 14 terms.
let n = 13
// q is an alias for k in the project definition.  qmax is the largest k I
// need to investigate
let qmax = 12_000

func makeProductSumsFor2Terms(upto size:Int) -> [Int] {
  // in no case do I need to have a product GT 2k or 24000.
  // initialize the array to all zeros, so q=0, q=1 do not influence the sum
  var psmin = Array(repeating:0, count:(size+1))
  for k in 2...size {
    let nMax = Int(sqrt(Double(k)))
    for nn in 0...nMax {
      let n = nMax - nn
      if ((k+n) % (n+1) == 0) {
        // Guaranteed to be executed at n == 0 because x%1 == 0
        // at n == 0 returns the simple solution psmin[k] = 2*k
        let x = n+2
        let y = (k+n) / (n+1)  //Integer division yields an int
        psmin[k] = x*y
        break
      }
    }
  }
  return psmin
}

// psmin is the smallest product sum found for each q where 2 <= q <= qmax
var psmin = makeProductSumsFor2Terms(upto:qmax)

/*
for i in 0...100 {
  print(psmin[i])
}
*/

/*
The form of the terms is determined by the fact that
product = 1*1*..*1*a1*...*an = a1*..*an
sum = 1+1+...+1+a1+..+an = sum(ai) + (k-n)*1
when the product == sum (i.e sum(ai) + k - n  = product(ai),
k = product(ai) - sum(ai) + n
I manually checked k and product for 2^n, 3^n, etc, to find limits to the
number of terms that can have a value > 2, >3, etc.
*/

for a in 1...2 {
  //print("a = \(a)")
  for b in a...2 {
    //print("b = \(b)")
    let pb = a * b
    let sb = a + b
    for c in b...2 {
      //print("c = \(c)")
      let pc = pb * c
      let sc = sb + c
      for d in c...2 {
        //print("d = \(d)")
        let pd = pc * d
        let sd = sc + d
        for e in d...2 {
          //print("e = \(e)")
          let pe = pd * e
          let se = sd + e
          for f in e...3 {
            //print("f = \(f)")
            let pf = pe * f
            let sf = se + f
            for g in f...3 {
              //print("g = \(g)")
              let pg = pf * g
              let sg = sf + g
              for h in g...4 {
                //print("h = \(h)")
                let ph = pg * h
                let sh = sg + h
                for i in h...6 {
                  //print("i = \(i)")
                  let pi = ph * i
                  let si = sh + i
                  for j in i...10 {
                    //print("j = \(j)")
                    let pj = pi * j
                    let sj = si + j
                    let kini = max(2,j)
                    for k in kini...qmax {
                      //print("k = \(k)")
                      let pk = pj * k
                      let sk = sj + k
                      let pkmin = pk * k * k
                      let skmin = sk + k + k
                      let qkmin = pkmin - skmin + n
                      if (qmax < qkmin) { break }
                      for l in k...qmax {
                        let pl = pk * l
                        let sl = sk + l
                        let plmin = pl * l
                        let slmin = sl + l
                        let qlmin = plmin - slmin + n
                        if (qmax < qlmin) { break }
                        for m in l...qmax {
                          let pm = pl * m
                          let sm = sl + m
                          let q = pm - sm + n
                          if (qmax < q) { break }
                          if (pm < psmin[q]) {
                            psmin[q] = pm
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

/*
for i in 0...100 {
  print(psmin[i])
}
*/

//sort and then sum the unique elements
var total = psmin[0] //known to be zero, so it won't influence the sum
var previous = psmin[0] //known to be zero, so it won't influence the sum
for ps in psmin.sorted() {
  if (ps == previous) { continue }
  previous = ps
  total += ps
}
print ("Sum = \(total)")
