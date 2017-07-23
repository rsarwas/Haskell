/*
Project Euler Problem 14: Longest Collatz sequence
*/

func collatzChainLengthRecursive(_ n:Int) -> Int
{
    if (n == 1) {
        return 1
    } else {
        if (n % 2 == 0) {
            return 1 + collatzChainLengthRecursive(n/2)
        } else {
            return 1 + collatzChainLengthRecursive(3*n+1)
        }
    }
}

func collatzChainLength(_ N:Int) -> Int
{
    var result = 1
    var n = N
    while (1 < n) {
        result += 1
        if (n % 2 == 0) {
            n =  n / 2
        } else {
            n = 3*n+1
        }
    }
    return result
}

func longestCollatzChain(lessThan N:Int) -> (Int, Int)
{
    var maxLength = 1
    var maxIndex = 1
    for i in (2 ..< N) {
        let currentLength = collatzChainLength(i)
        if maxLength < currentLength {
            maxLength = currentLength
            maxIndex = i
        }
    }
    return (maxIndex, maxLength)
}

let N = 1_000_000
let (i,length) = longestCollatzChain(lessThan:N)
print("For starting numbers under \(N), \(i) produces the longest Collatz Chain (\(length) elements)")

/*
For starting numbers under 1000000, 837799 produces the longest Collatz Chain (525 elements)
with swiftc pe14.swift
real	0m3.370s; user	0m3.357s; sys	0m0.007s
real	0m3.879s; user	0m3.865s; sys	0m0.007s (recursive solution 15% slower)
with swiftc -O pe14.swift
real	0m0.445s; user	0m0.434s; sys	0m0.005s
real	0m0.731s; user	0m0.718s; sys	0m0.008s (recursive solution 64% slower)
*/
