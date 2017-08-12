// Project Euler Problem 387
// Harshad Numbers
// Find the sum of the strong, right truncatable Harshad primes less than 10**14

func isPrime(_ n:Int) -> Bool
{
  // TODO: replace with real prime check
  return n % 2 == 1
}

func sumOfDigits(_ n:Int) -> Int
{
  if (n < 10) {
    return n
  }
  return n % 10 + sumOfDigits(n/10)
}

func sum<T: Sequence>(_ numbers: T) -> Int where T.Iterator.Element == Int
{
    return numbers.reduce(0, +)
}

func isHarshad(_ n:Int) -> Bool {
  // A Harshad number is a number that is divisible by the sum of its digits.
  return n % sumOfDigits(n) == 0
}

func harshadGenerator(_ xs:[Int]) -> [Int]
{

  // Generates the list of larger Harshad numbers that are right truncatable to
  // the given Harshad numbers. This is done by appending 0..9 to each number in the list
  // and returning those that are Harshad numbers.
  let hs = xs.flatMap {x in return (0...9).map {x*10+$0}}
  return hs.filter(isHarshad)
}

func rightTruncatableHarshads(_ digits:Int) -> [Int]
{
  // yields all right truncatable Harshad numbers below 10**(n+1)
  // applies the harshad function n times
  // seed with 1...9 to generate the harshad numbers between 10 an 99
  var result = harshadGenerator(Array(1...9))
  var counter = 1
  while counter < digits {
    counter += 1
    result = harshadGenerator(result)
  }
  return result
}

func strongHarshads(_ xs:[Int]) -> [Int]
{
  // strong Harshad numbers.
  // A Harshad number is strong if the number divided by the sum of the digits is prime.
  return xs.filter() {
    isPrime ($0 / sumOfDigits($0))
  }
}

func primeHarshads(_ xs:[Int]) -> [Int]
{
  // prime Harshad numbers.
  // Given a list of numbers, appends a digit, and returns the larger number if it is prime.
  // i.e. returns the prime numbers which when the last digit is truncated the truncated number is in the provided list
  let hs = xs.flatMap {h in return (0...9).map {h*10+$0}}
  return hs.filter(isPrime)
}

func sumStrongHarshad(_ n:Int) -> Int
{
    //Returns the sum of the Harshad numbers with n+1 digits
    let hList = rightTruncatableHarshads(n)
    let shList = strongHarshads(hList)
    let pshList = primeHarshads(shList)
    return sum(pshList)
}

func pe387(_ n:Int) -> Int
{
  // none of the single digit Harshads numbers are strong, because 1 is not prime,
  // therefore we start with 1 and not 0
  let list = (1...n-2).map(sumStrongHarshad)
  return sum(list)
}

print("pe387 = \(pe387(8))")
