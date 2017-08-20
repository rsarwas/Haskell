#! python3
# $ pip3 install primesieve
# Answer: 17427258 in 0m1.190s unix time (time python3 pe187.py)
import primesieve
n = 10**6
sqrt_n = 10**3
primes = primesieve.primes(n/2)
end = len(primes) - 1
start = 0
result = 0
for p in primes:
    if sqrt_n < p:
        break
    while n < primes[end] * p:
        end -= 1
    result += end - start + 1
    start += 1
print(result)
