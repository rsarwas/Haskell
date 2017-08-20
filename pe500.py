#! python3
# $ pip3 install primesieve
# Answer 35407281 in 0m0.877s unix time (time python3 pe500.py)
import primesieve
n = 500500
k = 500500507
primes = primesieve.n_primes(n)  # we do not need all these, but i don't know how many
prime_index = 1
square_index = 0
result = 2  # primes[0] mod k
squares = [4]  # primes[0]**2
for i in range(1,n):  # 0,n would be n loops, we skip the first, since it is done
  p = primes[prime_index]
  s = squares[square_index]
  r = min(p,s)
  if p < s:
    prime_index += 1
  else:
    square_index += 1
  result = (result * r) % k
  squares.append(r**2)
print(result)
