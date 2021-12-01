"""
Project Euler # 757
Problem: Find all stealthy numbers (SN)  SN = a*b = c*d, such that a+b = c+d+1
Solution:
If we let a = 1x, b = 2y, c = 2x, d = 1y
then a+b = c+d+1 => 1x+2y = 2x+1y+1 => y=x+1 for x={1...}
and 2x(x+1) for x={1...} are stealthy numbers (SN = a*b = c*d = 2xy)
The following are the first few stealthy numbers:
x=1, y=2 => SN=4 (2*1*2); (a=1, b=4, c=2, d=2 a+b=5, c+d=4, 4+1=5)
x=2, y=3 => SN=12 (2*2*3); (a=2, b=6, c=4, d=3 a+b=8, c+d=7, 7+1=8)
x=3, y=4 => SN=24 (2*3*4); (a=3, b=8, c=6, d=4 a+b=11, c+d=10, 10+1=11)
x=4, y=5 => SN=40 (2*4*5); (a=4, b=10, c=8, d=5 a+b=14, c+d=13, 13+1=14)
:

Similarly, if we let a=2x, b=3y, c=3x, d=2y
then a+b = c+d+1 => 2x+3y = 3x+2y+1 => y=x+1 for x={1...}
and 6x(x+1) for x={1...} are stealthy numbers (SN = a*b = c*d = 6xy)
The following are stealthy numbers in this pattern:
x=1, y=2 => SN=12 (6*1*2); (a=2, b=6, c=3, d=4 a+b=8, c+d=7, 7+1=8)
x=2, y=3 => SN=36 (6*2*3); (a=4, b=9, c=6, d=6 a+b=13, c+d=12, 12+1=13)
x=3, y=4 => SN=72 (6*3*4); (a=6, b=12, c=9, d=8 a+b=18, c+d=17, 17+1=18)

*Note* that the first SN (12) in the second set is also the second SN in the
first set, but that the second SN (36) in set 2 is not in the first set.
Aside from the first n-1 in set n being non unique, I could find no other
pattern for when a stealthy number would be non-unique.

This pattern for finding stealthy numbers can be generalized to

a = (i)x, b = (i+1)y, c = (i+1)x, d = (i)y  for i={1...}
then SNi = a*b = c*d = (i)(i+1)xy where x = n={1...} and y = n+1

or more generally: SN = { (i)(i+1)(n)(n+1) } for n={1...} for i={1...}

In psuedo code:
Note: Trying to find bounds on i and n below missed some numbers
decided to break when SN exceeded MAX
i_limit: # at smallest n (=i) SN = i(i+1)(i)(i+1) < MAX => i < sqrt(sqrt(MAX))
n_limit: # Nn(Nn+1)i(i+1) < MAX  =>  Nn < sqrt(MAX/i/(i+1))
for i = 1...  
    SN = (i)(i+1)(n)(n+1) # start at n=i to eliminate well known duplicates
    if SN > MAX break
    for n = (i+1)...
        SN = (i)(i+1)(n)(n+1)
        if SN > MAX break
        // somehow Uniquify the set of SN found (i.e. place in a Hashset)
"""

def details(max):
    total = 0
    for nx in range(1, max):
        q = nx * (nx + 1)
        print(f"Loop {nx} {q}nx:")
        if q * q > max:
            break
        count = 0
        for n in range(nx, max):
            stealth = q * n * (n+1)
            if stealth > max:
                count += n - nx
                break
            print(f"    {n} = {stealth}")
        print(f"    Found {count} Stealth Numbers")
        total += count
    print(f"Total = {total} (some duplicates)")

def simple(max):
    found = set()
    for nx in range(1, max):
        q = nx * (nx + 1)
        #print(f"Loop {nx} {q}nx:")
        if q * q > max:
            break
        for n in range(nx, max):
            stealth = q * n * (n+1)
            if stealth > max:
                break
            if not stealth in found:
                found.add(stealth)
                # print(f"adding {stealth}")
            else:
                pass # print(f"skipping {stealth}")
        # print(f"Found {len(found)} Stealth Numbers")
    print(f"Total = {len(found)} (unique)")

def new_details(max):
    found = set() # will guarantee uniqueness of the 'add'ed elements
    total = 0
    for i in range(1, max):
        print(f"Loop {i}; N = {i * (i+1)}x(x+1):")
        sn = i*i*(i+1)*(i+1)
        if sn > max:
            break
        found.add(sn)
        print(f"   {i} = {sn}")
        count = 1
        for n in range(i+1, max):
            sn = i * (i+1) * n * (n+1)
            if sn > max:
                break
            print(f"   {n} = {sn}")
            count += 1
            found.add(sn)
        print(f"    Found {count} Stealth Numbers")
        total += count
    print(f"Total = {total} (some duplicates)")
    # print(f"Total = {len(found)} (unique)")

def new_simple(max):
    found = set() # will guarantee uniqueness of the 'add'ed elements
    for i in range(1, max):
        q = i * (i+1)
        sn = q * q
        if sn > max:
            break
        found.add(sn)
        for n in range(i+1, max):
            sn = q * n * (n+1)
            if sn > max:
                break
            found.add(sn)
    print(f"Total = {len(found)} (unique)")

if __name__ == '__main__':
    # e = 6 # test = 2851 in .1 sec
    e = 14 # Answer = 75737353 in 69 seconds
    max = 10**e
    print(f"Count of stealthy numbers less than 10^{e}")
    # print("\nShow all stealthy numbers\n")
    # details(max)
    #simple(max)
    # print("\nNew simple solution\n")
    # new_details(max)
    new_simple(max)
