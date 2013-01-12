main_list = [map(int, line.split()) for line in open('pinball.txt')]

### Reusable function 'foobar' which takes two rows them finds the maximum results by 'tilting'
### Note that it _returns_ the updated current (required by 'reduce')

def foobar(current, row):
    copy = [0] + current + [0]
    for x in range(len(main_list)):
        current[x] = row[x] + max([copy[x + i] for i in range(0, 3)])
    return current

### Use a normal loop and mutable state

current = list(main_list[0])
for row in [nextRow for nextRow in main_list[1:]]:
    current = foobar(current, row)

print(max(current))

### Use a reduce function! (similar to foldr in Haskell)
### Note the lack of 'mutable' state (ie current) - we get the value we want _from_ reduce

def foldr1(f, l):
    return reduce(f, l[1:], list(l[0]))

print(max(foldr1(foobar, main_list)))

"""
NOTES

Instead of having to check for IndexErrors, just modify each 'row' with the previous appending an extra value '0' on the start and end.

1 2 3
4 5 6

Make that:

0 1 2 3 0
4 5 6

And use an offset of [0, 1, 2] instead of [-1, 0, 1]

------

Just on my use of map here:

map(int, line.split())

This is equivalent to:

map(lambda x: int(x), line.split())

Same as reduce(foobar, ...) where foobar take 2 arguments instead of one

reduce(lambda x,y: foobar(x, y), ...)

"""

### This is the same as the 'tilt' function from Haskell

def tilt(e, f, g):
    # This is called currying and it's a bit crazy when you first see it
    # I want to 'partially apply' the first three arguments (e, f, g) and return a new function that takes the remaining two
    def inner(current, row):
        copy = [e] + current + [e]
        for x in range(len(main_list)):
            current[x] = f([row[x], g([copy[x + i] for i in range(0, 3)])])
        return current
    return inner

# tilt(0, sum, max) is effectively 'foobar' from above - we can even assigned the 'curried' result to a variable 
# Basically I'm separating out the logic of combining the two rows from _what_ we want to do with it

foobar2000 = tilt(0, sum, max)
value = foldr1(foobar2000, main_list)
print(max(value))

# Why would we want to do that you might ask?

# Here is the shortest path using the same function with different arguments

print(min(foldr1(tilt(100000, sum, min), main_list)))
