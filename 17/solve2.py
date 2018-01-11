
reps = 50000000
steps = 369

value = 0
length = 1
position = 0

for r in xrange(reps):
    position = 1 + ((position + steps) % length)
    if position == 1:
        value = length
    length += 1

print value
