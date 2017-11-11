import random, math

results = [math.floor(random.random() * 20) for z in range(0, 2000000)]

with open('data.txt', 'w') as f:
    f.write(' '.join(map(str, results)))

