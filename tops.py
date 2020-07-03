import pickle
from topsis import topsis
from deap import base
from deap import creator
NOBJ = 4
filename = "Ferramenta"
a = []
z = []
result = []

meta = 0.07


creator.create("FitnessMin", base.Fitness, weights=(-1.0,)*NOBJ)
creator.create("Individual", list, fitness=creator.FitnessMin)
RANDINT_LOW = 0
RANDINT_UP = 1




infile = open(filename, 'rb')
new_dict = pickle.load(infile)

for i in new_dict:
    print(i)
    for j in i:
        j = (j - meta) -1
        z.append(j)
    a.append(list(z))
    z.clear()
print("A")
print(a)
j = [[0.028391167192429068, 0.10266795259226853, 0.23750433761309298, 0.4431915224169199], [0.09233117130866075, 0.10346781846399243, 0.15897655288375967, 0.4755277437728605], [0.12663919070813046, 0.09838405101909063, 0.762495662386907, 0.5361783035789236], [0.07692307692307687, 0.10370684975474338, 0.23750433761309298, 0.45565965394056696]]

print(type(a[0]))

w = [0.1, 0.2, 0.4, 0.3]

I = [1, 0, 0, 0]

decision = topsis(a, w, I)

result = decision.calc()
print("type(result)")
print(result)
for i in list(decision):
    print("Decision")
    print(i)
print(decision)