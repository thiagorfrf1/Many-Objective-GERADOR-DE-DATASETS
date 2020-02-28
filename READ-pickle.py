# -*- coding: UTF-8 -*-

import pickle


N_ATTRIBUTES = 101
cont = 0
bobj = 0.4

NOBJ = 4
P = [12]
SCALES = [1]

NGEN = 5000
CXPB = 0.2
MUTPB = 0.2
INDPB = 0.5
POP = 50
filename = "NGEN=" + str(NGEN) + "-POP=" + str(POP) + "-CXPB=" + str(CXPB) + "-MUTPB=" + str(MUTPB) + "-INDPB=" + str(INDPB)
globalBalance = 0.25
globalLinear = 0.25
globalN1 = 0.25
globalN2 = 0.25

infile = open(filename, 'rb')
new_dict = pickle.load(infile)
print("NEW DICT")
print(new_dict)
infile.close()
