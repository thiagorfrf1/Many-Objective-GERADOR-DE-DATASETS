import sys
import numpy as np
import pandas as pd
import scipy.stats as stats
import random
import math
import os
import matplotlib.pyplot as plt
import seaborn as sns
import array

from mpl_toolkits.mplot3d import Axes3D

from sklearn.neighbors import kneighbors_graph
from scipy.sparse.csgraph import minimum_spanning_tree

from deap import base
from deap import creator
from deap import tools
from deap import algorithms

from sklearn import preprocessing
from sklearn.model_selection import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report,confusion_matrix

import urllib.request
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import SignatureTranslatedAnonymousPackage as STAP
from rpy2.robjects.packages import importr
from rpy2.robjects import IntVector, Formula
pandas2ri.activate()

filename = "ALEATORIO-MUITODIFICIL100-5000GER"

dataFrame = pd.read_csv(str(filename)+".csv", index_col=False)


print(dataFrame)
df = dataFrame
print(df)
colors = {0: 'red', 1: 'blue', 2:"green"}
markers = {0: '.', 1: '.', 2: '.'}
fig, ax = plt.subplots()
grouped = df.groupby('label')
plt.rcParams['figure.figsize'] = (11, 7)
for key, group in grouped:
    group.plot(ax=ax, kind='scatter', x='x', y='y', label=key, color=colors[key])
plt.savefig(str(filename)+'.png')
plt.show()
