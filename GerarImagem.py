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

dataFrame = pd.read_csv("3classes-1-2-500-500GER-n_features=2.csv", index_col=False)


#teste = [1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 2, 0, 0, 2, 2, 1, 1, 2, 1, 0, 0, 2, 2, 2, 2, 0, 2, 1, 2, 2, 2, 2, 2, 1, 2, 1, 2, 2, 1, 1, 2, 2, 2, 1, 0, 0, 1, 2, 2, 1, 2, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 2, 2, 1, 2, 2, 0, 2, 2, 0, 1, 1, 1, 1, 2, 2, 1, 1, 2, 1, 1, 2, 1, 2, 2, 2, 0]

    #my_evaluate(teste[0])
    #dataFrame['label'] = teste
   # robjects.globalenv['dataFrame'] = dataFrame
    #array1 = np.array(teste)
   # data = pd.DataFrame(teste)

    # Criando o arquivo rotulado
    # Usar esse arquivo no classificador
    ##dataFrame.to_csv("3classes-1-2-100-500GER-n_features=2.csv")
    ##dataFrame.head(N_ATTRIBUTES)
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
plt.savefig('007.png')
plt.show()
