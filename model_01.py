# -*- coding: utf-8 -*-
"""
Last amended: 10/09/2017
Myfolder: C:\Users\ashok\OneDrive\Documents\python
keywords: pipelines , variance threshold

Objective: 
    Pipelining operations in machine learning
    
"""
#%%                                 Call libraries

%reset -f
# 1. Call libraries
#1.1
import pandas as pd
# 1.2
import numpy as np
# 1.3 Label Encoder transforms categories ['a','b','c'] to [0,1,2]
#     Other preprocessing functions: Imputer, MinMaxScaler, robust_scale
#      (for outlier data), OneHotEncoder, Generating polynomial features 
#       (1, X1, X2, X1 X2, X1^2, X2^2)
from sklearn.preprocessing import StandardScaler, LabelEncoder

# 1.4 sklearn.feature_selection module
#         Removing features with low variance
#         Univariate feature selection
#         Recursive feature elimination
#         Feature Selection from a model
from sklearn.feature_selection import VarianceThreshold

# 1.4 To do PCA. PCA does not work for sparse matricies
from sklearn.decomposition import PCA

# 1.5 sklearn.linear_model
#      LinearRegression
#      10-15 others
from sklearn.linear_model import LogisticRegression

# 1.6 sklearn.model_selection 
#     model_selection.cross_validate
#     GridSearch
#     Random Search
#     Stratified Kfold
from sklearn.model_selection import train_test_split

# 1.7 Create pipeline of data transformation and modeling
#  Pipeline of transforms with a final estimator.
from sklearn.pipeline import Pipeline

# 1.7
import os
import matplotlib.pyplot as plt

#%%                               Read Data and explore
# 2. Read data and have a look
os.chdir("C:\\Users\\ashok\\OneDrive\\Documents\\breast_cancer")
os.listdir()
df = pd.read_csv("data.csv")
df.shape
df.columns
df.head()

# 3. Separate data in predictor and target data
X = df.iloc[ : , 2: 31]      # Predictors
y = df.iloc[ :, 1]           # Target

# 4. How labelEncoder works
#     Encode labels with value between 0 and n_classes-1.
ll = LabelEncoder()
# 4.1 Create a numpy array of characters
n = np.array(["abc", "abc", "cde", "cde", "cde", "abc", "fgh", "fgh", "cde", "cde","fgh"])
n    # Homogenous array with 3-character string
ll.fit_transform(n)

# 4.2 Now transform our target
ll = LabelEncoder()
y = ll.fit_transform(y)
y
ll.transform(['M','B'])


# 5. Split the predictor and the target
#    Note also that we are not instantiating any object
X_train, X_test, y_train, y_test = train_test_split(X,y, test_size = 0.2)
X_train.shape
y_train.shape
X_test.shape
y_test.shape

#%%                               Using PCA Explained
# 6. what would be ideal number of components
# Ref: http://scikit-learn.org/stable/auto_examples/plot_digits_pipe.html#sphx-glr-auto-examples-plot-digits-pipe-py
#      http://scikit-learn.org/stable/modules/generated/sklearn.decomposition.PCA.html
# Number of components to keep. 
#  if n_components is not set all components are kept:
pca = PCA(n_components = None)
pca.fit(X_train)

# Plot the amount of variance explained by each of the selected components.
plt.figure(1, figsize=(4, 3))
plt.clf()  # Clear the current figure.
plt.xlim([-1,10])
plt.ylim([-2000,40000])
plt.xlabel('n_components')
plt.ylabel('explained_variance_')
plt.plot(pca.explained_variance_, linewidth=2)  # # plot y using x as index array 0..N-1
plt.show()

#%%                        Using variance threshold

# 7.  We want to remove all features that are less than the specified variance
# 7.1 Calculate variance of each column
(np.std(X_train, axis = 0))**2
# 7.2 Create VarianceThreshold object
sel = VarianceThreshold(threshold=0.01)
# 7.3 Fit the object on X_train and also transform
d=sel.fit_transform(X_train)
# 7.4 So what is the shape of transformed object
d.shape 

#%%                        Create pipeline

# 8.1
# These two lists will store our results
p1 = []
p2 = []
# 8.2 Enter a for loop and repeat the process 1000 times
for i in np.arange(1, 1000):
    # 8.3 For each loop generate a difft seed
    np.random.seed(int(np.random.uniform(low =1, high =10000)))
    
    # 8.4 Split the dataset each time within the loop
    #     Shuflle the datset and do stratification also 
    #     stratify = y, here 'y' is not yes but name of target col.
    X_train, X_test, y_train, y_test = train_test_split(X,y, 
                                                        test_size = 0.3, 
                                                        shuffle = True, 
                                                        stratify = y 
                                                        )
    
    # 8.5 Create two pipelines
    # 8.6 Create an object to perform Logistic Regression
#   C  Inverse of regularization strength; Like in support vector machines,
#         smaller values specify stronger regularization.
#   class_weight: Are all classes to be given equal weights?
    lr = LogisticRegression(max_iter = 200, C = 0.8, class_weight="balanced")
    
    # 8.7 We create two pipes
    # 8.8 First without variance threshold cutoff
    pipe_lr1 = Pipeline( 
                        [ 
                         ('ss', StandardScaler()), 
                         ('pca', PCA(n_components = 2)), 
                         ('model', lr) 
                         ] 
                         )
    # 8.9 Second with variance threshold cutoff
    pipe_lr2 = Pipeline([ 
                         ('vt', VarianceThreshold(threshold = 0.01)), 
                         ('ss', StandardScaler()), 
                         ('pca', PCA(n_components = 2)), 
                         ('model', lr)] )
    
    # 9. Operate both pipes
    pipe_lr1.fit(X_train,y_train)
    pipe_lr2.fit(X_train,y_train)
    
    # 9.1 Evaluate predictions score
    pa= pipe_lr1.score(X_test,y_test)
    pb = pipe_lr2.score(X_test,y_test)
    
    # 9.2 You could make predictions also
    # pipe_lr1.predict(X_test)
    
    # 9.3  Store results
    p1.append(pa)
    p2.append(pb)

# 10. Evaluate % of times second pipe gives better results than the first one
sum(np.array(p2) > np.array(p1))/10
##################################
