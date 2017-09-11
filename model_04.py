
#%%                         Call libraries
%reset -f
#1.1
import numpy as np
# 1.2
import pandas as pd
# 1.3
from sklearn.preprocessing import LabelEncoder
# 1.4
from sklearn.cross_validation import train_test_split
# 1.5 Read at the end about what is BaggingClassifier
from sklearn.ensemble import RandomForestClassifier, BaggingClassifier 
# 1.5 To calculate cross-entropy
from sklearn.metrics import log_loss
# 1.6  Callibrate probabilities
from sklearn.calibration import CalibratedClassifierCV
# 1.7 Measure process time & about OS
import time
import os

#%%                     Read otto data and explore
# 2
os.chdir("C:\\Users\\ashok\\OneDrive\\Documents\\fdp\\fdp\\decisiontree")
X = pd.read_csv('train.csv')
X.columns
X.head(2)

# 2.1 Drop column not needed
X = X.drop('id', axis=1)
X.columns
# 2.2 Check distribution of classes
X.target.value_counts()

# 2.3 Extract target
#     Encode it to make it manageable by ML algo
y = X.target.values
y = LabelEncoder().fit_transform(y)

# 2.4 Remove target from train
X = X.drop('target', axis=1)

# 2.5 Finally..
X.head(5)
y
np.unique(y)

#%%                     Modeling and predicting probabilties

# 3. Split into train and validation data
Xtrain, Xtest, ytrain, ytest = train_test_split(X, y, test_size=0.20, random_state=36)


#%%                       Case 1: Just with Random Forest
# 4. Predict just using Rando Forest
clf = RandomForestClassifier(n_estimators=50, n_jobs=-1)
# 4.1 Create model
clf.fit(Xtrain,ytrain)
# 4.2 Make probabilistic predictions
ypreds = clf.predict_proba(Xtest)    
# 4.3 Calculate logloss
# What is logloss: Log loss and cross entropy are measures of error used
#   in machine learning. The underlying math is the same. Log loss is usually
#    used when there are just two possible outcomes that can be either 0 or 1.
#     Cross entropy is usually used when there are three or more possible outcomes.
#  eps: Log loss is undefined for p=0, so probabiliy is clipped to
#        eps
ll =log_loss(ytest, ypreds, eps=1e-15)
ll          # 68% logloss


#%%                       Case 2: Random Forest + Bagging Classifier

# 5. Wrap RandoForest within BaggingClassifier to increase accuracy
start = time.time()
# we use a BaggingClassifier to make 5 predictions, and average
# beacause that's what CalibratedClassifierCV do behind the scene
# and we want to compare things fairly
# n_estimators: The number of base estimators in the ensemble.
# 5.1
clfbag = BaggingClassifier(base_estimator=clf, n_estimators=5 , verbose = 8) 
# 5.2 Train model
clfbag.fit(Xtrain, ytrain)
# 5.3 MAke probability predictions
ypreds = clfbag.predict_proba(Xtest)       # Predict probability
# 5.4 Logloss
ll =log_loss(ytest, ypreds, eps=1e-15, normalize=True)
end = time.time() - start
ll           # 60% logloss

#%%                  Case 3: RandomForest + Calibrate prob

# 6 Calibrate now
start = time.time()
# 6.1 Random Classifier object
clf = RandomForestClassifier(n_estimators=50, n_jobs=-1)

# 6.2 Create calibration object. Method to calibrate: isotonic regression
# in our case, 'isotonic' works better than default 'sigmoid'
calibrated_clf = CalibratedClassifierCV(clf, method='isotonic', cv=5)

# 6.3 Train callibration object
calibrated_clf.fit(Xtrain, ytrain)
# 6.4 Probabilities?
ypreds = calibrated_clf.predict_proba(Xtest)
# 6.5 logloss?
ll=log_loss(ytest, ypreds, eps=1e-15, normalize=True)
end = time.time() - start
ll               # 0.49%

#%%                     About Bagging classifier

"""
A Bagging classifier is an ensemble meta-estimator that fits base
classifiers each on random subsets of the original dataset and then
aggregate their individual predictions (either by voting or by averaging)
to form a final prediction. Such a meta-estimator can typically be used
as a way to reduce the variance of a black-box estimator (e.g., a decision
tree), by introducing randomization into its construction procedure and then
making an ensemble out of it.

This algorithm encompasses several works from the literature. When random
subsets of the dataset are drawn as random subsets of the samples, then this
algorithm is known as Pasting.
If samples are drawn with replacement, then the method is known as
Bagging (default).
When random subsets of the dataset are drawn as random subsets of the
features, then the method is known as Random Subspaces.
Finally, when base estimators are built on subsets of both samples and
features, then the method is known as Random Patches.
"""

############################################
