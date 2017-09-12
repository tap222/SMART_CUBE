import numpy as np
from sklearn.preprocessing import Imputer

X = np.array([[23.56],[53.45],['NaN'],[44.44],[77.78],['NaN'],[234.44],[11.33],[79.87]])

print X

imp = Imputer(missing_values='NaN', strategy='mean', axis=0)
X = imp.fit_transform(X)

print X
