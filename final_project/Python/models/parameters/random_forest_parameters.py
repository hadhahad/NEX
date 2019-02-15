import numpy as np

"""
    Parameter grids for cross-validation
"""

# Number of trees in random forest
n_estimators = [int(x) for x in np.linspace(start=10, stop=1000, num=20, dtype=np.int)]

# Number of features to consider at every split
max_features = ['auto', 'sqrt']

# Maximum number of levels in tree
max_depth = [int(x) for x in np.linspace(10, 110, num=5, dtype=np.int)]
# noinspection PyTypeChecker
max_depth.append(None)

# Minimum number of samples required to split a node
min_samples_split = [2, 5, 10]

# Estimation criterion
criterion = ['entropy', 'gini']

# Method of selecting samples for training each tree
bootstrap = [True, False]

# Random grid
random_grid = {'n_estimators': n_estimators,
               'max_features': max_features,
               'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'criterion': criterion,
               'bootstrap': bootstrap}


# Parameter grid based on the results of random search
param_grid = {
    'n_estimators': [10, 500],
    'min_samples_split': [4, 20],
    'max_features': ['sqrt', 'log2'],
    'max_depth': [10, 40],
    'criterion': ['entropy', 'gini'],
    'bootstrap': [True, False]
}

center_para_grid = {
    'n_estimators': [450],
    'min_samples_split': [4],
    'max_features': ['sqrt', 'log2'],
    'max_depth': [30],
    'criterion': ['entropy', 'gini'],
    'bootstrap': [True, False]
}
