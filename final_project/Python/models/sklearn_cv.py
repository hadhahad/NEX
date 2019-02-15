import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV, KFold, cross_validate
from itertools import product
np.random.seed(1234)


def cross_validate_random(estimator_name: str, random_grid: dict,
                          df: pd.DataFrame,
                          label: str, leading_col: str = None):

    # Separate data into labels and features
    labels = np.asarray(df.loc[:, label])
    if leading_col is None:
        features = df.drop([label], axis=1)
    else:
        features = df.drop([label, leading_col], axis=1)
    features = np.asarray(features)

    # Instantiate the model
    if estimator_name == "RandomForestClassifier":
        estimator = RandomForestClassifier()
    else:
        raise Exception("No such estimator is implemented here.")

    # Random search of parameters
    random_search = RandomizedSearchCV(estimator=estimator,
                                       param_distributions=random_grid,
                                       n_iter=100, cv=3, verbose=2,
                                       random_state=1234, n_jobs=-1)
    # Fit the random search model
    random_search.fit(features, labels)

    return random_search


def calculate_design_df(estimator_name: str, df: pd.DataFrame,
                        param_grid: dict, label: str, n_splits: int = 5):

    # Only Random Forest is available
    if estimator_name != "RandomForestClassifier":
        raise Exception("No such estimator is implemented here.")

    # Transform the parameter grid to design data frame
    design_data = {key: [] for key in param_grid.keys()}
    for params in product(*param_grid.values()):
        for idx, param_name in enumerate(design_data.keys()):
            design_data[param_name].append(params[idx])
    design_data = pd.DataFrame(design_data)
    accuracies = np.zeros(design_data.shape[0])

    # Calculate model accuracy for each of the grid elements
    for idx in design_data.index:
        print(f'\tPerforming cross-validation for the grid '
              f'item {idx + 1} out of {len(design_data.index)}...')
        rf = RandomForestClassifier(**design_data.loc[idx, param_grid.keys()])
        kf = KFold(n_splits=n_splits)
        scores = cross_validate(rf, df.drop(label, axis=1), df.loc[:, label],
                                scoring='precision_macro',
                                cv=kf, return_train_score=False)
        accuracies[idx] = scores['test_score'].mean()
        design_data.loc[:, 'accuracy'] = accuracies
        design_data.to_csv(f'../Output/temp{idx + 1}_design_df.csv')

    return design_data
