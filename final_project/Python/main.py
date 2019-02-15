import pandas as pd
from models.sklearn_cv import calculate_design_df
from models.parameters.random_forest_parameters import param_grid


path_data = r'../Data/'
path_output = r'../Output/'
df = pd.read_csv(path_data + 'falldetection.csv')
dep_variable = 'ACTIVITY'

design_df = calculate_design_df(estimator_name='RandomForestClassifier', df=df,
                                param_grid=param_grid, label=dep_variable)
