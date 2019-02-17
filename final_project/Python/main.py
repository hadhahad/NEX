import pandas as pd
from models.sklearn_cv import calculate_design_df
from models.parameters.random_forest_parameters import param_grid, center_param_grid


path_data = r'../Data/'
path_output = r'../Output/'
df = pd.read_csv(path_data + 'falldetection.csv')
dep_variable = 'ACTIVITY'


if __name__ == '__main__':

    design_df = calculate_design_df(estimator_name='RandomForestClassifier', df=df,
                                    param_grid=center_param_grid, label=dep_variable)

    design_df.to_csv(path_data + 'design_df_center.csv')
