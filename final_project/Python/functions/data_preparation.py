import pandas as pd
import numpy as np
from sklearn import preprocessing


def read_data(path: str, file_name: str):
    """
    Reading data from the memory

    :param path: path to the needed directory
    :param file_name: name of the csv-file containing data
    :return: df
    """

    df = pd.read_csv(path + file_name)

    return df


def fill_and_normalize(df: pd.DataFrame, cols: list):
    """
    Fill in NAs with median values and rescale the data to [0, 1] in terms of columns

    :param df: data frame
    :param cols: columns to process
    :return: processed_df
    """

    for col in cols:
        min_max_scaler = preprocessing.MinMaxScaler()
        df[col] = min_max_scaler.fit_transform(
            np.asarray(df[col]).reshape(-1, 1)
        )
        df[col] = df[col].fillna(df[col].median())

    return df


def load_data(no_preprocessing: iter, path: str = r'../Data/', file_name: str = 'falldetection.csv'):

    df = read_data(path, file_name)

    cols_to_process = [col for col in df.columns if col not in no_preprocessing]
    df = fill_and_normalize(df, cols_to_process)

    return df
