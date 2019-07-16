# pandas and numpy for data manipulation
import pandas as pd
import numpy as np
import gc

def hello_world():
    print('hello world!')

def main():
    print('Agg_functions loaded!')
    
if __name__== "__main__":
  main()


def agg_numeric(df, parent_var, df_name):
    """
    Groups and aggregates the numeric values in a child dataframe
    by the parent variable.
    
    Parameters
    --------
        df (dataframe): 
            the child dataframe to calculate the statistics on
        parent_var (string): 
            the parent variable used for grouping and aggregating
        df_name (string): 
            the variable used to rename the columns
        
    Return
    --------
        agg (dataframe): 
            a dataframe with the statistics aggregated by the `parent_var` for 
            all numeric columns. Each observation of the parent variable will have 
            one row in the dataframe with the parent variable as the index. 
            The columns are also renamed using the `df_name`. Columns with all duplicate
            values are removed. 
    
    """
    # Remove id variables other than grouping variable
    for col in df:
        if col != parent_var and 'SK_ID' in col:
            df = df.drop(columns = col)
    
    #Select numeric columns only. Add function so that if parent_var is not numeric, it gets added back
    parent_ids = df[parent_var].copy()
    numeric_df = df.select_dtypes('number').copy()
    numeric_df[parent_var] = parent_ids
    
    # Group by the specified variable and calculate the statistics
    agg = numeric_df.groupby(parent_var).agg(['count', 'mean', 'max', 'min', 'sum'])

    # Need to create new column names
    columns = []

    # Iterate through the variables names
    for var in agg.columns.levels[0]:
        if var != parent_var:
            # Iterate through the stat names
            for stat in agg.columns.levels[1]:
                # Make a new column name for the variable and stat
                columns.append('%s_%s_%s' % (df_name, var, stat))
    
    agg.columns = columns
    
    # Remove the columns with all redundant values
    _, idx = np.unique(agg, axis = 1, return_index=True)
    agg = agg.iloc[:, idx]
    
    return agg

def agg_categorical(df, parent_var, df_name):
    """
    Aggregates the categorical features in a child dataframe
    for each observation of the parent variable.
    
    Parameters
    --------
    df : dataframe 
        The dataframe to calculate the value counts for.
        
    parent_var : string
        The variable by which to group and aggregate the dataframe. For each unique
        value of this variable, the final dataframe will have one row
        
    df_name : string
        Variable added to the front of column names to keep track of columns

    
    Return
    --------
    categorical : dataframe
        A dataframe with aggregated statistics for each observation of the parent_var
        The columns are also renamed and columns with duplicate values are removed.
        
    """
    
    # Select the categorical columns
    categorical = pd.get_dummies(df.select_dtypes('object'))

    # Make sure to put the identifying id on the column
    categorical[parent_var] = df[parent_var]

    # Groupby the group var and calculate the sum and mean
    categorical = categorical.groupby(parent_var).agg(['sum', 'count', 'mean'])
    
    column_names = []
    
    # Iterate through the columns in level 0
    for var in categorical.columns.levels[0]:
        # Iterate through the stats in level 1
        for stat in ['sum', 'count', 'mean']:
            # Make a new column name
            column_names.append('%s_%s_%s' % (df_name, var, stat))
    
    categorical.columns = column_names
    
    # Remove duplicate columns by values
    _, idx = np.unique(categorical, axis = 1, return_index = True)
    categorical = categorical.iloc[:, idx]
    
    return categorical

def agg_child(df, parent_var, df_name):
    """Aggregate a child dataframe for each observation of the parent."""
    
    # Numeric and then categorical
    df_agg = agg_numeric(df, parent_var, df_name)
    df_agg_cat = agg_categorical(df, parent_var, df_name)
    
    # Merge on the parent variable
    df_info = df_agg.merge(df_agg_cat, on = parent_var, how = 'outer')
    
    # Remove any columns with duplicate values
    _, idx = np.unique(df_info, axis = 1, return_index = True)
    df_info = df_info.iloc[:, idx]
    
    # memory management
    gc.enable()
    del df_agg, df_agg_cat
    gc.collect()
    
    return df_info

def kaggle_submission(cv_results, train, test):
    
    # Extract the labels
    train_labels = np.array(train['TARGET'].astype(np.int32)).reshape((-1, ))
    train = train.drop(columns = ['TARGET', 'SK_ID_CURR'])
    test_ids = list(test['SK_ID_CURR'])
    test = test.drop(columns = ['TARGET', 'SK_ID_CURR']) 
    
    # Make model with optimal number of estimators and train on training data
    model = lgb.LGBMClassifier(n_estimators = len(cv_results['auc-mean']), random_state=RSEED)
    model.fit(train, train_labels)
    
    # Make predictions on the testing data
    preds = model.predict_proba(test)[:, 1]
    submission = pd.DataFrame({'SK_ID_CURR': test_ids, 
                                'TARGET': preds})
    
    return submission

