# -*- coding: utf-8 -*-
"""
Created on Tue Sep 14 14:22:40 2021

@author: abhishek.hegde
"""
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
from scipy.stats import skew
from scipy.stats.stats import pearsonr
from sklearn.model_selection import cross_val_score, train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import LinearRegression, RidgeCV, LassoCV, ElasticNetCV
from sklearn.metrics import mean_squared_error, make_scorer
from scipy.stats import skew


def preprocess(data):
    
    data_flat = data

    data_flat.drop('Pack_Format',axis=1, inplace=True)
    data_flat.drop('Converter',axis=1, inplace=True)
    data_flat.drop('Stack',axis=1, inplace=True)
    data_flat.drop('MetOPP_Supplier',axis=1, inplace=True)
    data_flat.drop('Jaw_Type',axis=1, inplace=True)
    data_flat.drop('Jaw_Width_mm',axis=1, inplace=True)
    data_flat.drop('Specimen_Width_mm',axis=1, inplace=True)
    data_flat.drop('Failure_Mode',axis=1, inplace=True)
    data_flat.drop('Comments',axis=1, inplace=True)
    data_flat.drop('Layer_1_outer',axis=1, inplace=True)
    data_flat.drop('Layer_1_thickness_gsm',axis=1, inplace=True)
    data_flat.drop('Layer_2_sealant',axis=1, inplace=True)
    data_flat.drop('Layer_2_thickness_um',axis=1, inplace=True)
    
    group_cols = list(data_flat.columns)
    group_cols.remove('Seal_Strength_N_15mm')
    data_flat = data_flat[data_flat.Seal_Strength_N_15mm.isna()==0]
    data_flat.fillna(0,inplace=True)
    data = pd.DataFrame(data_flat.groupby(group_cols).Seal_Strength_N_15mm.mean())
    data.to_csv('Agg-Data.csv')
    data = pd.read_csv('Agg-Data.csv')
    data['Mean(Seal_Strength_N_15mm)'] = data['Seal_Strength_N_15mm']
    data_flat.drop('Seal_Strength_N_15mm',axis=1, inplace=True)

    # Drop catergorical columns with only one category throughout dataset
    for col in data.columns:
        if data[col].nunique() == 1:
            print("Dropping ",col)
            data.drop(col,axis=1, inplace=True) 
            
    data['Failure_Mode_C'] = data['Failure_Mode_C'].astype('O')
    data['Failure_Mode_A'] = data['Failure_Mode_A'].astype('O')
    data['Failure_Mode_D'] = data['Failure_Mode_D'].astype('O')
    data['Failure_Mode_FR'] = data['Failure_Mode_FR'].astype('O') 
    data.drop('Seal_Strength_N_15mm',axis=1, inplace=True)
    
    data['Material_Name'] = data['Material_Name'].str.replace(' ', '')    
    
    return data

def feature_engg(data, predictors):
    
    for col in data.columns:
        if (col != 'Mean(Seal_Strength_N_15mm)') & (data[col].dtype in['float64','int64']):
            data[col+"-2"] = data[col]**2
            data[col+"-3"] = data[col]**3
            data[col+"-Sq"] = np.sqrt(data[col])
            data[col+"-log"] = np.log(data[col])
    
    if (('Sealing_Temperature_C' in predictors) & ('Sealing_Time_ms' in predictors)):
        data["Time_Temp"] = data["Sealing_Time_ms"] * data["Sealing_Temperature_C"]
        
    if (('Sealing_Temperature_C' in predictors) & ('Sealing_Pressure_N_cm2' in predictors)):
        data["Temp_Pr"] = data["Sealing_Pressure_N_cm2"] * data["Sealing_Temperature_C"]
        
    if (('Sealing_Time_ms' in predictors) & ('Sealing_Pressure_N_cm2' in predictors)):
        data["Time_Pr"] = data["Sealing_Time_ms"] * data["Sealing_Pressure_N_cm2"]
    
    print(data.info())
    return data

def get_materials(data_R):
    
    data = data_R
    Material_Names = list(data.Material_Name.value_counts().index)
    
    for i,name in enumerate(Material_Names):
        Material_Names[i] = name.replace(" ", "")
        
    return Material_Names

def get_predictors(data_R):
    
    data_flat = data_R
    data = preprocess(data_flat)

    data.drop('Mean(Seal_Strength_N_15mm)',axis=1, inplace=True)
    data.drop('Material_Name',axis=1, inplace=True)
    
    return list(data.columns)

def run_model(data_R, predictors, response, material, val_flag):
    

    data_flat = data_R

    data = preprocess(data_flat)

    # choosing predictors
    data_orig = data.copy()
    
    for col in data.columns:
        if col not in predictors:
            data.drop(col,axis=1, inplace=True)

    data['Mean(Seal_Strength_N_15mm)'] = data_orig['Mean(Seal_Strength_N_15mm)']
    data['Material_Name'] = data_orig['Material_Name']

    data = feature_engg(data, predictors)
    
    data_mat = data[data.Material_Name == material]
    data_mat.drop('Material_Name', inplace=True, axis = 1)
    


    num_cols = list()
    for col in data_mat.columns:
        if data_mat[col].dtype != 'O':
            num_cols.append(col)
   
    from sklearn.preprocessing import StandardScaler
    scaler = StandardScaler()
    data_mat[num_cols] = scaler.fit_transform(data_mat[num_cols])
    
    from sklearn.preprocessing import LabelEncoder as LE
   
    le= LE()
   
    for col in data_mat.columns:
        if data_mat[col].dtype == 'O':
            data_mat[col] = le.fit_transform(data_mat[col])
   
    #data_mat.to_csv('data_mat.csv',index=False)
   
    X_train = data_mat.drop('Mean(Seal_Strength_N_15mm)', axis=1)
    y_train = data_mat['Mean(Seal_Strength_N_15mm)']

   
    from sklearn.feature_selection import RFE
    from sklearn.linear_model import LinearRegression
   
   
    regressor = LinearRegression()
    regressor.fit(X_train, y_train)
   
    y_train_mlr = regressor.predict(X_train)
    
    
    # from sklearn.metrics import r2_score
    # print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_mlr)*100,4))))
    # 
    # from sklearn.metrics import r2_score
    # print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_mlr, squared = False),4))))
    
    sns.set_style('whitegrid')
    # Plot residuals
    plt.figure(figsize=(13,7))
    plt.scatter(y_train_mlr, y_train_mlr - y_train, c = "darkred", marker = "*", alpha=0.5, label = "Training data")
    #plt.scatter(y_test_las, y_test_las - y_test, c = "darkblue", alpha=0.5, label = "Validation data")
    plt.title("MLR Residuals",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Residuals",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    plt.savefig('static/plots/Plot_MLR_Residuals.jpg',dpi=300)
    # Plot predictions
    plt.figure(figsize=(10,9))
    plt.scatter(y_train_mlr, y_train, c = "darkred", alpha=0.5, marker = "*", label = "Training data")
    #plt.scatter(y_test_las, y_test, c = "darkblue", alpha=0.5, label = "Validation data")
    plt.title("MLR Predicted vs Real",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Real values",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    x1, y1 = [-2,3],[-2,3]
    x2, y2 = [-2,3], [-2,3]
    plt.plot(x1, y1, x2, y2, marker = 'o')
    plt.show()
    plt.savefig('static/plots/Plot_MLR_Predicted.jpg',dpi=300)
    
    # Plot important coefficients
    coefs = pd.Series(regressor.coef_, index = X_train.columns)
    imp_coefs = pd.concat([coefs.sort_values().head(10),
                         coefs.sort_values().tail(10)])
    imp_coefs.plot(kind = "barh")
    
    plt.title("Coefficients in the MLR Model")
    plt.savefig('static/plots/Plot_MLR_coefs.jpg',dpi=300)
    
    
    coefs = pd.DataFrame(coefs)
    coefs = coefs[abs(coefs[0])>=0.01]
    coefs.sort_values(by=0,ascending=False)
    coefs.reset_index(inplace=True)
    coefs.columns = ['Feature','Importance']
    coefs.sort_values(by='Importance',ascending=False,inplace=True)
    topMLR = coefs

    from sklearn.metrics import r2_score
    print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_mlr)*100,4))))
    
   
    from sklearn.metrics import r2_score
    print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_mlr,squared=False),4))))
    
    r2_MLR = round(r2_score(y_train,y_train_mlr)*100,2)
    rmse_MLR = round(mean_squared_error(y_train,y_train_mlr,squared=False),2)
    
    results = pd.DataFrame({'Method':['MLR'], 'RMSE': [rmse_MLR],'R2': [r2_MLR] })
    results = results[['Method', 'RMSE', 'R2']]
    
    
    # Lasso
   
    lasso = LassoCV(alphas = [0.0001, 0.0003, 0.0006, 0.001, 0.003, 0.006, 0.01, 0.03, 0.06, 0.1,
                              0.3, 0.6, 1],
                    max_iter = 50000, cv = 3)
    lasso.fit(X_train, y_train)
    alpha = lasso.alpha_
    print("Coarse Tuned Alpha :", alpha)
   
    lasso = LassoCV(alphas = [alpha * .6, alpha * .65, alpha * .7, alpha * .75, alpha * .8,
                              alpha * .85, alpha * .9, alpha * .95, alpha, alpha * 1.05,
                              alpha * 1.1, alpha * 1.15, alpha * 1.25, alpha * 1.3, alpha * 1.35,
                              alpha * 1.4],
                    max_iter = 50000, cv = 3)
    lasso.fit(X_train, y_train)
    alpha = lasso.alpha_
    print("Fine Tuned Alpha :", alpha)
   
    y_train_las = lasso.predict(X_train)
    
    # Plot residuals
    plt.figure(figsize=(13,7))
    plt.scatter(y_train_las, y_train_las - y_train, c = "darkred", marker = "*", alpha=0.5, label = "Training data")
    #plt.scatter(y_test_las, y_test_las - y_test, c = "darkblue", alpha=0.5, label = "Validation data")
    plt.title("Lasso Regularization - Residuals",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Residuals",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    plt.savefig('static/plots/Plot_LASSO_Residuals.jpg',dpi=300)
    
    # Plot predictions
    plt.figure(figsize=(10,9))
    plt.scatter(y_train_las, y_train, c = "darkred", alpha=0.5, marker = "*", label = "Training data")
    #plt.scatter(y_test_las, y_test, c = "darkblue", alpha=0.5, label = "Validation data")
    plt.title("Lasso Regularization - Predicted vs Real",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Real values",fontsize=14)
    x1, y1 = [-2,3],[-2,3]
    x2, y2 = [-2,3], [-2,3]
    plt.plot(x1, y1, x2, y2, marker = 'o')
    plt.legend(loc = "upper left",fontsize=14)
    plt.savefig('static/plots/Plot_LASSO_Predicted.jpg',dpi=300)
    
    # Plot important coefficients
    coefs = pd.Series(lasso.coef_, index = X_train.columns)
    print("Lasso picked " + str(sum(coefs != 0)) + " features and eliminated the other " +  \
          str(sum(coefs == 0)) + " features")
    imp_coefs = pd.concat([coefs.sort_values().head(10),
                         coefs.sort_values().tail(10)])
    imp_coefs.plot(kind = "barh")
    plt.title("Coefficients in the Lasso Model")
    plt.savefig('static/plots/Plot_LASSO_coefs.jpg',dpi=300)
    
    coefs = pd.DataFrame(coefs)
    coefs = coefs[abs(coefs[0])>=0.01]
    coefs.sort_values(by=0,ascending=False)
    coefs.reset_index(inplace=True)
    coefs.columns = ['Feature','Importance']
    coefs.sort_values(by='Importance',ascending=False,inplace=True)
    topLASSO = coefs
    

    from sklearn.metrics import r2_score
    print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_las)*100,4))))
   
    from sklearn.metrics import r2_score
    print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_las,squared=False),4))))
   
    r2_LASSO = round(r2_score(y_train,y_train_las)*100,2)
    rmse_LASSO = round(mean_squared_error(y_train,y_train_las,squared=False),2)
    
    tempResults = pd.DataFrame({'Method':['Lasso'], 'RMSE': [rmse_LASSO],'R2': [r2_LASSO] })

    results = pd.concat([results, tempResults])
    results = results[['Method', 'RMSE', 'R2']]
    results.sort_values(by='R2', ascending=False, inplace=True)
    
    return results,topMLR,topLASSO
