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
import warnings
import openpyxl
warnings.filterwarnings('ignore')
#-------------------------------------------------------------------------------
def get_equation(coefs):

    #print(coefs)

    equation= ""
    for i in range(len(coefs)):
        for j in range(2):
            if j==0:
                equation= equation + "("+str(coefs.iloc[i,j]) +")*"
            elif j==1:
                equation= equation + "("+str(coefs.iloc[i,j]) + ") + "
                
    length = len(equation)
    equation2=""
    
    # to remove last '+'
    for i in range(length):
        if(equation[i] == '+'):
            equation2 = equation[0:i] + equation[i+1:length]
            
    equation2 = equation2.replace("Intercept","1")

    return equation2
#-------------------------------------------------------------------------------    
def preprocess(data,polyFlag, valFlag):
    
    
    data_flat = data
    
    data_flat.Validation.fillna(0,inplace=True)
    
    data_flat_mat = data_flat[data_flat.Material_Name == data_flat.Material_Name.value_counts().index[0]]

    # Drop catergorical columns with only one category per material throughout dataset
    for col in data_flat_mat.columns:
        if (data_flat_mat[col].nunique() == 1) & (col not in ['Material_Name','Validation']):
            #print("Dropping ",col)
            data_flat.drop(col,axis=1, inplace=True) 
    
    # common columns to drop
    #data_flat.drop('Pack_Format',axis=1, inplace=True)
    #data_flat.drop('Converter',axis=1, inplace=True)
    #data_flat.drop('Stack',axis=1, inplace=True)
    #data_flat.drop('Layer_1_outer',axis=1, inplace=True)
    #data_flat.drop('Jaw_Type',axis=1, inplace=True)
    #data_flat.drop('Jaw_Width_mm',axis=1, inplace=True)
    #data_flat.drop('Specimen_Width_mm',axis=1, inplace=True)
    #data_flat.drop('Layer_2_thickness_um',axis=1, inplace=True)
    data_flat.drop('Sealing_Force_N',axis=1, inplace=True)
    failure_mode_cols = ['Failure_Mode_A','Failure_Mode_D','Failure_Mode','Failure_Mode_FR','Failure_Mode_C']
    for col in data_flat.columns:
        if col in failure_mode_cols:
        #print("Dropping ",col)
            data_flat.drop(col,axis=1, inplace=True)

    
    # laminate family specify dropping
    # if polyFlag == False:
    #     data_flat.drop('MetOPP_Supplier',axis=1, inplace=True)
    #     data_flat.drop('Failure_Mode',axis=1, inplace=True)
    #     data_flat.drop('Failure_Mode_A',axis=1, inplace=True)
    #     data_flat.drop('Failure_Mode_D',axis=1, inplace=True)
    #     data_flat.drop('Comments',axis=1, inplace=True)
    #     data_flat.drop('Layer_1_thickness_gsm',axis=1, inplace=True)
    #     data_flat.drop('Layer_2_sealant',axis=1, inplace=True)
        
    #print("dataflat",data_flat.info())
    #if polyFlag == True:
        #data_flat.drop('Layer_2',axis=1, inplace=True)
        #data_flat.drop('Layer_3_sealant',axis=1, inplace=True)
        #data_flat.drop('Layer_1_thickness_um',axis=1, inplace=True)
        #data_flat['Sealent_layer_thickness'] = data_flat['Layer_3_thickness_um']
        #data_flat.drop('Layer_3_thickness_um',axis=1, inplace=True)

    
    group_cols = list(data_flat.columns)
    group_cols.remove('Seal_Strength_N_15mm')
    group_cols.remove('Validation')
    data_flat = data_flat[data_flat.Seal_Strength_N_15mm.isna()==0]
    data_flat.fillna(0,inplace=True)
    #print(data_flat)
    data = pd.DataFrame(data_flat.groupby(group_cols).agg({'Seal_Strength_N_15mm':'mean','Validation':'max'}))
    #print(data)
    data.to_csv('Agg-Data.csv')
    data = pd.read_csv('Agg-Data.csv')
    data['Mean(Seal_Strength_N_15mm)'] = data['Seal_Strength_N_15mm']
    data_flat.drop('Seal_Strength_N_15mm',axis=1, inplace=True)

    data.drop('Seal_Strength_N_15mm',axis=1, inplace=True)
    #print(data)
    #print(data.info())
    
    #if polyFlag == False:
        #data.drop('Failure_Mode_C',axis=1, inplace=True)
        #data.drop('Failure_Mode_A',axis=1, inplace=True)
        #data.drop('Failure_Mode_D',axis=1, inplace=True)
        #data.drop('Failure_Mode_FR',axis=1, inplace=True)
    
    data['Material_Name'] = data['Material_Name'].str.replace(' ', '')  
    
    
    # adding validation column
    # if valFlag == True:
    #   
    #   if polyFlag==True:
    #       polymer_means = pd.read_excel('www/polymer_means_final.xlsx',engine='openpyxl')
    #       data['Validation'] = polymer_means.Validation
    #       data.Validation.fillna(0,inplace=True)
    #       
    #   if polyFlag==False:
    #       for i,row in data.iterrows():
    #         if ((row['Sealing_Temperature_C']==120) & (row['Sealing_Pressure_N_cm2']==38.5) & (row['Sealing_Time_ms']==200)):
    #             data.loc[i,'Validation'] = 1    
    #         if ((row['Sealing_Temperature_C']==120) & (row['Sealing_Pressure_N_cm2']==115.5) & (row['Sealing_Time_ms']==750)):
    #             data.loc[i,'Validation'] = 1   
    #         if ((row['Sealing_Temperature_C']==240) & (row['Sealing_Pressure_N_cm2']==38.5) & (row['Sealing_Time_ms']==500)):
    #             data.loc[i,'Validation'] = 1   
    #         if ((row['Sealing_Temperature_C']==168) & (row['Sealing_Pressure_N_cm2']==115.5) & (row['Sealing_Time_ms']==200)):
    #             data.loc[i,'Validation'] = 1  
    #       data['Validation'].fillna(0,inplace=True)
    
    #print(data)
    return data
#-------------------------------------------------------------------------------
def feature_engg(data, predictors):
    
    for col in data.columns:
        if  ((col not in ['Mean(Seal_Strength_N_15mm)','Validation']) & (data[col].dtype in['float64','int64'])):
            data[col+"-2"] = data[col]**2
            #data[col+"-3"] = data[col]**3
            #data[col+"-Sq"] = np.sqrt(data[col])
            #data[col+"-log"] = np.log(data[col])
    
    if (('Sealing_Temperature_C' in predictors) & ('Sealing_Time_ms' in predictors)):
        data["Time_Temp"] = data["Sealing_Time_ms"] * data["Sealing_Temperature_C"]
        
    if (('Sealing_Temperature_C' in predictors) & ('Sealing_Pressure_N_cm2' in predictors)):
        data["Temp_Pr"] = data["Sealing_Pressure_N_cm2"] * data["Sealing_Temperature_C"]
        
    if (('Sealing_Time_ms' in predictors) & ('Sealing_Pressure_N_cm2' in predictors)):
        data["Time_Pr"] = data["Sealing_Time_ms"] * data["Sealing_Pressure_N_cm2"]
        
    #if (('Sealent_layer_thickness' in predictors) & ('Sealing_Temperature_C' in predictors)):
    #    data["Temp_Thick"] = data["Sealent_layer_thickness"] * data["Sealing_Temperature_C"]
    
    #if (('Sealing_Time_ms' in predictors) & ('Sealent_layer_thickness' in predictors)):
    #    data["Time_Thick"] = data["Sealing_Time_ms"] * data["Sealent_layer_thickness"]
        
    #if (('Sealent_layer_thickness' in predictors) & ('Sealing_Pressure_N_cm2' in predictors)):
    #    data["Pr_Thick"] = data["Sealing_Pressure_N_cm2"] * data["Sealent_layer_thickness"]
        
    return data
#-------------------------------------------------------------------------------
def get_materials(data_R):
    
    data = data_R
    Material_Names = list(data.Material_Name.value_counts().index)
    
    for i,name in enumerate(Material_Names):
        Material_Names[i] = name.replace(" ", "")
        
    return Material_Names
#-------------------------------------------------------------------------------
def get_predictors(data_R,polyFlag,valFlag):
    
    data_flat = data_R
    data = preprocess(data_flat,polyFlag,valFlag)

    data.drop('Mean(Seal_Strength_N_15mm)',axis=1, inplace=True)
    data.drop('Validation',axis=1, inplace=True)
    data.drop('Material_Name',axis=1, inplace=True)
    
    return list(data.columns)
#-------------------------------------------------------------------------------
def run_model(data_R, predictors, material, polyFlag, valFlag):
  
  
    if isinstance(predictors, str):
      predictors= list([predictors])
    
    
    data_flat = data_R

    data = preprocess(data_flat,polyFlag,valFlag)
    #print(data)
    data_orig = data.copy()
    
    # choosing predictors
    for col in data.columns:
        if ((col not in predictors)&(col !='Validation')):
            data.drop(col,axis=1, inplace=True)

    data['Mean(Seal_Strength_N_15mm)'] = data_orig['Mean(Seal_Strength_N_15mm)']
    data['Material_Name'] = data_orig['Material_Name']
    #print("----------------*------------ \n This is Orig Data ",data_orig.info())
    #print("----------------*------------ \n This is Data b4 FE ",data.info())

    data = feature_engg(data, predictors)
    #print("----------------*------------ \n This is Data after FE ",data.info())
    
    #print(material)
    #print("data material \n",data.Material_Name)
    data_mat = data[data.Material_Name == material]
    
    #print("----------------*------------ \n This is Material data",data_mat)
    data_mat.drop('Material_Name', inplace=True, axis = 1)
    
    # removing one unique outlier for polymer data
    if polyFlag== True:
        if (('Sealing_Temperature_C' in predictors) & ('Sealing_Pressure_N_cm2' in predictors) & ('Sealing_Time_ms' in predictors)):
          data_mat= data_mat[((data_mat.Sealing_Pressure_N_cm2!=25)|(data_mat.Sealing_Temperature_C!=100)|(data_mat.Sealing_Time_ms!=200))]
    
    if material in ['Paper90+metOPP18(Huhtamaki)','Paper90+metOPP18(Huhtamaki2)']:
        data_mat= data_mat[((data_mat.Sealing_Temperature_C!=120)|(data_mat['Mean(Seal_Strength_N_15mm)']>4))]
    
    #generate correlation heatmap
    heatmap_list = predictors
    heatmap_list.extend(['Mean(Seal_Strength_N_15mm)'])
    #print(heatmap_list)
    plt.figure(figsize=(10,7))
    matrix = np.triu(data_mat[heatmap_list].corr())
    sns.set_style('white')
    sns.heatmap(data_mat[heatmap_list].corr(), annot_kws={'size': 15},mask=matrix,annot=True)
    plt.title('Numerical Feature Correlations', fontsize=17)
    plt.xticks(fontsize=14, rotation=45)
    plt.yticks(fontsize=14)
    plt.savefig('./www/Correlation_Heatmap.jpg', bbox_inches = 'tight',dpi=200)

    
    num_cols = list()
    for col in data_mat.columns:
        if ((data_mat[col].dtype != 'O') & (col not in ['Mean(Seal_Strength_N_15mm)','Validation'])):
            num_cols.append(col)
    #print(data_mat)

    
    from sklearn.preprocessing import StandardScaler
    scaler = StandardScaler()
    #data_mat[num_cols] = scaler.fit_transform(data_mat[num_cols])
    
    from sklearn.preprocessing import LabelEncoder as LE
   
    le= LE()
   
    for col in data_mat.columns:
        if data_mat[col].dtype == 'O':
            data_mat[col] = le.fit_transform(data_mat[col])
   
    #data_mat.to_csv('data_mat.csv',index=False)
    #print("THIS IS DATAMAT ",data_mat.info())
    #print("\n 1 \n")

    if (valFlag==True) & (data_mat.Validation.nunique()!=1) :
      
      #print("\n 2 \n")

    
      X_train = data_mat[data_mat.Validation == 0]
      X_val = data_mat[data_mat.Validation == 1]
      y_train = X_train['Mean(Seal_Strength_N_15mm)']
      y_val = X_val['Mean(Seal_Strength_N_15mm)']
      
      X_train.drop('Mean(Seal_Strength_N_15mm)', axis=1 , inplace= True)
      X_train.drop('Validation', axis=1, inplace= True)
      
      X_val.drop('Mean(Seal_Strength_N_15mm)', axis=1, inplace= True)
      X_val.drop('Validation', axis=1, inplace= True)
    
      #print("\n 3 \n")

    if (valFlag==False) | (data_mat.Validation.nunique()==1) :
      
      #print("\n 3 \n")
    
      y_train = data_mat['Mean(Seal_Strength_N_15mm)']
      #print(y_train.shape)
      X_train = data_mat.drop('Validation', axis=1)
      X_train = X_train.drop('Mean(Seal_Strength_N_15mm)', axis=1)
      #print(X_train)



    from sklearn.feature_selection import RFE
    from sklearn.linear_model import LinearRegression
   
   
    # regressor = LinearRegression()
    # regressor.fit(X_train, y_train)
    # 
    # y_train_mlr = regressor.predict(X_train)
    #print(6)
    # Adding a constant variable 
    import statsmodels.api as sm  
    #X_train_rfe = sm.add_constant(X_train_rfe)
    #print(y_train.info())
    #print(X_train.info())
    
    regressor = LinearRegression()
    regressor = regressor.fit(X_train, y_train)
    
    y_train_mlr = regressor.predict(X_train)
    #lm = sm.OLS(y_train,X_train).fit() 
    #print(7)
   # print(8)
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        y_val_mlr = regressor.predict(X_val)
    # from sklearn.metrics import r2_score
    # print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_mlr)*100,4))))
    # 
    # from sklearn.metrics import r2_score
    # print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_mlr, squared = False),4))))
    
    sns.set_style('whitegrid')
    # Plot residuals
    plt.figure(figsize=(10,9))
    plt.scatter(y_train_mlr, y_train_mlr - y_train, c = "darkred", marker = "*", alpha=0.5, label = "Training data")
    
    limits_list = y_train_mlr
    #print(8)
    
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_mlr, y_val_mlr - y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_mlr, y_val_mlr))
    
    #print(9)    
    lineStart = limits_list.min()-1
    lineEnd = limits_list.max()+1
      
    plt.title("MLR",fontsize=16)
    plt.xlabel("Predicted values",fontsize=10)
    plt.ylabel("Residuals",fontsize=10)
    plt.legend(loc = "upper left",fontsize=10)
        
    plt.plot([lineStart,lineEnd],[0,0], color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.savefig('./www/Plot_MLR_Residuals.jpg', bbox_inches = 'tight',dpi=200)
    
    #print("\n 1 \n")
    
    # Plot predictions
    plt.figure(figsize=(10,9))
    
    limits_list = y_train_mlr
    
    plt.scatter(y_train_mlr, y_train, c = "darkred", alpha=0.5, marker = "*", label = "Training data")
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_mlr, y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_mlr, y_val_mlr))

    lineStart = limits_list.min()-1
    lineEnd =limits_list.max()+1
    
    plt.title("MLR",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Real values",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    
    plt.plot([lineStart,lineEnd],[lineStart,lineEnd] , color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.ylim(lineStart,lineEnd)
    plt.savefig('./www/Plot_MLR_Predicted.jpg', bbox_inches = 'tight',dpi=200)

    # Plot important coefficients
    coefs = pd.Series(regressor.coef_, index = X_train.columns)
    imp_coefs = pd.concat([coefs.sort_values().head(10),
                         coefs.sort_values().tail(10)])
    imp_coefs.plot(kind = "barh")
    
    plt.title("Coefficients in the MLR Model")
    plt.savefig('static/plots/Plot_MLR_coefs.jpg')
    
    from decimal import Decimal

    coefs = pd.DataFrame(coefs)
    #coefs = coefs[abs(coefs[0])>=0.01]
    coefs.sort_values(by=0,ascending=False)
    coefs.reset_index(inplace=True)
    coefs.columns = ['Feature','Importance']
    coefs = coefs.append(pd.DataFrame({'Feature': 'Intercept','Importance':regressor.intercept_},index=[0]))
    coefs.sort_values(by='Importance',ascending=False,inplace=True)
    coefs['Importance'] = coefs['Importance'].apply(lambda x: '%.2e' % Decimal(str(x)))    

    topMLR = coefs
  
    
    from sklearn.metrics import r2_score
    #print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_mlr)*100,4))))
    
   
    from sklearn.metrics import r2_score
    #print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_mlr,squared=False),4))))
    
    r2_MLR = round(r2_score(y_train,y_train_mlr)*100,2)
    rmse_MLR = round(mean_squared_error(y_train,y_train_mlr,squared=False),2)
    
    if (valFlag==False) | (data_mat.Validation.nunique()==1) :
        results = pd.DataFrame({'Algorithm':['MLR'], 'RMSE': [rmse_MLR],'R2': [r2_MLR] })
        results = results[['Algorithm', 'RMSE', 'R2']]
      
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        r2_MLR_val = round(r2_score(y_val,y_val_mlr)*100,2)
        rmse_MLR_val = round(mean_squared_error(y_val,y_val_mlr,squared=False),2)
        results = pd.DataFrame({'Algorithm':['MLR'], 'Train_RMSE': [rmse_MLR],'Validation_RMSE': [rmse_MLR_val],'Train_R2': [r2_MLR],'Validation_R2': [r2_MLR_val]})
        results = results[['Algorithm', 'Train_RMSE', 'Validation_RMSE', 'Train_R2', 'Validation_R2']]
  
    #print("\n 2 \n")

    # Lasso
   
    lasso = LassoCV(alphas = [0.0001, 0.0003, 0.0006, 0.001, 0.003, 0.006, 0.01, 0.03, 0.06, 0.1,
                              0.3, 0.6, 1],
                    max_iter = 50000, cv = 3)
    lasso.fit(X_train, y_train)
    alpha = lasso.alpha_
    #print("Coarse Tuned Alpha :", alpha)
   
    lasso = LassoCV(alphas = [alpha * .6, alpha * .65, alpha * .7, alpha * .75, alpha * .8,
                              alpha * .85, alpha * .9, alpha * .95, alpha, alpha * 1.05,
                              alpha * 1.1, alpha * 1.15, alpha * 1.25, alpha * 1.3, alpha * 1.35,
                              alpha * 1.4],
                    max_iter = 50000, cv = 3)
    lasso.fit(X_train, y_train)
    alpha = lasso.alpha_
    #print("Fine Tuned Alpha :", alpha)
   
    y_train_las = lasso.predict(X_train)

    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        y_val_las = lasso.predict(X_val)
    
    #print("\n 3 \n")

    sns.set_style('whitegrid')
    # Plot residuals
    plt.figure(figsize=(10,9))
    plt.scatter(y_train_las, y_train_las - y_train, c = "darkred", marker = "*", alpha=0.5, label = "Training data")
    
    limits_list = y_train_las
    #print(8)
    
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_las, y_val_las - y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_las, y_val_las))
    
    #print(9)    
    lineStart = limits_list.min()-1
    lineEnd = limits_list.max()+1
      
    plt.title("LASSO Residuals",fontsize=16)
    plt.xlabel("Predicted values",fontsize=10)
    plt.ylabel("Residuals",fontsize=10)
    plt.legend(loc = "upper left",fontsize=10)
        
    plt.plot([lineStart,lineEnd],[0,0], color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.savefig('./www/Plot_LASSO_Residuals.jpg', bbox_inches = 'tight',dpi=200)

    
    # Plot predictions
    plt.figure(figsize=(10,9))
    
    limits_list = y_train_las
    
    plt.scatter(y_train_las, y_train, c = "darkred", alpha=0.5, marker = "*", label = "Training data")
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_las, y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_las, y_val_las))

    lineStart = limits_list.min()-1
    lineEnd =limits_list.max()+1
    
    plt.title("LASSO - Real v/s Predicted",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Real values",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    
    plt.plot([lineStart,lineEnd],[lineStart,lineEnd] , color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.ylim(lineStart,lineEnd)
    plt.savefig('./www/Plot_LASSO_Predicted.jpg', bbox_inches = 'tight',dpi=200)
    
    # Plot important coefficients
    coefs = pd.Series(lasso.coef_, index = X_train.columns)
    #print("Lasso picked " + str(sum(coefs != 0)) + " features and eliminated the other " +  \
          #str(sum(coefs == 0)) + " features")
    imp_coefs = pd.concat([coefs.sort_values().head(10),
                         coefs.sort_values().tail(10)])
    imp_coefs.plot(kind = "barh")
    plt.title("Coefficients in the Lasso Model")
    plt.savefig('static/plots/Plot_LASSO_coefs.jpg')
    
    coefs = pd.DataFrame(coefs)
    coefs = coefs[coefs[0]!=0]
    coefs.sort_values(by=0,ascending=False)
    coefs.reset_index(inplace=True)
    coefs.columns = ['Feature','Importance']
    coefs.sort_values(by='Importance',ascending=False,inplace=True)
    coefs = coefs.append(pd.DataFrame({'Feature': 'Intercept','Importance':lasso.intercept_},index=[0]))
    coefs['Importance'] = coefs['Importance'].apply(lambda x: '%.2e' % Decimal(str(x)))    

    topLASSO = coefs
    

    from sklearn.metrics import r2_score
    #print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_las)*100,4))))
   
    from sklearn.metrics import r2_score
    #print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_las,squared=False),4))))
   
    
    r2_LASSO = round(r2_score(y_train,y_train_las)*100,2)
    rmse_LASSO = round(mean_squared_error(y_train,y_train_las,squared=False),2)
    
    if (valFlag==False) | (data_mat.Validation.nunique()==1) :
      
        tempResults = pd.DataFrame({'Algorithm':['MLR + Lasso'], 'RMSE': [rmse_LASSO],'R2': [r2_LASSO] })
        results = pd.concat([results, tempResults])
        results = results[['Algorithm','R2','RMSE']]
        results.sort_values(by='R2', ascending=False, inplace=True)

    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        r2_LASSO_val = round(r2_score(y_val,y_val_las)*100,2)
        rmse_LASSO_val = round(mean_squared_error(y_val,y_val_las,squared=False),2)
          
        tempResults = pd.DataFrame({'Algorithm':['MLR + Lasso'], 'Train_RMSE': [rmse_LASSO],'Validation_RMSE': [rmse_LASSO_val],'Train_R2': [r2_LASSO],'Validation_R2': [r2_LASSO_val]})
        results = pd.concat([results, tempResults])
        results = results[['Algorithm', 'Train_RMSE', 'Validation_RMSE', 'Train_R2','Validation_R2']]
        results.sort_values(by='Train_R2', ascending=False, inplace=True)
     
    #print("\n 4 \n")
    
    # Elastic Net
    
    elastic_net = ElasticNetCV(alphas = [0.0000001,0.00001,0.0001, 0.0003, 0.0006, 0.001, 0.003, 0.006, 0.01, 0.03, 0.06, 0.1, 
                              0.3, 0.6, 1], 
                    max_iter = 50000, cv = 3)
    elastic_net.fit(X_train, y_train)
    alpha = elastic_net.alpha_
    #print("Coarse Tuned Alpha :", alpha)
    
    elastic_net = ElasticNetCV(alphas = [alpha * .6, alpha * .65, alpha * .7, alpha * .75, alpha * .8, 
                              alpha * .85, alpha * .9, alpha * .95, alpha, alpha * 1.05, 
                              alpha * 1.1, alpha * 1.15, alpha * 1.25, alpha * 1.3, alpha * 1.35, 
                              alpha * 1.4], 
                    max_iter = 50000, cv = 3)
    elastic_net.fit(X_train, y_train)
    alpha = elastic_net.alpha_
    l1_ratio = elastic_net.l1_ratio_ 
    #print("Fine Tuned Alpha :", alpha)
    #print("L1 :", l1_ratio)
    
    y_train_elastic_net= elastic_net.predict(X_train)
    #y_val_elastic_net = elastic_net.predict(X_val)
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        y_val_en = elastic_net.predict(X_val)
    
    
    #print("\n 5 \n")
    sns.set_style('whitegrid')
    # Plot residuals
    plt.figure(figsize=(10,9))
    plt.scatter(y_train_elastic_net, y_train_elastic_net - y_train, c = "darkred", marker = "*", alpha=0.5, label = "Training data")
    
    limits_list = y_train_elastic_net
    #print(8)
    
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_en, y_val_en - y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_elastic_net, y_val_en))
    
    #print(9)    
    lineStart = limits_list.min()-1
    lineEnd = limits_list.max()+1
      
    plt.title("Elastic Net Residuals",fontsize=16)
    plt.xlabel("Predicted values",fontsize=10)
    plt.ylabel("Residuals",fontsize=10)
    plt.legend(loc = "upper left",fontsize=10)
        
    plt.plot([lineStart,lineEnd],[0,0], color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.savefig('./www/Plot_EN_Residuals.jpg', bbox_inches = 'tight',dpi=200)

    
    # Plot predictions
    plt.figure(figsize=(10,9))
    
    limits_list = y_train_elastic_net
    
    plt.scatter(y_train_elastic_net, y_train, c = "darkred", alpha=0.5, marker = "*", label = "Training data")
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_en, y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_elastic_net, y_val_en))

    lineStart = limits_list.min()-1
    lineEnd =limits_list.max()+1
    
    plt.title("Elastic Net - Real v/s Predicted",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Real values",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    
    plt.plot([lineStart,lineEnd],[lineStart,lineEnd] , color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.ylim(lineStart,lineEnd)
    plt.savefig('./www/Plot_EN_Predicted.jpg', bbox_inches = 'tight',dpi=200)
    
    # Plot important coefficients
    coefs = pd.Series(elastic_net.coef_, index = X_train.columns)
    #print("Elastic Net picked " + str(sum(coefs != 0)) + " features and eliminated the other " +  \
          #str(sum(coefs == 0)) + " features")
    imp_coefs = pd.concat([coefs.sort_values().head(10),
                         coefs.sort_values().tail(10)])
    imp_coefs.plot(kind = "barh")
    plt.title("Coefficients in the Elastic Net Model")
    plt.show()
    
    coefs = pd.DataFrame(coefs)
    #coefs = coefs[abs(coefs[0])>=0.01]
    coefs = coefs[coefs[0]!=0]
    coefs.sort_values(by=0,ascending=False)
    coefs.reset_index(inplace=True)
    coefs.columns = ['Feature','Importance']
    coefs.sort_values(by='Importance',ascending=False,inplace=True)
    coefs = coefs.append(pd.DataFrame({'Feature': 'Intercept','Importance':elastic_net.intercept_},index=[0]))
    coefs['Importance'] = coefs['Importance'].apply(lambda x: '%.2e' % Decimal(str(x)))    

    topEN = coefs
    

    # from sklearn.metrics import r2_score
    # print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_elastic_net)*100,4))))
    # 
    # from sklearn.metrics import r2_score
    # print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_elastic_net,squared=False),4))))
   
    r2_EN = round(r2_score(y_train,y_train_elastic_net)*100,2)
    rmse_EN = round(mean_squared_error(y_train,y_train_elastic_net,squared=False),2)
    
    if (valFlag==False) | (data_mat.Validation.nunique()==1) :
        
        tempResults = pd.DataFrame({'Algorithm':['MLR + Elastic Net'], 'RMSE': [rmse_EN],'R2': [r2_EN] })
        results = pd.concat([results, tempResults])
        results = results[['Algorithm','R2','RMSE']]
        results.sort_values(by='R2', ascending=False, inplace=True)
        
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        r2_EN_val = round(r2_score(y_val,y_val_en)*100,2)
        rmse_EN_val = round(mean_squared_error(y_val,y_val_en,squared=False),2)
          
        tempResults = pd.DataFrame({'Algorithm':['MLR + Elastic Net'], 'Train_RMSE': [rmse_EN],'Validation_RMSE': [rmse_EN_val],'Train_R2': [r2_EN],'Validation_R2': [r2_EN_val]})
        results = pd.concat([results, tempResults])
        results = results[['Algorithm', 'Train_RMSE', 'Validation_RMSE', 'Train_R2','Validation_R2']]
        results.sort_values(by='Train_R2', ascending=False, inplace=True)
    
    # Ridge
    #print("\n 6 \n")
    ridge = RidgeCV(alphas = [0.01, 0.03, 0.06, 0.1, 0.3, 0.6, 1, 3, 6, 10, 30, 60])
    ridge.fit(X_train, y_train)
    alpha = ridge.alpha_
    #print("Coarse Tuned Alpha :", alpha)
    
    ridge = RidgeCV(alphas = [alpha * .6, alpha * .65, alpha * .7, alpha * .75, alpha * .8, alpha * .85, 
                              alpha * .9, alpha * .95, alpha, alpha * 1.05, alpha * 1.1, alpha * 1.15,
                              alpha * 1.25, alpha * 1.3, alpha * 1.35, alpha * 1.4], 
                    cv = 3)
                    
    ridge.fit(X_train, y_train)
    alpha = ridge.alpha_
    #print("Fine Tuned Alpha :", alpha)

    y_train_rdg = ridge.predict(X_train)
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        y_val_rdg = ridge.predict(X_val)

    sns.set_style('whitegrid')
    # Plot residuals
    plt.figure(figsize=(10,9))
    plt.scatter(y_train_rdg, y_train_rdg - y_train, c = "darkred", marker = "*", alpha=0.5, label = "Training data")
    
    limits_list = y_train_rdg
    #print("\n 8 \n")
    
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_rdg, y_val_rdg - y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_rdg, y_val_rdg))
    
    #print(9)    
    lineStart = limits_list.min()-1
    lineEnd = limits_list.max()+1
      
    plt.title("Ridge Residuals",fontsize=16)
    plt.xlabel("Predicted values",fontsize=10)
    plt.ylabel("Residuals",fontsize=10)
    plt.legend(loc = "upper left",fontsize=10)
        
    plt.plot([lineStart,lineEnd],[0,0], color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.savefig('./www/Plot_RG_Residuals.jpg', bbox_inches = 'tight',dpi=200)

    
    # Plot predictions
    plt.figure(figsize=(10,9))
    
    limits_list = y_train_rdg
    
    plt.scatter(y_train_rdg, y_train, c = "darkred", alpha=0.5, marker = "*", label = "Training data")
    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        plt.scatter(y_val_rdg, y_val, c = "darkblue", alpha=0.5, label = "Validation data")
        limits_list = np.concatenate((y_train_rdg, y_val_rdg))

    lineStart = limits_list.min()-1
    lineEnd =limits_list.max()+1
    
    plt.title("Ridge - Real v/s Predicted",fontsize=16)
    plt.xlabel("Predicted values",fontsize=14)
    plt.ylabel("Real values",fontsize=14)
    plt.legend(loc = "upper left",fontsize=14)
    
    plt.plot([lineStart,lineEnd],[lineStart,lineEnd] , color='orange',marker = 'o')
    plt.xlim(lineStart,lineEnd)
    plt.ylim(lineStart,lineEnd)
    plt.savefig('./www/Plot_RG_Predicted.jpg', bbox_inches = 'tight',dpi=200)
    #print("\n 9 \n")

    # Plot important coefficients
    coefs = pd.Series(ridge.coef_, index = X_train.columns)
    #print("Ridge picked " + str(sum(coefs != 0)) + " features and eliminated the other " +  \
          #str(sum(coefs == 0)) + " features")
    imp_coefs = pd.concat([coefs.sort_values().head(10),
                         coefs.sort_values().tail(10)])
    imp_coefs.plot(kind = "barh")
    plt.title("Coefficients in the Ridge Model")
    plt.show()
    
    coefs = pd.DataFrame(coefs)
    #coefs = coefs[abs(coefs[0])>=0.01]
    coefs.sort_values(by=0,ascending=False)
    coefs.reset_index(inplace=True)
    coefs.columns = ['Feature','Importance']
    coefs = coefs.append(pd.DataFrame({'Feature': 'Intercept','Importance':ridge.intercept_},index=[0]))
    coefs['Importance'] = coefs['Importance'].apply(lambda x: '%.2e' % Decimal(str(x)))    
    
    topRG = coefs
    
    from sklearn.metrics import r2_score
    #print("Our model gave {0} r2 on Train Data".format((round(r2_score(y_train,y_train_rdg)*100,4))))
   
    from sklearn.metrics import r2_score
    #print("Our model gave {0} RMSE on Train Data".format((round(mean_squared_error(y_train,y_train_rdg,squared=False),4))))
   
    r2_RG = round(r2_score(y_train,y_train_rdg)*100,2)
    rmse_RG = round(mean_squared_error(y_train,y_train_rdg,squared=False),2)
    
    if (valFlag==False) | (data_mat.Validation.nunique()==1) :
        
        tempResults = pd.DataFrame({'Algorithm':['MLR + Ridge'], 'RMSE': [rmse_RG],'R2': [r2_RG] })
        results = pd.concat([results, tempResults])
        results = results[['Algorithm','R2','RMSE']]
        results.sort_values(by='R2', ascending=False, inplace=True)

    if (valFlag==True) & (data_mat.Validation.nunique()!=1):
        
        r2_RG_val = round(r2_score(y_val,y_val_rdg)*100,2)
        rmse_RG_val = round(mean_squared_error(y_val,y_val_rdg,squared=False),2)
          
        tempResults = pd.DataFrame({'Algorithm':['MLR + Ridge'], 'Train_RMSE': [rmse_RG],'Validation_RMSE': [rmse_RG_val],'Train_R2': [r2_RG],'Validation_R2': [r2_RG_val]})
        results = pd.concat([results, tempResults])
        results = results[['Algorithm', 'Train_RMSE', 'Validation_RMSE', 'Train_R2','Validation_R2']]
        results.sort_values(by='Train_R2', ascending=False, inplace=True)
    #print("\n 8 \n")

    return results,topMLR,topLASSO,topEN,topRG
#-------------------------------------------------------------------------------
