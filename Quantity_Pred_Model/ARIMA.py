'''*************************************
import all the libraries required
'''
import os
import matplotlib.pyplot as plt
import pandas as pd
#import numpy as np
#from statsmodels.tsa.arima_model import ARIMA
from sklearn.metrics import mean_squared_error
import time
import statsmodels.api as sm



'''*************************************
Contants needed for the program
'''
data_path = '/home/jeff/AI_Finance_book/2_1_seasonality/'
os.chdir(data_path)
list_flds = []
fld_name =''
index_col_name= 'Month'
file_path_in = os.path.join(data_path,'Import_file_names.txt')
file_path_out = os.path.join(data_path,'output_dataset.txt')
f_name = pd.read_csv(file_path_in,sep='\t')

##default to optimize the ARIMA parameters by trying out all the combinations
parameter_op = True


'''*************************************
    this loop import all data files, with dataframe name specified in the import_file_names specified
    input: raw data files in csv format, header starting at line 4; and import_file_names.txt
    output: dataframe list that store all dataframe names
'''
def preprocess_data(f_name,list_flds):

    #for each file
    for index, row in f_name.iterrows():

        #execute the code in a dynamic manner
        #after execution of the code, the variables values are stored in namespeace
        ##read in file name and the dataframe name
        print(row['pd_name']+':************')
        namespace={}
        print('curr_pd_name = "'+row['pd_name']+'"')
        exec('curr_pd_name = "'+row['pd_name']+'"',namespace)
        print('curr_filename = "'+row['filename']+'"')
        exec('curr_filename = "'+row['filename']+'"',namespace)
        curr_filename = namespace['curr_filename']
        curr_pd_name = namespace['curr_pd_name']

        ##Read in the actual file
        curr_pd = pd.read_csv(curr_filename+'.csv', header=4)
        list_flds.append(curr_pd_name)#collection of pd names for display of series later
        fld_name = list(curr_pd)[1]
        #convert the value columns to numeric, not string
        curr_pd[fld_name]=pd.to_numeric(curr_pd[fld_name],errors='coerce')
        #rename the column with values to be the dataframe name
        curr_pd=curr_pd.rename(index=str,columns={fld_name:curr_pd_name})

        #convert the month column to datetime format
        #convert index to datetime
        curr_pd[index_col_name]= pd.to_datetime(curr_pd[index_col_name], format="%b %Y")

        #start building up the consolidated dataframe that includes all dataframes
        if index == 0:
            df_all = curr_pd
        else:
            df_all = pd.merge(df_all,curr_pd,how='outer',left_on =index_col_name,right_on=index_col_name)
        print(curr_pd_name+' done!')

    ##convert text to date then reorder by date
    df_all[index_col_name]= pd.to_datetime(df_all[index_col_name], format="%b %Y")
    df_all = df_all.sort_values(by=[index_col_name])

    ##handle missing values -> interpolate
    for fld in list_flds:
        print(fld)
        #default_missing_val = 0.0
        for index, row in df_all.iterrows():
            df_all[fld] = df_all[fld].interpolate()

    ##Output dataset
    df_all.to_csv(file_path_out,sep='\t',index=False)

    return list_flds


'''*************************************
this function load data from a file path and create a monthly time index for the dataframe
'''
def load_data(file_path_out,fld_name,idx_start,idx_end):


    df_all = pd.read_csv(file_path_out,sep='\t')

    ##Using Statsmodels...
    dta = df_all[[index_col_name,fld_name]]

    # seasonal difference
    #months_in_year = 12
    #dta_series = pd.Series(dta[fld_name])
    dta_df = dta[fld_name]
    dta_df.index = pd.DatetimeIndex(start=idx_start, end=idx_end, freq='MS')
    return dta_df


'''*************************************
    this function produce the ARIMA forecast and error rate
    input:
        1) dataframe that contains the data series
        2) show- boolean if True shows the prediction and actual visually
        3) parameter order: (p,d,q)
            p is the number of autoregressive terms - lagged term
            d is the number of nonseasonal differences needed for stationarity, and ---1=first derivative; 2=second derivatives; 3=third derivatives
            q is the number of lagged forecast errors in the prediction equation.e.g. moving average
        4) training and testing dataset split
    output:
         error of the fitted parameters in ARIMA model - per testing against testing data
'''
def forecast(dta_df,show,para_order=(1,1,1),train_ratio=0.70):
    '''****************
        Step 3: Build the Model
        Step 4: Make Prediction and measure accuracy
        ****************
    
    
        ##Step 3: Build the Model
        ##prepare training and testing set
        ##the first 70% be training set, the remaining 30% becomes testing set
    '''

    size = int(len(dta_df)*train_ratio)
    train_df = dta_df[:size]
    test_df = dta_df[size+1:]

    ##p,d,q
    try:
        res=sm.tsa.ARIMA(train_df,order=para_order).fit()
    ##when the parameters do not make sense
    except Exception:
        return 999999
    predictions=[]
    test_outcome=[]
    len_test_df = len(test_df)

    ##convert the testing set to a list for result comparision
    test_outcome = test_df.values.tolist()

    ##Step 4: Make Prediction and measure accuracy
    ##working to see how to make prediction on ARIMA --- 1 = 1st record after training set
    output = res.forecast(len_test_df)
    predictions= output[0]

    ##make prediction as data series with index same as test_df
    prediction_df = pd.Series(predictions)
    prediction_df.index = test_df.index

    ##combine both actual and predction of test data into data
    data = pd.concat([test_df,prediction_df], axis=1)
    data_name = list(data)[0]
    data.columns=['actual','predicted']

    try:
        error = mean_squared_error(test_outcome, predictions)
    except Exception: #when the error is too large to be stored in the variable
        print("error too large")
        return 999999
    print('Test MSE: %.3f' % error)

    if show:
        data.actual.plot(color='blue',grid=True,label='actual',title=data_name)
        data.predicted.plot(color='red',grid=True,label='predicted')
        plt.legend()
        plt.show()
    return error
'''*************************************MAIN*************************************

Step 1: Download & Import Data


Trying out the parameters
For better style, we can even seperate this part of program to another file
'''


'''****************
Step 2: Pre-processing the data
****************
'''
#import all the files and generate a consolidated data file at file_path_out
list_flds = preprocess_data(f_name,list_flds)

#this stores the optimized parameters of each field
flds_para_dict={}
#looping through list of fields
for fld in list_flds:
    dta_df=load_data(file_path_out,fld,'2001-01-01','2018-01-01')

    if parameter_op: #if needed parameters optimization, step 5 is required
        '''****************
        Step 3: Build the Model
        Step 4: Make Prediction and measure accuracy
        Step 5: Further improvement of model – Fine tuning the parameters
        ****************
        '''
        start = time.time()
        lowest_MSE=999999
        lowest_order = (0,0,0)
        for p_para in range(13):
            for d_para in range(3):
                for q_para in range(4):
                    order = (p_para,d_para,q_para)
                    print(order)
                    error = forecast(dta_df,False,order)
                    ##Step 5: Further improvement of model – Fine tuning the parameters
                    if error<lowest_MSE:
                        lowest_MSE=error
                        lowest_order = order
        end = time.time()
        print("Best para is")
        print(lowest_order)
        print('Test MSE: %.3f' % lowest_MSE)
        total_time = (end-start)
        print('it takes %.3f s to compute' % total_time)
        flds_para_dict[fld] = lowest_order
    else: #if no need for paramters optimization, step 5 is skipped
        '''****************
        Step 3: Build the Model and Step 4: Make Prediction and measure accuracy
        Step 4: Make Prediction and measure accuracy
        ****************
        '''
        error = forecast(dta_df,True,(7,1,2))
