# Churn Prevention in R
The project was part of an exam at university in Statistical Data Analysis. This company gave us 6 datasets about the activity in a smartphone application of thousands of users. We had to analyse the activity, the points system and the access frequency in order to understand if an user was about to uninstall the app and why. The code is in R-Markdown so it's already commented enough. The important steps are:  

1. **Analysis** of the important feature in each dataset (*ANOVA*, *Linear Regression* and *Visualization*);  
2. **Feature engineering** in order to have a unique dataset with each row assigned to a single user and with all important feature for his activity monitoring;    
3. **PCA** to analyse if there are some correlations that can be used to get a ligther dataset;   
4. **Hierarchical clustering** to see if there are some important clusters to analyse;   
5. **Machine learning** to train a model to predict churn from the activity monitoring (**BINARY CLASSIFICATION**).   

For the machine learning part we used 3 model and compared their results with  evaluation metrics and confusion matrices. The 3 model were: *Logistic Regression*, *Classification Tree*, *Random Forest*
