# Data Extraction

First, I set the directory and download usefull package.
I import 6 files to have all the test and train data with read.table function.
I do a table() on the subject table to know if I have every subject.

# Naming variables - Step 1

I give basic names to the 4 "subject" and "Y" datasets.
For the 2 "X" datasets, I name the variables with the name of the file "features.txt".

# Filter on measurements on the mean and standard deviation

I take only the variables that we need. The variables which contains "mean" or "std" with the help of the command:
sort(c(grep("mean", var_names), grep("std", var_names)))

# Naming variables - Step 2

I put new name for the variables that I keep because the previous name was really bad, and also not good when you want to use it in some codes.

# Data combining

I simply use the function cbind to get the 2 datasets with test data and the other with train data.
After, I use the function rbind to put combine the 2 datasets and create a unique dataset for every data that we have imported.

# Add variable with name of activities

Using the activity convention name, I create a new variable with an understable name.

# Independent tidy data set with the average of each variable for each activity and each subject

I group by and summarize my dataset in order to have the second tidy dataset.
Finally, I write the table in my folder in txt format.


