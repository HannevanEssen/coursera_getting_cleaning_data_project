# loads the tidyverse library for manipulating the data
library(tidyverse)

# loading all the data files
X_train_data <- read_table("X_train.txt", col_names=FALSE)
X_test_data <- read_table("X_test.txt", col_names=FALSE)
y_train_data <- read_table("y_train.txt", col_names=FALSE)
y_test_data <- read_table("y_test.txt", col_names=FALSE)
subject_train_data <- read_table("subject_train.txt", col_names=FALSE)
subject_test_data <- read_table("subject_test.txt", col_names=FALSE)
activity_labels <- read_table("activity_labels.txt", col_names=FALSE)
features <- read_table("features.txt", col_names=FALSE)

# the features file contains column number and name in one column
# I seperate it into 2 columns and throw away the column number
features <- separate(features, X1, into=c("rownum", "feature"), sep = " ") %>%
            select(-rownum)

# I add the column names to the data
colnames(X_train_data) <- features$feature
colnames(X_test_data) <- features$feature

# I merge the training and test data
X_data <- rbind(X_train_data, X_test_data)
y_data <- rbind(y_train_data, y_test_data)
subject_data <- rbind(subject_train_data, subject_test_data)

# I merge the x and the y data and join it with the activity labels
data <- add_column(X_data, X1 = y_data$X1) %>%
 full_join(activity_labels, by = c("X1"="X1")) 
data <-  add_column(data, activity=data$X2, .before=1) %>%
 select(-X1, -X2) %>%
 add_column(subject=subject_data$X1, .before=1)

column_order_data <- colnames(data)

# I filter out the columns with mean or std in the name, but not the angle columns
# and reorder the columns in their original order
data <- select(data, subject, activity, contains("mean"), contains("std"), -contains("angle("))

k <- keep(column_order_data, function(x) x %in% colnames(data))

data <- data[k]

# I group by subject and activity and calculate the average for each
sum_data <- group_by(data, subject, activity) %>%
            summarise_all(funs(mean))

# I create a tidy dataset with the results
write.table(sum_data, "samsung_data_summary.txt", row.name=FALSE)

