# PRODIGY_DS_02
# Task2 
#Perform data cleaning and exploratory data analysis (EDA) on a dataset of your choice, such as the Titanic dataset from Kaggle. 
#Explore the relationships between variables and identify patterns and trends in the data. 

library(readxl)

library(dplyr)

library(ggplot2)

data2 <- read_excel("C:/Users/GD DHANUSH/Documents/Task2_Prod.xlsx")

View(data2)

head(data2)

#Convert the 'Survived' column to numeric: YES = 1, NO = 0
data2 <- data2 %>%
  mutate(Survived_numeric = ifelse(Survived == "YES", 1, 0))

#Calculate the correlation between 'Pclass' and 'Survived_numeric'
correlation <- cor(data2$Pclass, data2$Survived_numeric, use = "complete.obs")

#Print the correlation
print(paste("Correlation between Pclass and Survived:", correlation))

#visualize the relationship - 

ggplot(data2, aes(x = as.factor(Pclass), y = Survived_numeric)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "red") +
  labs(x = "Passenger Class", y = "Survived (1 = Yes, 0 = No)",
       title = "Survival vs. Passenger Class") 
       
#The correlation value is -0.11593791080972. This indicates a weak negative correlation between 
#PClass (Passenger Class) and Survived. A negative correlation means that as the passenger 
#class number increases (moving from 1st class to 3rd class), the likelihood of survival 
#slightly decreases. However, given the small magnitude of the correlation coefficient, the 
#relationship is weak. 


<img width="267" alt="image" src="https://github.com/DhanushCU/PRODIGY_DS_02/assets/159162806/d70f4520-9254-4c96-b0b4-0f4261273026"> 

#The scatter plot with jitter shows the distribution of survival (1 for Yes, 0 for No) 
#Across different passenger classes. The red diamonds represent the mean survival rate for each class.
#Passenger Class 1: The mean survival rate is higher compared to the other classes, indicating that a larger proportion of 1st class passengers survived.
#Passenger Class 2: The mean survival rate is lower than that of the 1st class but higher than that of the 3rd class, showing a moderate likelihood of survival.
#Passenger Class 3: The mean survival rate is the lowest, indicating that 3rd class passengers had the lowest likelihood of survival.
#Summary
#The negative correlation and the visual distribution indicate that higher passenger classes 
#(1st class) had a higher likelihood of survival compared to lower classes (3rd class). 
#Despite the weak correlation, the pattern in the data suggests a trend where passengers in higher 
#classes had better survival outcomes. 


#Summary statistics to know the data better
summary(data2)

#Count of survivors by passenger class

ggplot(data2, aes(x = as.factor(Pclass), fill = Survived)) +
  geom_bar(position = "dodge") +
  labs(x = "Passenger Class", y = "Count", fill = "Survived",
       title = "Survival Count by Passenger Class")
       
#Bar gragh of Survival by PClass
<img width="268" alt="image" src="https://github.com/DhanushCU/PRODIGY_DS_02/assets/159162806/ed8005c8-a974-4d5e-b22c-e4b4373279f4">   


#Age distribution by survival

ggplot(data2, aes(x = Age, fill = Survived)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(x = "Age", y = "Count", fill = "Survived",
       title = "Age Distribution by Survival") 

<img width="276" alt="image" src="https://github.com/DhanushCU/PRODIGY_DS_02/assets/159162806/9c5b4ba3-e387-4a19-b959-eb551b2097e1">  

#Interpretation:The graph you sent shows the distribution of ages of passengers on the Titanic, colored by whether they survived or not. It appears that more children (under 20) did not survive than survived. 
There were also fewer survivors among people in their 40s and 80s than other age groups. Overall, it looks like there were more survivors among those in their 20s, 30s, and 50s. 


#Fare distribution by passenger class ticket -

ggplot(data2, aes(x = as.factor(Pclass), y = Fare, fill = as.factor(Pclass))) +
  geom_boxplot() +
  labs(x = "Passenger Class", y = "Fare", fill = "Passenger Class",
       title = "Fare Distribution by Passenger Class") 

<img width="275" alt="image" src="https://github.com/DhanushCU/PRODIGY_DS_02/assets/159162806/2bd63a92-7ba8-4d3e-a674-ae5a7d6b5e84">

Shows the Tickets of Passenger Class 1 is most expensive, follwed by Class 2 and 3 respectively. 
