#impot required libraries
library(tidyr)
library(dplyr)
library(stringr)
library(caTools)
library(MASS)
library(caret)
library(ggplot2)
library(cowplot)
library(car)


#load the data given 

emp_survey <- read.csv('employee_survey_data.csv', stringsAsFactors = F)
mngr_survey <- read.csv('manager_survey_data.csv', stringsAsFactors = F)
gen_data <- read.csv('general_data.csv', stringsAsFactors = F)
in_time <- read.csv('in_time.csv', stringsAsFactors = F)
out_time <- read.csv('out_time.csv', stringsAsFactors = F)


###################### DATA QUALITY CHECKS #################################
summary(gen_data)
#check emp_survey dataset
sum(is.na(gen_data)) # We have missing values in the Data
sum(duplicated(gen_data)) # No Duplicate Values

View(emp_survey)
dim(emp_survey)
summary(emp_survey)
sum(is.na(emp_survey)) # We have missing values in the data.

sum(duplicated(emp_survey)) # No duplicate values


#Lets check mngr survey
summary(mngr_survey)
sum(is.na(mngr_survey)) #there is no missing data 
sum(duplicated(mngr_survey)) #no duplicate values present in mngr_survey table


# Convert categorical into factors

gen_data$Attrition <- as.factor(gen_data$Attrition)
gen_data$BusinessTravel <- as.factor(gen_data$BusinessTravel)
gen_data$Department <- as.factor(gen_data$Department)
gen_data$EducationField <- as.factor(gen_data$EducationField)
gen_data$Gender <- as.factor(gen_data$Gender)
gen_data$JobRole <- as.factor(gen_data$JobRole)
gen_data$MaritalStatus <- as.factor(gen_data$MaritalStatus)


#remove over18, StandardHours and employeecount as these has zero variance.
gen_data <- gen_data[!colnames(gen_data) %in%c('Over18','StandardHours','EmployeeCount')]




#Lets work on intime
dim(in_time)
#change column 1 name to employee id in both in_time and out_time file which can be used in combining data 
#in different tables
colnames(in_time)[1] <- 'EmployeeID'
colnames(out_time)[1] <- 'EmployeeID'

#renaming names of column by  removing x from column name so that each column represents date
colnames(in_time) <- str_replace_all(colnames(in_time) , 'X', '')
colnames(out_time) <- str_replace_all(colnames(out_time) , 'X', '')


#file has login time in office 

#head(in_time)
#now we take difference of time from both in and out files
#To get number of hours worked by each employe for every date present in column
EmployeeID <- in_time$EmployeeID
delta_time <- as.data.frame.Date(EmployeeID)
for(i in colnames(in_time)[-1]){
  temp <-  as.numeric(difftime(as.POSIXct(out_time[,c(i)]),as.POSIXct(in_time[,c(i)]),
                               units ='hours' ))
  delta_time <- cbind(delta_time,temp)
}
colnames(delta_time) <- colnames(in_time)




#Removing holidays from list 
holiday_list <- apply(delta_time[,-1], 2, sum,na.rm=T)

#select all column names whose column sum is zero which represents holiday
#and remove them from data set
holiday_days <- names(holiday_list[holiday_list==0])
delta_time <- delta_time[,!colnames(delta_time) %in% holiday_days]


#counting number of leaves of every employee

number_of_leaves <- apply(delta_time[,-1], 1,function(x){
  return(length(x)-length(na.omit(x)))
})

#total number of hours worked
number_of_working_hours <- apply(delta_time[,-1], 1,sum,na.rm=T)
average_working_hours <- apply(delta_time[,-1], 1,mean,na.rm=T)

new_data <- cbind(EmployeeID,number_of_leaves)
new_data <- cbind(new_data,number_of_working_hours)
new_data <-  cbind(new_data,average_working_hours)

new_data <- as.data.frame(new_data)


# Lets Merge all the dataframes into a master data frame.
# Before merging, quickly check if the ID's are same in all files.

length(unique(gen_data$EmployeeID))               # 4410, confirming EmployeedID is key 
length(unique(emp_survey$EmployeeID))             # 4410, confirming EmployeedID is key
length(unique(new_data$EmployeeID))               # 4410, confirming EmployeedID is key

setdiff(gen_data$EmployeeID,emp_survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(gen_data$EmployeeID,new_data$EmployeeID) #Identical EmployeeID across these datasets



# Lets Merge all the dataframes into a master data frame.
MasterFrame <- merge(gen_data,emp_survey,by='EmployeeID',all=F)
MasterFrame <- merge(MasterFrame,mngr_survey,by='EmployeeID',all=F)
MasterFrame <- merge(MasterFrame,new_data,by='EmployeeID',all=F)

sum(is.na(MasterFrame))/nrow(MasterFrame)*100 
#2.5 Percent of missing Data. We can safely ignore the missing values

MasterData <- na.omit(MasterFrame)

sum(is.na(MasterData)) #0 missing values

################################################# EXPLORATORY DATA ANALYSIS ################################################

# Univariate analysis of Categorical Variables

bar_attr<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                 legend.position="none")

plot_grid(ggplot(MasterData, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_attr,
          ggplot(MasterData, aes(x=Department,fill=Attrition))+ geom_bar()+bar_attr,
          ggplot(MasterData, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_attr,
          ggplot(MasterData, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_attr,
          ggplot(MasterData, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_attr,
          ggplot(MasterData, aes(x=MaritalStatus,fill=Attrition))+ geom_bar(),
          align = "h")  

# Univariate analysis of Ordinal Variables

bar_attr_ordinal <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                          legend.position="none")

plot_grid(ggplot(MasterData, aes(x=Education,fill=Attrition))+ geom_bar()+bar_attr_ordinal,
          ggplot(MasterData, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_attr_ordinal,
          ggplot(MasterData, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_attr_ordinal,
          ggplot(MasterData, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_attr_ordinal,
          ggplot(MasterData, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_attr_ordinal,
          ggplot(MasterData, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(),
          align = "h") 

# Observations from Univariate Analysis:-

# Employees travelled rarely are tend to leave the company
# Attrition in HR looks low and R&D look high
# Life Sciences department shows more attrition and HR at low
# Female associates show less attrition than Male associates
# Employees from HR stay longer than the Sales executives
# Divorced employees are less likely to leave the company
# Associates holding bachelor degrees are more prone to leave and Doctors seem to be committed to the company
# Low work environment is impacting the attrition 
# People with low performance rating are prone to leave the company


##NumCompaniesWorked
ggplot(MasterData,aes(x=factor(NumCompaniesWorked),fill=Attrition))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=0.7),stat="count")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

## JobLevel
ggplot(MasterData,aes(x=JobLevel,fill=Attrition))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=0.7),stat="count")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# PercentSalaryHike
ggplot(MasterData,aes(x=PercentSalaryHike,fill=Attrition))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=0.7),stat="count")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

# JobInvolvement
ggplot(MasterData,aes(x=JobInvolvement,fill=Attrition))+
  geom_bar()+
  geom_text(aes(label=paste(round((..count..)/sum(..count..)*100,1),'%'),vjust=0.7),stat="count")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#################### Continues variable ###########################

Grade_total <- MasterData %>% dplyr::select(Age,MonthlyIncome) %>%
  group_by(Age) %>% 
  summarise(MonthlyIncome = sum(x= MonthlyIncome, na.rm = TRUE))

ggplot(Grade_total, aes(x = Age, y = MonthlyIncome, col = factor(MonthlyIncome))) + 
  geom_point(aes(size=MonthlyIncome)) 


ggplot(MasterData, aes(MonthlyIncome, fill = Age)) +
  geom_histogram(binwidth = 500)+ scale_y_sqrt()
boxplot(MasterData$MonthlyIncome, horizontal=TRUE, main="MonthlyIncome")


plot_grid(ggplot(MasterData, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
          ggplot(MasterData, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "v",ncol = 1)
plot_grid(ggplot(MasterData, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(MasterData, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "v",ncol = 1)

plot_grid(ggplot(MasterData, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(MasterData, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "v",ncol = 1)

plot_grid(ggplot(MasterData, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
          ggplot(MasterData, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip(), 
          align = "H",ncol = 1)


#################### Multivariate Analysis ##############################

plot_grid(ggplot(MasterData, aes(x=Attrition, y=MonthlyIncome, fill=Attrition)) +
            geom_boxplot() + coord_flip())

plot_grid(ggplot(MasterData, aes(x=Attrition, y=YearsAtCompany, fill=Attrition)) +
            geom_boxplot() + coord_flip())

plot_grid(ggplot(MasterData, aes(x=Attrition, y=DistanceFromHome, fill=Attrition)) +
            geom_boxplot() + coord_flip())

plot_grid(ggplot(MasterData, aes(x=Attrition, y=Age, fill=Attrition)) +
            geom_boxplot() + coord_flip())


plot_grid(ggplot(MasterData, aes(x=Attrition, y=average_working_hours, fill=Attrition)) +
            geom_boxplot() + coord_flip())





############################################################################################################################
############################################################################################################################

#                                  PRE PROCESSING FOR DATA MODELLING                                                       #

############################################################################################################################
############################################################################################################################


#Remove Employee id column for modelling 
MasterData <- MasterData[,!(names(MasterData) %in% c('EmployeeID'))]
str(MasterData)

# Handling Outliers
rm_outliers <- function(x){
  Intquart <- IQR(x)
  lowerquantile <- unname(quantile(x,0.25))
  upperrquantile <- unname(quantile(x,0.75))
  x <- replace(x,which(x > (upperrquantile + (Intquart * 1.5))),(upperrquantile + (Intquart * 1.5)))
  x <- replace(x,which(x < (lowerquantile - (Intquart * 1.5))),(lowerquantile - (Intquart * 1.5)))
  return(x)
}


MasterData$Age <- rm_outliers(MasterData$Age)
MasterData$DistanceFromHome <- rm_outliers(MasterData$DistanceFromHome)
MasterData$MonthlyIncome <- rm_outliers(MasterData$MonthlyIncome)
MasterData$NumCompaniesWorked <- rm_outliers(MasterData$NumCompaniesWorked)
MasterData$PercentSalaryHike <- rm_outliers(MasterData$PercentSalaryHike)
MasterData$TotalWorkingYears <- rm_outliers(MasterData$TotalWorkingYears)
MasterData$TrainingTimesLastYear <- rm_outliers(MasterData$TrainingTimesLastYear)
MasterData$YearsAtCompany <- rm_outliers(MasterData$YearsAtCompany)
MasterData$YearsSinceLastPromotion <- rm_outliers(MasterData$YearsSinceLastPromotion)
MasterData$YearsWithCurrManager <- rm_outliers(MasterData$YearsWithCurrManager)
MasterData$number_of_leaves <- rm_outliers(MasterData$number_of_leaves)
MasterData$average_working_hours <- rm_outliers(MasterData$average_working_hours)
MasterData$number_of_working_hours <- rm_outliers(MasterData$number_of_working_hours)


# Scale the Integer variables
MasterData$Age <- scale(MasterData$Age)
MasterData$DistanceFromHome <- scale(MasterData$DistanceFromHome)
MasterData$MonthlyIncome <- scale(MasterData$MonthlyIncome)
MasterData$NumCompaniesWorked <- scale(MasterData$NumCompaniesWorked)
MasterData$PercentSalaryHike <- scale(MasterData$PercentSalaryHike)
MasterData$TotalWorkingYears <- scale(MasterData$TotalWorkingYears)
MasterData$TrainingTimesLastYear <- scale(MasterData$TrainingTimesLastYear)
MasterData$YearsAtCompany <- scale(MasterData$YearsAtCompany)
MasterData$YearsSinceLastPromotion <- scale(MasterData$YearsSinceLastPromotion)
MasterData$YearsWithCurrManager <- scale(MasterData$YearsWithCurrManager)
MasterData$number_of_leaves <- scale(MasterData$number_of_leaves)
MasterData$number_of_working_hours <- scale(MasterData$number_of_working_hours)
MasterData$average_working_hours <- scale(MasterData$average_working_hours)


# Modifying the factor variables with appropriate label names, so that it becomes easy for model explanation.
MasterData$Education <- gsub(1,'Below College',MasterData$Education)
MasterData$Education <- gsub(2,'College',MasterData$Education)
MasterData$Education <- gsub(3,'Bachelor',MasterData$Education)
MasterData$Education <- gsub(4,'Master',MasterData$Education)
MasterData$Education <- gsub(5,'Doctor',MasterData$Education)

MasterData$JobInvolvement <- gsub(1,'Low',MasterData$JobInvolvement)
MasterData$JobInvolvement <- gsub(2,'Medium',MasterData$JobInvolvement)
MasterData$JobInvolvement <- gsub(3,'High',MasterData$JobInvolvement)
MasterData$JobInvolvement <- gsub(4,'Very High',MasterData$JobInvolvement)

MasterData$PerformanceRating <- gsub(1,'Low',MasterData$PerformanceRating)
MasterData$PerformanceRating <- gsub(2,'Good',MasterData$PerformanceRating)
MasterData$PerformanceRating <- gsub(3,'Excellent',MasterData$PerformanceRating)
MasterData$PerformanceRating <- gsub(4,'Outstanding',MasterData$PerformanceRating)


MasterData$EnvironmentSatisfaction <- gsub(1,'Low',MasterData$EnvironmentSatisfaction)
MasterData$EnvironmentSatisfaction <- gsub(2,'Medium',MasterData$EnvironmentSatisfaction)
MasterData$EnvironmentSatisfaction <- gsub(3,'High',MasterData$EnvironmentSatisfaction)
MasterData$EnvironmentSatisfaction <- gsub(4,'Very High',MasterData$EnvironmentSatisfaction)


MasterData$JobSatisfaction <- gsub(1,'Low',MasterData$JobSatisfaction)
MasterData$JobSatisfaction <- gsub(2,'Medium',MasterData$JobSatisfaction)
MasterData$JobSatisfaction <- gsub(3,'High',MasterData$JobSatisfaction)
MasterData$JobSatisfaction <- gsub(4,'Very High',MasterData$JobSatisfaction)


MasterData$WorkLifeBalance <- gsub(1,'Bad',MasterData$WorkLifeBalance)
MasterData$WorkLifeBalance <- gsub(2,'Good',MasterData$WorkLifeBalance)
MasterData$WorkLifeBalance <- gsub(3,'Better',MasterData$WorkLifeBalance)
MasterData$WorkLifeBalance <- gsub(4,'Best',MasterData$WorkLifeBalance)






#Dummy Variable creation
dummy <- data.frame(model.matrix(~BusinessTravel,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('BusinessTravel'))],dummy)


dummy <- data.frame(model.matrix(~Department,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('Department'))],dummy)

dummy <- data.frame(model.matrix(~EducationField,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('EducationField'))],dummy)


MasterData$Gender <- as.factor(MasterData$Gender)
levels(MasterData$Gender) <- c(1,0)
MasterData$Gender <- as.numeric(levels(MasterData$Gender))[MasterData$Gender]


dummy <- data.frame(model.matrix(~JobRole,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('JobRole'))],dummy)


dummy <- data.frame(model.matrix(~MaritalStatus,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('MaritalStatus'))],dummy)



dummy <- data.frame(model.matrix(~Education,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('Education'))],dummy)


dummy <- data.frame(model.matrix(~EnvironmentSatisfaction,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('EnvironmentSatisfaction'))],dummy)


dummy <- data.frame(model.matrix(~JobInvolvement,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('JobInvolvement'))],dummy)

dummy <- data.frame(model.matrix(~JobSatisfaction,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('JobSatisfaction'))],dummy)


MasterData$PerformanceRating <- as.factor(MasterData$PerformanceRating)
levels(MasterData$PerformanceRating) <- c(1,0)
MasterData$PerformanceRating <- as.numeric(levels(MasterData$PerformanceRating))[MasterData$PerformanceRating]

dummy <- data.frame(model.matrix(~WorkLifeBalance,data = MasterData))
dummy <- dummy[,-1]
MasterData <- cbind(MasterData[,!(names(MasterData) %in% c('WorkLifeBalance'))],dummy)


MasterData$Attrition <- ifelse(MasterData$Attrition == 'Yes',1,0)

summary(MasterData)
###############################################################################################
###############################################################################################
#set seed to make model reproducable
#split the data into train and test 
set.seed(100)
ind = sample.split(MasterData$Attrition, SplitRatio = 0.7)
train = MasterData[ind,]
test = MasterData[!(ind),]

########################################Modeling ##############################################3

model_1 <- glm(Attrition ~ ., data = train, family = 'binomial')

summary(model_1)
#AIC: 2129.7

#Lets looks at stepwise selection

model_2 <- stepAIC(model_1,direction = 'both')

summary(model_2)


sort(vif(model_2),decreasing = T)

# Lets remove stockoption from the model as it has high p value. Though the EducationFieldLife.Sciences and EducationFieldMedical has high
# VIF, but are highly significant in the present model. We found after running several models, these high collinear variables will be removed.

model_3 <- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + number_of_leaves + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EducationDoctor + EnvironmentSatisfactionLow + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)

summary(model_3)
sort(vif(model_3),decreasing = T)



#Joblevel  has high P value compared to others as high vifs are highly significant 
model_4 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + number_of_leaves + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle + EducationDoctor + EnvironmentSatisfactionLow + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)

summary(model_4)


#EducationDoctor has high P value
model_5 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + number_of_leaves + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director + JobRoleResearch.Scientist + JobRoleSales.Executive + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)


summary(model_5)

#JobRoleResearch.Scientist  hashigh P value 
model_6 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + number_of_leaves + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director  + JobRoleSales.Executive + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)

summary(model_6)


# number_of_leaves has high P value compared to others (0.241307)
model_7 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director  + JobRoleSales.Executive + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 EnvironmentSatisfactionVery.High + JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)


summary(model_7)

# EnvironmentSatisfactionVery.High has high P value  (0.14)
model_8 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 JobRoleResearch.Director  + JobRoleSales.Executive + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)

summary(model_8)


#JobRoleResearch.Directorl has high P value (0.12)
model_9 <-glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager  + 
                number_of_working_hours + BusinessTravelTravel_Frequently + 
                BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                JobRoleSales.Executive + 
                MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                JobSatisfactionLow + JobSatisfactionVery.High + 
                WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
              family = "binomial", data = train)


summary(model_9)
sort(vif(model_9),decreasing = T)

# JobRoleSales.Executive  has P value
model_10 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                  TotalWorkingYears + TrainingTimesLastYear + 
                  YearsSinceLastPromotion + YearsWithCurrManager  + 
                  number_of_working_hours + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                  MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                  JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
                family = "binomial", data = train)


summary(model_10)
sort(vif(model_10),decreasing = T)

#TrainingTimesLastYear has P val
model_11 <-glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree + JobRoleManager + JobRoleManufacturing.Director + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)

summary(model_11)

# Lets continue eliminating variables until we see all the variables are highly significant
# JobRoleManager has high P value compared to others
model_12 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                  TotalWorkingYears  + 
                  YearsSinceLastPromotion + YearsWithCurrManager  + 
                  number_of_working_hours + BusinessTravelTravel_Frequently + 
                  BusinessTravelTravel_Rarely + EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree  + JobRoleManufacturing.Director + 
                  MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                  JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
                family = "binomial", data = train)

summary(model_12)

# BusinessTravelTravel_Rarely has high P value compared to other variables
model_13 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                  TotalWorkingYears  + 
                  YearsSinceLastPromotion + YearsWithCurrManager  + 
                  number_of_working_hours + BusinessTravelTravel_Frequently + 
                  EducationFieldLife.Sciences + 
                  EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                  EducationFieldTechnical.Degree  + JobRoleManufacturing.Director + 
                  MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                  JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
                family = "binomial", data = train)

summary(model_13)
sort(vif(model_13),decreasing = T)

# EducationFieldLife.Sciences as it has high vif
model_14 <-  glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                   TotalWorkingYears  + 
                   YearsSinceLastPromotion + YearsWithCurrManager  + 
                   number_of_working_hours + BusinessTravelTravel_Frequently + 
                   EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + 
                   EducationFieldTechnical.Degree  + JobRoleManufacturing.Director + 
                   MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                   JobSatisfactionLow + JobSatisfactionVery.High + 
                   WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
                 family = "binomial", data = train)



summary(model_14)
sort(vif(model_14),decreasing = T)

#EducationFieldMarketing has high P value compared to others

model_15 <-glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 EducationFieldMedical + EducationFieldOther + 
                 EducationFieldTechnical.Degree  + JobRoleManufacturing.Director + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)

summary(model_15)
sort(vif(model_14),decreasing = T)

#EducationFieldMedical has high P value

model_16 <- glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                  TotalWorkingYears  + 
                  YearsSinceLastPromotion + YearsWithCurrManager  + 
                  number_of_working_hours + BusinessTravelTravel_Frequently + 
                  EducationFieldOther + 
                  EducationFieldTechnical.Degree  + JobRoleManufacturing.Director + 
                  MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                  JobSatisfactionLow + JobSatisfactionVery.High + 
                  WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
                family = "binomial", data = train)

summary(model_16)

# EducationFieldOther has high P value compared to others
model_17 <-glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 EducationFieldTechnical.Degree  + JobRoleManufacturing.Director + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)
summary(model_17)



# EducationFieldTechnical.Degree  has high P value compared to others
model_18 <-glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 JobRoleManufacturing.Director + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)
summary(model_18)

#we can remove more variables relative  to other p values 
#JobRoleManufacturing.Director as it hsa high p values compared to others
model_19 <-glm(formula = Attrition ~ Age  + NumCompaniesWorked + 
                 TotalWorkingYears  + 
                 YearsSinceLastPromotion + YearsWithCurrManager  + 
                 number_of_working_hours + BusinessTravelTravel_Frequently + 
                 MaritalStatusSingle  + EnvironmentSatisfactionLow + 
                 JobSatisfactionLow + JobSatisfactionVery.High + 
                 WorkLifeBalanceBest + WorkLifeBalanceBetter + WorkLifeBalanceGood, 
               family = "binomial", data = train)
summary(model_19)


############### Model Evaluation ##################
#lets predict and check performance of model on test data
#predicted probabilities of Churn 1 for test data
test_pred = predict(model_19, type = "response", 
                    newdata = test[,-2])
summary(test_pred)
test$prob <- test_pred

#Initially lets use 0.5 as cutoff to check accuracy and other scores of the model
test_pred_Attr <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
test_actual_Attr <- factor(ifelse(test$Attrition==1,"Yes","No"))

#confusion matrix 
confusionMatrix(test_pred_Attr,test_actual_Attr, positive = "Yes")
#Accuracy is about 0.86 which is good
#but we get Sensitivity : 0.30144         
#Specificity : 0.97410    
#now our business goal is to predict employee who will leave the company
#rather than predicting more of non churn,for this we need to adjust specificity and Sensitivity

#Lets create a sequence of for all cut of and plot 
#and select optimal cut off based on business needs

x <- seq(0.1,0.8,length.out=10)

sensitivity <- vector(length = 10)

accuracy <- vector(length = 10)

specificity <- vector(length = 10)

for(i in seq(0.1,1,length.out=10)){
  test_pred_Attr <- factor(ifelse(test_pred >= i, "Yes", "No"))
  conf <- confusionMatrix(test_pred_Attr,test_actual_Attr, positive = "Yes")
  sensitivity[i*10] <- conf$byClass[1]
  specificity[i*10] <- conf$byClass[2]
  accuracy[i*10] <- conf$byClass[11]
  
}

analyze_score <- as.data.frame(x)
analyze_score <- cbind(analyze_score,sensitivity)
analyze_score <- cbind(analyze_score,specificity)
analyze_score <- cbind(analyze_score,accuracy)


ggplot(data = analyze_score)+geom_line(mapping = aes(y=sensitivity,x=x,color = 'sensitivity'))+
  geom_line(mapping = aes(y=specificity,x=x,color = 'specificity'))+
  geom_line(mapping = aes(y=accuracy,x=x,color = 'accuracy'))+
  scale_color_manual(values = c(
    'sensitivity' = 'darkblue',
    'specificity' = 'red',
    'accuracy' = 'green')) +
  ylab('score')

#we can see in the plot that specificity increases as we increase cutoff and viseversa for specificity
#and accuracy reaches down to 0.50 
#To have a balanced between specificity and sensitivity we can get value from the intersection
#which is around 0.19


test_pred_Attr <- factor(ifelse(test_pred >= 0.19, "Yes", "No"))
test_actual_Attr <- factor(ifelse(test$Attrition==1,"Yes","No"))


#confusion matrix 
confusionMatrix(test_pred_Attr,test_actual_Attr, positive = "Yes")
#Now we have accuracy of 0.77 with sensitivity and specificity 0.71 and 0.78 respectly which 
#can be good for industrial use


##################################################################################################

##################################################################################################
### KS -statistic - Test Data ######

test_pred_Attr <- ifelse(test_pred_Attr=="Yes",1,0)
test_actual_Attr <- ifelse(test_actual_Attr=="Yes",1,0)

library(ROCR)
#on testing  data
pred_object_test<- prediction(test_pred_Attr, test_actual_Attr)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

# ROC Curve

plot(performance_measures_test,
     colorize=T)
abline(a=0,b=1)


ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
# KS Statistic = 0.4973775

which.max(ks_table_test)
# Top 2 Decile

################################ GAIN AND LIFT CHART #############################################


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attr_decile = lift(test_actual_Attr, test_pred, groups = 10)

plot(Attr_decile$bucket,Attr_decile$Cumlift,type='l',ylab="Cumulative Lift", xlab="Bucket")
plot(Attr_decile$bucket, Attr_decile$Gain, type="l",ylab="Gain", xlab="Bucket")

