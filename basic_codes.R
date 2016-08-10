#Initial Data Understanding
head(Training.Set.Values)
dim(Training.Set.Values) # [1] 59400    40
Training.Set.Values[Training.Set.Values==""]<- NA

# converting in a date format
Training.Set.Values$date_recorded <- as.Date(Training.Set.Values$date_recorded,format= "%Y-%m-%d")

# converting construction_year into a yyyy-mm-dd format
Training.Set.Values$construction_year= paste(Training.Set.Values$construction_year,"-01-01", sep="")
Training.Set.Values$construction_year <- as.Date(Training.Set.Values$construction_year,format= "%Y-%m-%d")

str(Training.Set.Values)

#creating a new feature called Number.of.days.in.use
Training.Set.Values$Number.of.days.in.use=difftime(Training.Set.Values$date_recorded,Training.Set.Values$construction_year,units = "days")

dim(Training.Set.Values) # [1] 59400    41

#dropping columns with large no. of missing values
Training.Set.Values <- Training.Set.Values[,colSums(is.na(Training.Set.Values))<=(nrow(Training.Set.Values)*0.4)] ### we see that 1 column have been dropped.
dim(Training.Set.Values) # [1] 59400    40

#seperating numeric dataframe
Training.Set.Values_numeric_columns<-Training.Set.Values[,c(1,2,5,10,14,15,18,40)]
dim(Training.Set.Values_numeric_columns) # [1] 59400    8
#seperating character dataframe
Training.Set.Values_character_columns<-Training.Set.Values[,c(1,4,6,9,11:13,16,17,19:21,22,24:39)]
dim(Training.Set.Values_character_columns) # [1] 59400    29

#check quantiles in numeric dataframe
for(i in c(1:8)) 
{
  print(quantile(Training.Set.Values_numeric_columns[,i], probs = seq(0, 1, by= 0.025),na.rm=TRUE))
  print(colnames(Training.Set.Values_numeric_columns)[i])
}

print(quantile(Training.Set.Values_numeric_columns$amount_tsh, probs = seq(0.975, 1, by= 0.0025),na.rm=TRUE))
print(quantile(Training.Set.Values_numeric_columns$num_private, probs = seq(0.975, 1, by= 0.0025),na.rm=TRUE))
print(quantile(Training.Set.Values_numeric_columns$Number.of.days.in.use, probs = seq(0.000, 0.025, by= 0.0025),na.rm=TRUE))
print(quantile(Training.Set.Values_numeric_columns$Number.of.days.in.use, probs = seq(0.65, 0.675, by= 0.0025),na.rm=TRUE))

#capping outliers in Number.of.days.in.use
Training.Set.Values_numeric_columns$Number.of.days.in.use[Training.Set.Values_numeric_columns$Number.of.days.in.use>19372]<-19372
Training.Set.Values_numeric_columns$Number.of.days.in.use[Training.Set.Values_numeric_columns$Number.of.days.in.use<0]<-64

# Removing whiet spaces and converting everything in Training.Set.Values_character_columns to upper case.
#Training.Set.Values_character_columns = as.data.frame(sapply(Training.Set.Values_character_columns, toupper))
#Training.Set.Values_character_columns <- data.frame(lapply(Training.Set.Values_character_columns, trimws))
#for(i in c(1)) # convert column 'id' back to numeric type.
#{
#  Training.Set.Values_character_columns[,i]<-as.numeric(Training.Set.Values_character_columns[,i])
#}
#length(Training.Set.Values_character_columns$id)
#str(Training.Set.Values_character_columns)


#combine character dataframe and Training.Set.Labels
Training.Set.Values_character_columns_combined <- merge(Training.Set.Values_character_columns,Training.Set.Labels,by='id')
dim(Training.Set.Labels)
# The following dim() command isn't working properly. it is showing only 47491 observations whereas till yesterday it showed all 59400 rows. pls help!
dim(Training.Set.Values_character_columns_combined) # [1] 59400    30 |now all of sudden it's 47491   30
View(Training.Set.Values_character_columns_combined)
# I can't do further analysis unless I am able to merge two dataframes correctly. 

# splitting Training.Set.Values_character_columns_combined based on response and replaceing NAs by respective modes.
Training.Set.Values_character_columns_combined_functional=subset(Training.Set.Values_character_columns_combined,Training.Set.Values_character_columns_combined$status_group=='functional',select = c(1:30),drop=TRUE)
Training.Set.Values_character_columns_combined_nonfunctional=subset(Training.Set.Values_character_columns_combined,Training.Set.Values_character_columns_combined$status_group=='non functional',select = c(1:30),drop=TRUE)
Training.Set.Values_character_columns_combined_repair=subset(Training.Set.Values_character_columns_combined,Training.Set.Values_character_columns_combined$status_group=='functional needs repair',select = c(1:30),drop=TRUE)

dim(Training.Set.Values_character_columns_combined_functional) # [1] 32259    30
dim(Training.Set.Values_character_columns_combined_nonfunctional) # [1] 22824    30
dim(Training.Set.Values_character_columns_combined_repair)# [1] 4317   30
32259+22824+4317 # 59400!
25774+18237+3480 #47491

#Defining Mode Function
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

#Replacing Missing values in character columns by their respective modes.
#functional
for(i in c(2:30))
{
  Training.Set.Values_character_columns_combined_functional[,i][is.na(Training.Set.Values_character_columns_combined_functional[,i])]=Mode(subset(Training.Set.Values_character_columns_combined_functional[,i], Training.Set.Values_character_columns_combined_functional[,i] != "NA"))
  
}
for(i in c(2:30))
{
  print(unique(is.na(Training.Set.Values_character_columns_combined_functional[,i])))
  
}

#non-functional
for(i in c(2:30))
{
  Training.Set.Values_character_columns_combined_nonfunctional[,i][is.na(Training.Set.Values_character_columns_combined_nonfunctional[,i])]=Mode(subset(Training.Set.Values_character_columns_combined_nonfunctional[,i], Training.Set.Values_character_columns_combined_nonfunctional[,i] != "NA"))
  
}
for(i in c(2:30))
{
  print(unique(is.na(Training.Set.Values_character_columns_combined_nonfunctional[,i])))
  
}

#repair
for(i in c(2:30))
{
  Training.Set.Values_character_columns_combined_repair[,i][is.na(Training.Set.Values_character_columns_combined_repair[,i])]=Mode(subset(Training.Set.Values_character_columns_combined_repair[,i], Training.Set.Values_character_columns_combined_repair[,i] != "NA"))
  
}
for(i in c(2:30))
{
  print(unique(is.na(Training.Set.Values_character_columns_combined_repair[,i])))
  
}



#Replacing NAs in numeric dataframe by medians of respective columns.
for(i in c(2:8)) 
{
  Training.Set.Values_numeric_columns[,i][is.na(Training.Set.Values_numeric_columns[,i])]=median(Training.Set.Values_numeric_columns[,i], na.rm = TRUE)
}

# Combining Training.Set.Values_numeric_columns with Training.Set.Labels
Training.Set.Values_numeric_columns_combined= merge(Training.Set.Values_numeric_columns,Training.Set.Labels, by="id")
dim(Training.Set.Values_numeric_columns_combined) #[1] 59400     9

#perform ANOVA on numeric dataframe

results=aov(Training.Set.Values_numeric_columns_combined$amount_tsh~Training.Set.Values_numeric_columns_combined$status_group)
summary(results)
results=aov(Training.Set.Values_numeric_columns_combined$gps_height~Training.Set.Values_numeric_columns_combined$status_group)
summary(results)
results=aov(Training.Set.Values_numeric_columns_combined$num_private~Training.Set.Values_numeric_columns_combined$status_group)
summary(results) ## The predictor num_private is not significant
results=aov(Training.Set.Values_numeric_columns_combined$region_code~Training.Set.Values_numeric_columns_combined$status_group)
summary(results)
results=aov(Training.Set.Values_numeric_columns_combined$district_code~Training.Set.Values_numeric_columns_combined$status_group)
summary(results)
results=aov(Training.Set.Values_numeric_columns_combined$population~Training.Set.Values_numeric_columns_combined$status_group)
summary(results)
results=aov(Training.Set.Values_numeric_columns_combined$Number.of.days.in.use~Training.Set.Values_numeric_columns_combined$status_group)
summary(results)


#combining 3 character dataframes to one character dataframe
Training.Set.Values_character_columns_combined_New <- rbind(Training.Set.Values_character_columns_combined_functional,Training.Set.Values_character_columns_combined_nonfunctional,Training.Set.Values_character_columns_combined_repair)
dim(Training.Set.Values_character_columns_combined_New) #[1] 59400    30 | [1] 47491    30
dim(Training.Set.Values_character_columns_combined) # [1] 59400    30 | [1] 47491    30

#performing chi-sq test to character dataframe

for(i in c(2:29))
{
  # tabulating data for chi-sq test
  tab<- table(Training.Set.Values_character_columns_combined_New$status_group,Training.Set.Values_character_columns_combined_New[,i])
  # chi-sq test for categorical variable
  print(chisq.test(tab))
  print(colnames(Training.Set.Values_character_columns_combined_New)[i])
  print(i)
}
warnings()

#Drop insignificant predictor from numeric dataframe
dim(Training.Set.Values_numeric_columns_combined) #[1] 59400     9
Training.Set.Values_numeric_columns_combined=Training.Set.Values_numeric_columns_combined[-c(4)]
dim(Training.Set.Values_numeric_columns_combined) #[1] 59400     8

#Drop insignificant predictor from character dataframe
dim(Training.Set.Values_character_columns_combined_New) # [1] 59400    30
Training.Set.Values_character_columns_combined_New=Training.Set.Values_character_columns_combined_New[-c(2,3,6)]
dim(Training.Set.Values_character_columns_combined_New) # [1] 59400    27
Training.Set.Values_character_columns_combined_New=Training.Set.Values_character_columns_combined_New[-c(27)]
dim(Training.Set.Values_character_columns_combined_New) # [1] 59400    26


# combine numeric and character dataframes.
df_Train= merge(Training.Set.Values_character_columns_combined_New,Training.Set.Values_numeric_columns_combined, by='id')
dim(df_Train) # [1] 59400    33


##############################Perform similar operations on Test dataset.######################################

#Initial Data Understanding
head(Test.Set.Values)
dim(Test.Set.Values) # [1] 14850    40
Test.Set.Values[Test.Set.Values==""]<- NA

# converting in a date format
Test.Set.Values$date_recorded <- as.Date(Test.Set.Values$date_recorded,format= "%Y-%m-%d")

# converting construction_year into a yyyy-mm-dd format
Test.Set.Values$construction_year= paste(Test.Set.Values$construction_year,"-01-01", sep="")
Test.Set.Values$construction_year <- as.Date(Test.Set.Values$construction_year,format= "%Y-%m-%d")

str(Test.Set.Values)

#creating a new feature called Number.of.days.in.use
Test.Set.Values$Number.of.days.in.use=difftime(Test.Set.Values$date_recorded,Test.Set.Values$construction_year,units = "days")

dim(Test.Set.Values) # [1] 14850    41

#dropping columns- scheme_name
Test.Set.Values <- Test.Set.Values[-c(22)]
dim(Test.Set.Values) # [1] 14850    40



#seperating numeric dataframe
Test.Set.Values_numeric_columns<-Test.Set.Values[,c(1,2,5,10,14,15,18,40)]
dim(Test.Set.Values_numeric_columns) # [1] 14850    8
#seperating character dataframe
Test.Set.Values_character_columns<-Test.Set.Values[,c(1,4,6,9,11:13,16,17,19:21,22,24:39)]
dim(Test.Set.Values_character_columns) # [1] 14850    29

#check quantiles in numeric dataframe
for(i in c(1:8)) 
{
  print(quantile(Test.Set.Values_numeric_columns[,i], probs = seq(0, 1, by= 0.025),na.rm=TRUE))
  print(colnames(Test.Set.Values_numeric_columns)[i])
}

print(quantile(Test.Set.Values_numeric_columns$Number.of.days.in.use, probs = seq(0.000, 0.025, by= 0.0025),na.rm=TRUE))
print(quantile(Test.Set.Values_numeric_columns$Number.of.days.in.use, probs = seq(0.625, 0.65, by= 0.0025),na.rm=TRUE))

#capping outliers in Number.of.days.in.use
Test.Set.Values_numeric_columns$Number.of.days.in.use[Test.Set.Values_numeric_columns$Number.of.days.in.use>19383]<-19383
Test.Set.Values_numeric_columns$Number.of.days.in.use[Test.Set.Values_numeric_columns$Number.of.days.in.use<0]<-69


#Defining Mode Function
Mode <- function(x) {
  u <- unique(x)
  u[which.max(tabulate(match(x, u)))]
}

#Replacing Missing values in character columns by their respective modes.
dim(Test.Set.Values_character_columns) # [1] 14850    29
View(Test.Set.Values_character_columns)
for(i in c(2:29))
{
  Test.Set.Values_character_columns[,i][is.na(Test.Set.Values_character_columns[,i])]=Mode(subset(Test.Set.Values_character_columns[,i], Test.Set.Values_character_columns[,i] != "NA"))
  
}
for(i in c(2:29))
{
  print(unique(is.na(Test.Set.Values_character_columns[,i])))
  
}


#Replacing NAs in numeric dataframe by medians of respective columns.
dim(Test.Set.Values_numeric_columns) # [1] 14850     8
for(i in c(2:8)) 
{
  Test.Set.Values_numeric_columns[,i][is.na(Test.Set.Values_numeric_columns[,i])]=median(Test.Set.Values_numeric_columns[,i], na.rm = TRUE)
}


#Drop insignificant predictor from numeric dataframe
View(Test.Set.Values_numeric_columns)
dim(Test.Set.Values_numeric_columns) #[1] 14850     8
Test.Set.Values_numeric_columns=Test.Set.Values_numeric_columns[-c(4)]
dim(Test.Set.Values_numeric_columns) #[1] 14850     7

#Drop insignificant predictor from character dataframe
dim(Test.Set.Values_character_columns) # [1] 14850    29
Test.Set.Values_character_columns=Test.Set.Values_character_columns[-c(2,3,6)]
dim(Test.Set.Values_character_columns) # [1] 59400    26


# combine numeric and character dataframes.
df_Test= merge(Test.Set.Values_character_columns,Test.Set.Values_numeric_columns, by='id')
dim(df_Test) # [1] 14850    32


############Applying simple classification tree.
install.packages("tree")
library(tree)

set.seed(21)
train=sample(1:nrow(df_Train),59400)
train_data=df_Train[train,]
test_data=df_Train[train,]
attach(df_Train)
View(train_data)
tree.training_simple=tree(status_group~.-status_group-id-wpt_name-lga-ward,train_data)
summary(tree.training_simple)
tree.pred.simple=predict(tree.training_simple,test_data,type = "class")
table(tree.pred.simple,test_data$status_group)
# for 45000 observations in training
#tree.pred.simple          functional functional needs repair non functional
#functional                    7091                     775           2593
#functional needs repair          0                       0              0
#non functional                 738                     228           2975

##################### Random Forest
attach(df_Train)
library(randomForest)
bag.tree=randomForest(formula=status_group~.-status_group-id-wpt_name-lga-ward,data = df_Train,mtry=7,importance=TRUE)

dim(df_Train)# [1] 59400    33
dim(df_Test) # [1] 14850    32
dim(train_data)
names(df_Test)
df_Test_1 <- df_Test[ -c(1,2,5,6) ]
dim(df_Test_1) # [1] 14850    27

bag.tree.pred=predict(bag.tree,df_Test_1,type = "class")

#Error in model.frame.default(Terms, newdata, na.action = na.omit) : 
#variable lengths differ (found for 'basin')
#In addition: Warning message:
#  'newdata' had 14850 rows but variables found have 59400 rows 

length(df_Test_1$basin)
length(train_data$basin)
View(train_data)

str(df_Test)
str(df_Test_1)
str(df_Train)
colnames(df_Train)
colnames(df_Test)

table(bag.tree.pred,test_data$status_group)

str(train_data)
View(train_data)
dim(train_data)
for(i in c(32)) # convert column 'Number.of.days.in.use' back to numeric type.
{
  df_Train[,i]<-as.numeric(df_Train[,i])
}

for(i in c(32)) # convert column 'Number.of.days.in.use' back to numeric type.
{
  test_data[,i]<-as.numeric(test_data[,i])
}

for(i in c(32)) # convert column 'Number.of.days.in.use' back to numeric type.
{
  df_Test[,i]<-as.numeric(df_Test[,i])
}


dim(df_Test_1)
View(df_Test_1)
for(i in c(27)) # convert column 'Number.of.days.in.use' back to numeric type.
{
  df_Test_1[,i]<-as.numeric(df_Test_1[,i])
}


#################### Boosting 
install.packages("gbm")
library(gbm)
boost.tree=gbm(status_group~.-id-wpt_name-lga-ward,data = train_data,distribution = "multinomial",n.trees = 5000,interaction.depth = 4,shrinkage = 0.2, verbose = FALSE)
boost.tree.pred=predict(boost.tree,test_data,type="response",n.trees=5000)
View(boost.tree.pred)
p.boost.tree.pred <- apply(boost.tree.pred, 1, which.max)
View(p.boost.tree.pred)
write.csv(p.boost.tree.pred, file = "p_boost_tree_pred.csv")
p_boost_tree_pred_2=as.list(p_boost_tree_pred_1)
table(p_boost_tree_pred_2,test_data$status_group)
print(test_data$status_group)
View(test_data$status_group)
write.csv(test_data$status_group, file = "test_data_status_group.csv")
# calculated accuracy on excel. it's 79.35%

dim(boost.tree.pred)
View(boost.tree.pred)

######################## C5.0##
install.packages("C50")
library(C50)
c50_model= C5.0(df_Train[-c(1,2,5,6,33)],df_Train$status_group, rules = TRUE)
c50_pred=predict(c50_model,df_Test_1,type = "class")
#Error in predict.C5.0(c50_model, df_Test_1, type = "class") : 
#either a tree or rules must be provided


