library(data.table)
library(ggplot2)

################################################### Data/ Data Cleaning/ Transformation ########################################
df <- fread('BankChurners.csv')
df

str(df)
summary(df)

#factoring Categorial Data 
df$CLIENTNUM <- as.factor(df$CLIENTNUM)
df$Attrition_Flag <- as.factor(df$Attrition_Flag)
df$Gender         <- as.factor(df$Gender)
df$Education_Level <- as.factor(df$Education_Level)
df$Marital_Status <- as.factor(df$Marital_Status)
df$Income_Category <- as.factor(df$Income_Category)
df$Card_Category <- as.factor(df$Card_Category)

#drop last two columns due to relevancy 
df <- df[,-(22:23)]
str(df)

################################################### Exploratory Data Analysis #################################################

#Percentage of Attrition Flag based on Gender
a <- df[,list(numPeps = .N), by = .(Gender, Attrition_Flag)]
a
ggplot(a, aes("", numPeps, fill = Attrition_Flag)) + geom_bar(stat = 'identity') + coord_polar(theta = 'y', start = 0) +
  labs(title = 'Attrition based on Gender', x = '', y = '', fill = 'Gender') + facet_wrap(.~Gender) + theme_void()+
  theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label = paste(numPeps/100,"%")), 
                                                            size = 5, position = position_stack(vjust = 0.5))

#Perecntage of Clients by Gender
b<- df[,.N, by = Gender]
b 
ggplot(b, aes(" ", N, fill = Gender)) + geom_bar(stat = 'identity') + coord_polar(theta = 'y', start = 0) + 
  theme_void()+ geom_text(aes(label = paste(N/100,"%")), size = 5, position = position_stack(vjust = 0.5))+ 
  labs( title = 'Percentage of Clients by Gender', y = '') + theme(plot.title = element_text(hjust = 0.5))

#What's the dstribution of Clients by Education level
c <- df[,list(numPeps = .N), by = .(Education_Level, Gender)]
c
ggplot(c, aes(x = factor(Education_Level), y = numPeps, fill = Gender)) + 
  geom_bar(stat = 'identity', position = 'dodge') + theme(plot.title = element_text(hjust = 0.5)) +
  labs( x = 'Education', y = 'Number of Clients', title = 'Education level  by Gender', fill = 'Gender') 

#Percentage of Customers by Martical Status, Education level and Gender
d <- df[, list(.N), by = .(Marital_Status, Education_Level, Gender)]
d
ggplot(d, aes(factor(Education_Level), N, fill = Marital_Status)) + geom_bar( stat = 'identity', position = 'dodge') +
  facet_wrap(.~Gender) + labs(x = 'Education', y = 'Number of Clients', fill = 'Martial Status',
                              title = 'Gender by Education Level and Marital Status') +
  theme(plot.title = element_text(hjust = 0.5))

#Average income by Age Group 
#14-25 --> Trailing Millennials
#26-31 --> Leading Millennials
#32-48 --> Generation X 
#49-67 --> Baby Boomers
#68+ --> Senior Citizens 

df[,Age_Group := fcase(between(Customer_Age, 14, 25), 'Trailing Millennials',
                       between(Customer_Age, 26, 31), 'Leading Millennials',
                       between(Customer_Age, 32, 48), 'Generation X',
                       between(Customer_Age, 42, 67), 'Baby Boomers',
                       between(Customer_Age, 68, 1000), 'Senior Citizens ',
                       default = 'Group Unkown')]
#Income Category Age Group
e <- df[, list(.N), by = .(Age_Group, Income_Category)]
e
ggplot(e, aes(factor(Income_Category), N, fill = Age_Group)) + geom_bar(stat = 'identity', position = 'dodge') + 
  scale_y_log10() + labs(x = 'Income', y = "Clients", fill = 'Age Group', title = 'Income by Age Groups') + 
  theme(plot.title = element_text(hjust = 0.5))

#Income Categroy by Education level 

f <- df[, list(.N), by = .(Education_Level, Income_Category, Gender)]
f
ggplot(f, aes(factor(Income_Category), N, fill = Education_Level)) + geom_bar(stat = 'identity') +
  scale_y_log10() + facet_wrap(~Gender) + labs( x = 'Income', y = 'Clients', fill = 'Education Level',
                                                title = 'Income Level by Education based on Gender') +
  theme(plot.title = element_text(hjust = 0.5))

#Income Category by Martial Status
g <- df[, list(.N), by = .(Income_Category, Marital_Status)]
g
ggplot(g, aes(factor(Income_Category),N, fill = Marital_Status)) + geom_bar(stat = 'identity', position = 'dodge') +
  scale_y_log10() + theme(plot.title = element_text(hjust = 0.5)) + labs(x = 'Income', y = 'Clients ', fill = 'Martial Status',
                                                                         title = 'Income Catergory by Marital Status ')

#Age Group vs Attrition 
h <- df[, list(numPeps = .N), by= .(Age_Group,Attrition_Flag )]
h
ggplot(h, aes(factor(Age_Group), numPeps, fill = Attrition_Flag)) + geom_bar(stat = 'identity', position = 'dodge') + 
  scale_y_log10() + labs( title = 'Attrition on each Age Level', x = 'Age Group', y = 'Clients', fill = 'Attrition') +
  theme(plot.title = element_text(hjust = 0.5))

#Overall distribution of Age
ggplot(df[, .N, Customer_Age], aes(Customer_Age,N)) + geom_histogram(stat = 'identity', binwidth = 10) + 
  labs(title = 'Age Distribution', x = 'Age', y = 'Clients') + theme(plot.title = element_text(hjust = 0.5))

#Age Distribution of Attrition 
ggplot(df[,.(Customer_Age,Attrition_Flag)], aes(Attrition_Flag, Customer_Age, fill = Attrition_Flag)) + geom_boxplot() +
  labs(title = 'Age Stastics by Attrition', x = '', y = 'Summary', fill = 'Attrition') + 
  theme(plot.title = element_text(hjust = 0.5))

#Number of dependents on card by Attrition Level
i <- df[,.N, by = .(Dependent_count, Attrition_Flag, Gender)][order(Dependent_count)]
i
ggplot(i, aes(factor(Dependent_count),N, fill = Attrition_Flag)) + geom_bar( stat = 'identity', position = 'dodge') +
  facet_wrap(~Gender) + labs( title = 'Distribution of dependent count based on Gender', x = 'Dependents',y = 'Number of People', 
                              fill = 'Attrition') + theme(plot.title = element_text(hjust = 0.5))

#Precentage of Education 
ggplot(df[, .N, Education_Level], aes('',N, fill = Education_Level)) + geom_bar(stat = 'identity') + 
  coord_polar(theta = 'y',start = 0) + theme_void() + scale_fill_brewer(palette = 'Dark2') + 
  geom_text(aes(label = paste(N/100,"%")), size = 5, position = position_stack(vjust = 0.5)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Precentage of Clients based on Education', x = "", y = '', fill = 'Education Level') 

#Percentage of Card Users 
ggplot(df[, .N, Card_Category], aes('',N, fill = Card_Category)) + geom_bar(stat = 'identity') + 
  coord_polar(theta = 'y',start = 0) + theme_void() + scale_fill_brewer(palette = 'BrBG') + 
  geom_text(aes(label = paste(N/100,"%")), size = 5, position = position_stack(vjust = 0.5)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Precentage of Clients based on Card Category', x = "", y = '', fill = 'Card Type') 


#Distribution of Bank Relationship  
ggplot(df[,.N,Months_on_book], aes(factor(Months_on_book),N)) + theme(plot.title = element_text(hjust = 0.5)) +
  geom_histogram(stat = 'identity', color = 'darkblue',fill = 'lightblue') + 
   + labs( title = 'Distribution of Bank Relation', x = 'Relations') 

#Distribution of Total Relation
ggplot(df[,.N, Total_Relationship_Count], aes(factor(Total_Relationship_Count),N, fill = factor(Total_Relationship_Count))) + 
  geom_bar(stat = 'identity',position = 'dodge') + scale_fill_brewer(palette = 'RdBu') + 
  theme(plot.title = element_text(hjust = 0.5)) + labs(title = 'Total Number of Products with Bank', 
                                                       x = "", y = '', fill = 'Production') 

#Relationship has a ranking system. We can factorize it 
df$Total_Relationship_Count <- as.factor(df$Total_Relationship_Count)

#Card Inactivity distribution 
ggplot(df[,.N, Months_Inactive_12_mon], aes(factor(Months_Inactive_12_mon),N, fill = factor(Months_Inactive_12_mon))) + 
  geom_bar(stat = 'identity',position = 'dodge') + scale_fill_brewer(palette = 'RdBu') + scale_y_log10() +
  theme(plot.title = element_text(hjust = 0.5)) + labs(title = 'Total Number of Products with Bank', 
                                                       x = "", y = '', fill = 'Inactivity Range') 


###################################################  Modeling ########################################################### 
library(caret)

set.seed(1000)
nsize <- floor(0.80 * nrow(df)) 
id <- sample(seq_len(nrow(df)), size = nsize)

df[,CLIENTNUM := NULL]

###  Linear Regression ####
# How varaibles can I dentify the Average Card Utilization Ratio(Generate Probabilities)
trainDT <- df[id,]
testDT  <- df[-id,] 
test_result <- testDT$Avg_Utilization_Ratio
testDT[,Avg_Utilization_Ratio := NULL]


mod1 <- lm(Avg_Utilization_Ratio ~ ., data = trainDT)
mod1 <- step(mod1)
mod1
summary(mod1)
test_pred <- predict(mod1, testDT[,.(Gender,Income_Category,Card_Category,Months_on_book, Contacts_Count_12_mon,
                                     Credit_Limit,Total_Revolving_Bal,Total_Trans_Amt,Total_Trans_Ct,
                                     Total_Ct_Chng_Q4_Q1,Age_Group)])

pred_Lm <- data.table(cbind(Actual_Value = test_result,Predicted_Value = test_pred))
pred_Lm
#correaltion
correlation_Accuracy <- cor(pred_Lm) 
correlation_Accuracy #0.809 accuracy
RMSE(pred_Lm$Predicted_Value,pred_Lm $Actual_Value) #0.159216


####  Logsitic Regression ####
mod2 <- glm(Avg_Utilization_Ratio ~ ., data = trainDT,)
mod2 <- step(mod2)
summary(mod2) 

test_pred <- predict(mod2, testDT, type = 'response')
pred_Glm <- data.table(cbind(Actual_Value = test_result, Predicted_Value = test_pred))
pred_Glm

cor(pred_Glm)
