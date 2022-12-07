### ----------------------------- ###
# Title:  PG DA DataScience with R  #
# By   :  Yigit Brave Cesur         #
# Date :  2022-10-01                #
### ------------------------------###
 
#### Statement Of Project Objectives ####
# To make a model that predicts the Weekly sale of Walmart store based on the
# given user inputs with good accuracy .

#### Business Understanding ####
# From the business perspective , if we know under what circumstances , the walmart
# sales increases , we can predict the future sales and it can also answer various
# business questions .


#### Data Understanding ####
# *Store - the store number or the location of store
# *Date - the week of sales
# *Weekly Sales - weekly sales for a certain store
# *Holiday Flag - indicates whether the current week is a special holiday week where 0
#                 indicates Non - holiday week and 1 indicates Holiday week
# *Temperature - On the day of the sale , the temperature
# *Fuel Price - The cost of gasoline in the area
# *CPI ( consumer price index ) - the current consumer price index .
# *Unemployment - The current rate of unemployment

### Holiday Events
# Super Bowl: 12-Feb-10, 11-Feb-11, 10-Feb-12, 8-Feb-13
# Labour Day: 10-Sep-10, 9-Sep-11, 7-Sep-12, 6-Sep-13
# Thanksgiving: 26-Nov-10, 25-Nov-11, 23-Nov-12, 29-Nov-13
# Christmas: 31-Dec-10, 30-Dec-11, 28-Dec-12, 27-Dec-13

### Basic Statistics tasks

# 1-Which store has maximum sales
# 2-Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation
# 3-Which store/s has good quarterly growth rate in Q3’2012
# 4-Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
# 5-Provide a monthly and semester view of sales in units and give insights



### Statistical Model
# For Store 1 – Build  prediction models to forecast demand
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# Change dates into days by creating new variable.
# Select the model which gives best accuracy.

# ---------------------------------------------------------------------
# LIBRARIES 
libraries <- function(){
 library(readr)
 library(tidyverse)
 library(funModeling)
 library(ggplot2)
 library(forecast)
 library(arules)
 library(dplyr)
 library(lubridate)
 library(zoo)
 library(stringr)
 library(lessR)
 library(BBmisc)
 library(rlang)
 library(corrplot)
 library(psych)
 library(pastecs)
 library(d3Tree)
 library(caTools)
 library(MLmetrics)
 print("The libraries have been loaded .")
}
libraries()

# ----------------------------------------------------------------------

# First look to DATA 
WM<- read_csv("Walmart_Store_sales.csv")

colnames(WM)
dim(WM)
         
glimpse(WM)           
   
summary(WM)              

# -------------------------------------------------------
# TRANSFORM NUMERIC To CATEGORICAL

WM$Store <- as.factor(WM$Store)

WM$Holiday_Flag <- as.factor(WM$Holiday_Flag)

# -------------------------------------------------------
# SUMMARY OF DATA for DESCRIPTION
# MEASURE OF CENTRAL TENDENCY AND DISPERSION 

## Take a look for the types of Data
profiling_num(WM)      # General statistical info For numerical variables 

plot_num(WM)           # Visualizing the the Numeric variables (Look at Plots section)

freq(WM)               # All about Categorical Variables  ( Look at the Plots section)



describe(WM)            #  Checking Missing Values with Central Tendency
# We have any missing values on WM


num_cols <- WM %>%
  select(Weekly_Sales ,Temperature , Fuel_Price , CPI , Unemployment) 
stat.desc(num_cols)    

# see the dataframe as Interactive . You can see the structure of Full Data Frame


d3tree(list(root = df2tree(rootname ='WM', 
                           struct = as.data.frame(WM)), 
            layout = 'collapse'))                       # look at the VIEWER secction ..Click on Blue spots

# ---------------------------------------------------------------------
#### Missing and Duplicated Values

# We already checked Missing Values the above
sum(is.na(WM))
colSums(is.na(WM))  #Observed no NA values

#Checking Duplicate Values
all(duplicated(WM) == TRUE)
#observed no duplicate values

# ---------------------------------------------------------------------

# TRANSFORM THE DATE TO DATE OBJECT

WM$Date <- gsub('/', '-', WM$Date)
WM$Date

WM$Date=as.Date(WM$Date,format="%d-%m-%Y")
WM$Date
class(WM$Date)

WM$Day <- format(WM$Date, format = "%d")       # Extract day from date
WM$Month <- format(WM$Date, format = "%m")       # Extract Month from date
WM$Year <- format(WM$Date, format = "%Y")       # Extract Year from date


# Converting Date series to quaterly
WM$Quaterly <- as.yearqtr(WM$Date)
WM_Quarterly <- WM %>%
  group_by(Quaterly)%>%
  summarise_all(mean)


# ----------------------------------------------------------------------
# DATA VISUALIZATION

# Holiday Flag
ggplot(WM, aes(x=as.factor(Holiday_Flag) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7))+ 
  ggtitle("Holiday") 

# Temperature  
ggplot(WM, aes(Temperature)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef")

ggplot(WM, aes(Temperature)) +                
  geom_density(fill="#69b3a2", color="#e9ecef")

# CPI
ggplot(WM, aes(CPI)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef")

ggplot( WM, aes(x=CPI)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# Fuel Price
ggplot(WM, aes(Fuel_Price)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef")

ggplot( WM, aes(x=Fuel_Price)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# Unemployment
ggplot(WM, aes(Unemployment)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef")

ggplot( WM, aes(x=Unemployment)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

# Weekly Sales
ggplot(WM, aes(Weekly_Sales)) +
  geom_histogram(fill="#69b3a2", color="#e9ecef")

ggplot( WM, aes(x=Weekly_Sales)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

describe(WM$Weekly_Sales)

--------------------------------------------------------------
#How the Variables affect the Sales 
  
#Correlation between Temperature and Sales
WM%>%
  ggplot(aes(Temperature, Weekly_Sales))+
  geom_point(alpha =1/10)+
  labs(title = 'Temperature: There are more people to shop in low temperatures',
       y='Weekly sales',
       x='Temperature')+
  theme_bw()  

#Correlation between Fuel Price  and Sales
WM%>%
  ggplot(aes(Fuel_Price, Weekly_Sales))+
  geom_point(alpha =1/10)+
  labs(title = 'Fuel Price: There are more people to shop in specific fuel price',
       y='Weekly sales',
       x='Fuel Price')+
  theme_bw()

#Correlation between CPI and Sales
WM%>%
  ggplot(aes(CPI, Weekly_Sales))+
  geom_point(alpha =1/10)+
  labs(title = 'CPI: Seems that different range of CPI have same sales distributions',
       y='Weekly sales',
       x='CPI')+
  theme_bw()

#Correlation between Unemployment and Sales
WM%>%
  ggplot(aes(Unemployment, Weekly_Sales))+
  geom_point(alpha =1/10)+
  labs(title = 'Unemployment: Lower unemployment rate has higher weekly sales',
       y='Weekly sales',
       x='Unemployment')+
  theme_bw()

#Correlation between Holiday Flag and Sales
WM%>%
  ggplot(aes(Holiday_Flag, Weekly_Sales))+
  geom_boxplot()+
  labs(title = 'Holiday Flag: Seems no difference in weekly sales',
       subtitle = 'However, there are extreme value in Non-holiday week',
       y='Weekly sales',
       x='Holiday Flag')+
  theme_bw()

# Store and Weekly Sales
WM%>%
  ggplot(aes(Weekly_Sales, reorder(as_factor(Store),
                                   FUN = median, Weekly_Sales)))+
  geom_boxplot()+
  labs(title = 'Store: There are difference in weekly sales',
       x='Weekly sales',
       y='Store')+
  theme_bw()


#Correlated Variables
cor_WM=cor(WM[, c(3,5,6,7,8)] )    # By index
corrplot(cor_WM , method='pie', type='lower', tl.cex=0.9)


#--------------------------------------------------------------------------------------------------
### 1- Which store has maximum sales ###

### Option 1 ####
store_max_sales <- WM %>% group_by(Store) %>% summarise(total_sales = sum(Weekly_Sales)) %>% 
  filter(total_sales == max(total_sales))
store_max_sales

### Option 2 ####
#Aggregating data by 'Store' and Finding sum of 'Weekly_Sales' 
Store_Sales<- data.frame( aggregate(Weekly_Sales ~ Store, data = WM, sum))
#Changing column name of sales 
colnames(Store_Sales)[2] <- "Total_Sales_by_Store"
#Finding out Store with highest Sales 
Store_Sales <-arrange(Store_Sales, desc(Total_Sales_by_Store))      #Arranged Stores based on Sales in descending order
Store_Sales[1,]                                                     #Choosing the first store that comes in this order
print(paste('Store no.', Store_Sales[1,]$Store,
            'has the maximum sales and the value is = ', Store_Sales[1,]$Total_Sales_by_Store))
# Converting Store column into factor 
Store_Sales$Store <- as.character(Store_Sales$Store)
Store_Sales$Store <- factor(Store_Sales$Store, levels=unique(Store_Sales$Store))
#Plotting Store vs TotalSales
options(repr.plot.width = 14, repr.plot.height = 8)
max_store_chart <-ggplot(data=Store_Sales, aes(x=Store, y=Total_Sales_by_Store)) + geom_bar(stat="identity",fill="red") +
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5, hjust=0.5))+ scale_x_discrete(breaks = WM$Store)+
  ggtitle('Store vs Sales')+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Stores") + ylab("Total Sales")
max_store_chart

#-------------------------------------------------------------------------------------------------------------------------
# 2-Which store has maximum standard deviation i.e., the sales vary a lot. Also, find out the coefficient of mean to standard deviation

max_SD_stores <- WM %>% group_by(Store) %>% summarise(SD_stores = sd(Weekly_Sales)) %>% 
  filter(SD_stores == max(SD_stores))
max_SD_stores

CV_Stores <- WM %>%  group_by(Store) %>% summarise(CV = sd(Weekly_Sales)/mean(Weekly_Sales)) %>% 
  filter(CV == max(CV))
CV_Stores

WM %>% 
  group_by(Store) %>% 
  summarize(sd=sd(Weekly_Sales),mean=mean(Weekly_Sales),CV=sd/mean) %>% 
  arrange(desc(sd))

#------------------------------------------------------------------------------------------
## 3- Which store/s has good quarterly growth rate in the third quarter of 2012

### Q3/2012 period
Q3_2012 <- subset(WM, subset = Quaterly == ' 2012 Q3')
Max_Q3_2012 <- max(Q3_2012$Weekly_Sales)
Q3_2012[Q3_2012$Weekly_Sales == Max_Q3_2012, ]
# it looks STORE 20 IN the date of 2012-07-06 is the most growth rate in 2012-Q3

### Q2/2012 period
Q2_2012 <- subset(WM, subset = Quaterly == ' 2012 Q2')
Max_Q2_2012 <- max(Q2_2012$Weekly_Sales)
Q2_2012[Q2_2012$Weekly_Sales == Max_Q2_2012, ]
# It looks STORE 20 in the date of 2012-04-06 is the most sales in 2012-Q2

Walmart.data<-read.csv('Walmart_Store_sales.csv')
Walmart.data$Date<-as.Date(Walmart.data$Date,format="%d-%m-%Y")
Q3_WM<-filter(Walmart.data, Walmart.data$Date>="2012-07-06" & Walmart.data$Date<= "2012-09-28")
max_sales_Q.data<-third_Q.data %>%
  group_by(Store) %>%
  summarize(quarterly_sales = sum(Weekly_Sales),)  %>%
  arrange(desc(quarterly_sales))
max_sales_Q.data
max_sales_Q.data[1,1]

# FOR GROWTH RATE  (GR)
#Creating new dataframe to do alterations 
WM1 <- read_csv("Walmart_Store_sales.csv")

#Creating a month- year column in data2 
WM1$month_Year = substr(WM1$Date, 4, 10)

#Subsetting Q3-2012 data (i.e, 07-2012,08-2012,09-2012), Q2-2012 data (i.e, 04-2012,05- 2012,06-2012)
Q3_2012 <- filter(WM1,month_Year == "07-2012" | month_Year== "08-2012" | month_Year== "09-2012")
Q2_2012 <- filter(WM1,month_Year == "04-2012" | month_Year== "05-2012" | month_Year== "06-2012")

#Aggregating sales by store for Q3-2012 
Q3_2012_Sales<-summarise(group_by(Q3_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q3_2012_Sales)[2] <- "Q3_2012_Sales_by_Store"

#Aggregating sales by store each Q2-2012 
Q2_2012_Sales<-summarise(group_by(Q2_2012,Store),sum(Weekly_Sales))

#Changing column names
colnames(Q2_2012_Sales)[2] <- "Q2_2012_Sales_by_Store"

#merging two quarters data by store
Q3_2012_GR <- merge ( Q2_2012_Sales , Q3_2012_Sales , by = 'Store')

#Creating Growth rate column for Sales by Store in the above dataframe 
Q3_2012_GR <- mutate(Q3_2012_GR, Growth_Rate = ((Q3_2012_Sales_by_Store - Q2_2012_Sales_by_Store)*100) / Q2_2012_Sales_by_Store)

#Creating only positive growth rates
Plus_GR <- filter(Q3_2012_GR, Growth_Rate > 0 ) 
Plus_GR<-arrange(Plus_GR, desc(Growth_Rate)) 
View(Plus_GR)
Better_Growth_Stores<- Plus_GR$Store

#printing the output
print(paste(c('The positive growth rate Stores are', Better_Growth_Stores),collapse=" " )) 
print(paste('Store',Plus_GR[1,1], 'has highest growth rate & it is',Plus_GR[1,4]))

# Store 7 -13.33% , Store 16 - 8.49% , Store 35 - 4.47% and 7 more stores with positive growth rates.
 

#-----------------------------------------------------------------------------------------
# 4-Some holidays have a negative impact on sales. Find out holidays which have higher sales than the mean sales in non-holiday season for all stores together
#######################################################
#Creating Holidays Data dataframe
#WM2<- read_csv("Walmart_Store_sales.csv")

Holiday_Dates <- c("12-02-2010", "11-02-2011", "10-02-2012", "08-02-2013","10-09-2010", "09-09-2011", "07-09-2012", "06-09-2013","26-11-2010", "25-11-2011", "23-11-2012", "29- 11-2013","31-12-2010", "30-12-2011", "28-12-2012", "27-12-2013")
Events <-c(rep("Super Bowl", 4), rep("Labour Day", 4),rep("Thanksgiving", 4), rep("Christmas", 4))
Holidays <- data.frame(Events,Holiday_Dates)

#merging both dataframes
WM2<-merge(WM,Holidays, by.x= "Date", by.y="Holiday_Dates", all.x = TRUE)

#Replacing null values in Event with No_Holiday 
WM2$Events = as.character(WM2$Events) 
WM2$Events[is.na(WM2$Events)]= "No_Holiday" 
head(WM2)

#Creating dataframe the mean of sales for No_Holiday and also mean of sales for different events
Holiday_Sales<-aggregate(Weekly_Sales ~ Events, data = WM2, mean)
#Changing column names
colnames(Holiday_Sales)[2] <- "Mean_Sales_by_Event_Type"
View(Holiday_Sales)

# Christmas and Labour Day has negative impact on sales where as Thanks giving and Super Bowl has positive impact on sales

# checking negative impact based on holiday date and non- holiday date
#Filtering holiday dates and finding mean of Weekly Sales 
Holiday_Dates <- filter(WM2,Holiday_Flag ==1)
Holiday_Date_Sales<-summarise(group_by(Holiday_Dates,Date),mean(Weekly_Sales))

#Calculating mean of Weekly Sales for non holidays
avg_NonHoliday_sales <- mean(filter(WM2,Holiday_Flag ==0)$Weekly_Sales) 
Holiday_Date_Sales$more_than_non_holiday <- Holiday_Date_Sales[,2] > avg_NonHoliday_sales
View(Holiday_Date_Sales)


weekly_sales <- aggregate(Weekly_Sales~Date, data=WM,mean)
weekly_sales$Date <-as.Date(weekly_sales$Date, "%d-%m-%Y")
weekly_sales <-arrange(weekly_sales,Date)
weekly_sales$Date <-factor(weekly_sales$Date)

# plotting weekly mean sales
Visual <- ggplot(data=weekly_sales, aes(x=Date, y=Weekly_Sales, group=1)) +
  geom_line(color="steelblue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_x_discrete(breaks = levels(weekly_sales$Date)[c(T, rep(F, 9))])

#Plotting Christmas
Visual +ggtitle('CHRISTMAS')+
  geom_point(aes(x = factor("2010-12-31"), y = 898500.4), color = "red", size = 2) +
  geom_point(aes(x = factor("2011-12-30"), y = 1023165.8), color = "red", size = 2) +
  geom_hline(aes(yintercept = avg_NonHoliday_sales), linetype="dashed")

#Plotting Laborday
Visual + ggtitle('LABOR DAY')+
  geom_point(aes(x = factor("2010-09-10"), y = 1014097.7), color = "deeppink", size = 2) +
  geom_point(aes(x = factor("2011-09-09"), y = 1039182.8), color = "deeppink", size = 2) +
  geom_point(aes(x = factor("2012-09-07"), y = 	1074001.3), color = "deeppink", size = 2) +
  geom_hline(aes(yintercept = avg_NonHoliday_sales), linetype="dashed")

#Plotting Thanks Giving
Visual + ggtitle('THANKS GIVING')+
  geom_point(aes(x = factor("2010-11-26"), y = 	1462689.0), color = "indianred4", size = 2) +
  geom_point(aes(x = factor("2011-11-25"), y = 1479857.9), color = "indianred4", size = 2) +
  geom_hline(aes(yintercept = avg_NonHoliday_sales), linetype="dashed")

#Plotting Superbowl
Visual + ggtitle('SUPER BOWL')+
  geom_point(aes(x = factor("2010-02-12"), y = 	1074148.4), color = "goldenrod4", size = 2) +
  geom_point(aes(x = factor("2011-02-11"), y = 1051915.4), color = "goldenrod4", size = 2) +
  geom_point(aes(x = factor("2012-02-10"), y = 1111320.2), color = "goldenrod4", size = 2) +
  geom_hline(aes(yintercept = avg_NonHoliday_sales), linetype="dashed")

#------------------------------
WM%>%
  group_by(Date)%>%
  summarise(Weekly_Sales = mean(Weekly_Sales, na.rm=T))%>%
  ggplot(aes(Date, Weekly_Sales))+
  geom_point(aes(color=Weekly_Sales>1200000), show.legend = F)+
  geom_line(color='grey')+
  labs(title = 'Date: Some dates have extreme high weekly sales',
       y='Weekly sales',
       x='Date')+
  theme_bw()

# Creating a dataFrame 
Super_Bowl <- c(20100212 , 20110211, 20120210 )
Super_Bowl <- dmy(Super_Bowl)
Labour_Day <- c(20100910 , 20110909 , 20120907 )
Labour_Day <- dmy(Labour_Day)
Thanksgiving <- c(20101126,20111125, 20121123 )
Thanksgiving <- dmy(Thanksgiving)
Christmas <-  c(20101231, 20111230,20121228 )
Christmas <- dmy(Christmas)

holiday_sales <- WM %>%
  mutate(Holiday= case_when(Date %in% Super_Bowl ~ "Super Bowl",
                            Date %in% Labour_Day ~ "Labour Day",
                            Date %in% Thanksgiving ~ "Thanksgiving",
                            Date %in% Christmas ~ "Christmas",
                            TRUE ~ "Non-Holiday"))

holiday_sales %>%
  select(Weekly_Sales,Holiday) %>%
  group_by(Holiday) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  arrange(desc(average_sales))

holiday_sales %>%
  select(Weekly_Sales,Holiday) %>%
  group_by(Holiday) %>%
  summarise(average_sales = mean(Weekly_Sales)) %>%
  ggplot(aes(x=Holiday,y=average_sales,fill=Holiday)) + geom_col() + ggtitle("Sales by Holidays") + theme(plot.title = element_text(hjust = 0.5))

# Christmas and Labour Day has negative impact on sales where as Thanks giving and Super Bowl has positive impact on sales


#-------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------
# 5- Provide a monthly and semester view of sales in units and give insights

monthly_data <- WM %>%
  mutate(Month = month(Date,label = TRUE),Year = year(Date),Semester = as.character(semester(Date,with_year = TRUE)))

# MONTHLY 2010
monthly_data %>%
  filter(Year == 2010) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
  ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + geom_col() + ggtitle("2010") +theme(plot.title = element_text(hjust = 0.5))
### Increase in April, July and October. Most increase in December.


# MONTHLY 2011
monthly_data %>%
  filter(Year == 2011) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
  ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + geom_col() + ggtitle("2011") +theme(plot.title = element_text(hjust = 0.5))
### Same trends with 2010

# MONTHLY 2012
monthly_data %>%
  filter(Year == 2012) %>%
  group_by(Month) %>%
  summarise(Monthly_sale = sum(Weekly_Sales)) %>% 
  ggplot(aes(x=Month,y=Monthly_sale,fill=Month)) + geom_col() + ggtitle("2012") +theme(plot.title = element_text(hjust = 0.5))
### Trends has changed. There are increases in March, June, and August.

### SEMESTERLY by SALES
monthly_data %>% 
  group_by(Semester) %>%
  summarise(Semester_sales = sum(Weekly_Sales)) %>%
  ggplot(aes(x=Semester,y=Semester_sales,fill=Semester)) + geom_col() + ggtitle("Sales by Semesters") +theme(plot.title = element_text(hjust = 0.5))

#--------------------------------------------------------------------------------------------------------
### STATISTICAL MODEL

# For Store 1 – Build  prediction models to forecast demand
# Linear Regression – Utilize variables like date and restructure dates as 1 for 5 Feb 2010 (starting from the earliest date in order). Hypothesize if CPI, unemployment, and fuel price have any impact on sales.
# Change dates into days by creating new variable.
# Select the model which gives best accuracy.

STORE1 <- WM %>%
filter(Store==1) %>%
  mutate(Year = year(Date),Day = day(Date),Month = month(Date),Date=1:143) %>%
  #select(-Store,-Holiday_Flag)
  select(-Store)
# STORE1 by SALES vs TEMPERATURE
ggplot(STORE1,aes(x=Temperature,y=Weekly_Sales)) + geom_point() + geom_smooth(color="red1") +ggtitle("Sales vs Temperature") + theme(plot.title = element_text(hjust = 0.5))

corr_with_Temperature <- cor(STORE1$Weekly_Sales,STORE1$Temperature)


# STORE1 by SALES vs FUEL PRICE
ggplot(STORE1,aes(x=Fuel_Price,y=Weekly_Sales)) + geom_point() + geom_smooth(color="blue") +ggtitle("Sales vs Fuel Price") + theme(plot.title = element_text(hjust = 0.5))

corr_with_FuelPrice <- cor(STORE1$Weekly_Sales,STORE1$Fuel_Price)

# STORE1 by SALES vs CPI
ggplot(STORE1,aes(x=CPI,y=Weekly_Sales)) + geom_point() + geom_smooth(color="green") +ggtitle("Sales vs CPI") + theme(plot.title = element_text(hjust = 0.5))

corr_with_CPI <- cor(STORE1$Weekly_Sales,STORE1$CPI)

# STORE1 by SALES vs UNEMPLOYMENT
ggplot(STORE1,aes(x=Unemployment ,y=Weekly_Sales)) + geom_point() + geom_smooth(color="black") +ggtitle("Sales vs Unemployment") + theme(plot.title = element_text(hjust = 0.5))

corr_with_Unemployment <- cor(STORE1$Weekly_Sales,STORE1$Unemployment)


#------------------------------------------------------------------------------

#Checking on Outliers with Boxplots

par(mfrow=c(2,3))
boxplot(STORE1$Weekly_Sales,col="Blue",main = "Weekly Sales") 
boxplot(STORE1$Unemployment,col="Black",main = "Unemployment")
boxplot(STORE1$Temperature,col="Red",main = "Temperature")
boxplot(STORE1$Fuel_Price,col = "Green",main = "Fuel Price")
boxplot(STORE1$CPI,col = "Yellow",main = "CPI")

#Finding and Removing Outliers from Weekly Sales column and Unemployment column

Quant_ws <- quantile(STORE1$Weekly_Sales, probs=c(.25, .75), na.rm = FALSE)
IQR_ws <- IQR(STORE1$Weekly_Sales)
STORE1<- subset(STORE1, STORE1$Weekly_Sales > (Quant_ws[1] - 1.5*IQR_ws) & STORE1$Weekly_Sales < (Quant_ws[2]+1.5*IQR_ws))
View(STORE1)

Quant_unemp <- quantile(STORE1$Unemployment, probs=c(.25, .75), na.rm = FALSE)
IQR_unemp <- IQR(STORE1$Unemployment)
STORE1<- subset(STORE1, STORE1$Unemployment > (Quant_unemp[1] - 1.5*IQR_unemp) & STORE1$Unemployment < (Quant_unemp[2]+1.5*IQR_unemp))

View(STORE1)

#Dropping SOME columns

STORE1 <- subset(STORE1, select= -c(Year, Month, Quaterly, Holiday_Flag))
STORE1 <- subset(STORE1, select = -c(Store))
STORE1 <- subset(STORE1, select = -c(Date))

#Correlation plot

corr<-cor(STORE1 [,c(1,3:6)])
str(corr)

corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

#Split into train and test


set.seed(1) 
sample = sample.split(STORE1$Weekly_Sales, SplitRatio = 0.70) 
train_STORE1 = subset(STORE1, sample == T)
test_STORE1 = subset(STORE1, sample == F)

#####  Create BASE model with all columns: 
model1 = lm(formula = Weekly_Sales ~ . , data = train_STORE1)
summary(model1)

#CPI and Day are greater than others . because others are higher than 0.05 that are not significant

#### Creating revised STORE1 with only relevant columns
STORE1_rev = subset(STORE1, select = c(Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment) )
set.seed(1)
sample = sample.split(STORE1_rev$Weekly_Sales, SplitRatio = 0.70) 
train_STORE1_rev = subset(STORE1_rev, sample == T)
test_STORE1_rev = subset(STORE1_rev, sample == F)

model2 = lm(formula = Weekly_Sales ~ . , data = train_STORE1_rev)
summary(model2)

#Model after dropping Fuel Price

model3 = lm(formula= Weekly_Sales~ Temperature + CPI + Unemployment, data = train_STORE1_rev)
summary(model3)

#model usimg only CPI and unemployment

model4 = lm(formula= Weekly_Sales~ CPI + Unemployment, data = train_STORE1_rev)
summary(model4) 

#Model after dropping Fuel Price and Unemployment

model5 = lm(formula= Weekly_Sales~ Temperature + CPI , data = train_STORE1_rev)
summary(model5)

#model using only CPI and Temperature

y_prediction_test = predict(model5, newdata = test_STORE1_rev)
y_prediction_test 

ggplot() + 
  geom_point(aes(x=test_STORE1_rev$Weekly_Sales,y=y_prediction_test)) +
  xlab('actual_sales') +
  ylab('predicted_sales')+
  ggtitle('comparison of test data')

### Parameters to validate the accuracy of the model and improvise.

MAPE(y_prediction_test,test_STORE1_rev$Weekly_Sales)

RMSE(y_prediction_test,test_STORE1_rev$Weekly_Sales)




