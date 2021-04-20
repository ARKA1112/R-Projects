#At first I will set the proper working directory
setwd(choose.dir())
getwd()

#Now we will read the data into R working space and store it into Comcast
#Also we will take the strings as factors incase we need to use them as categorical variables

comcast = read.csv('Comcast Telecom Complaints data.csv', header = T, stringsAsFactors = T)

#View the comcast data
library(lubridate)
?dmy
View(comcast)
comcast$Date = dmy(comcast$Date) #way to convert date and 
head(comcast)
comcast$Date  =as.factor(comcast$Date)
com_date = as.data.frame(summary(comcast$Date))
View(com_date)                       
library(ggplot2)
ggplot(com_date,aes(y = com_date$`summary(comcast$Date)`,x=row.names(com_date)))+
  theme(axis.text.x = element_text(angle = 45, size = 7))+geom_point(size = 4, col = 'Magenta')+
  geom_line(group = 1,size = 1, col  = 'black')+
  ggtitle("Number of complaints received according to date")+
  xlab('Date')+ylab('Number of complaints received')+
  theme(axis.ticks.x = element_line(colour = 'red',size = 3),axis.ticks.y = element_line(colour = 'red',size = 3),axis.text.y = element_text(size = '20'))+
  theme(axis.text.x = element_text(colour = 'black', size = 8, hjust =0.9), axis.title.y = element_text(size = 15),plot.background = element_rect(fill = 'white'),panel.grid = element_line(color = 'white'), panel.background = element_rect(fill = 'cyan'))+
  theme(axis.title.x = element_text(color = 'black',size = 15))
?theme
library(dplyr)
com_date$Date = row.names(com_date)
com_month$sum = tapply(com_date$`summary(comcast$Date)`,[1:3],sum)
?tapply
rownames(com_date) = c(1:91)
com_date$Date = as.Date(com_date$Date)
View(com_date)
View(com_month)

# Selecting the data based on MONTHS
Jan = com_date[com_date$Date >= '2015-01-01' & com_date$Date <= '2015-01-31',]         
Jan = sum(Jan$`summary(comcast$Date)`)
Feb = com_date[com_date$Date >= '2015-02-01' & com_date$Date <= '2015-02-28',]
Feb = sum(Feb$`summary(comcast$Date)`)
Mar = com_date[com_date$Date >= '2015-03-01' & com_date$Date <= '2015-03-31',]
Mar = sum(Mar$`summary(comcast$Date)`)
Apr = com_date[com_date$Date >= '2015-04-01' & com_date$Date <= '2015-04-30',]
Apr = sum(Apr$`summary(comcast$Date)`)
May = com_date[com_date$Date >= '2015-05-01' & com_date$Date <= '2015-05-31',]
May = sum(May$`summary(comcast$Date)`)
Jun = com_date[com_date$Date >= '2015-06-01' & com_date$Date <= '2015-06-30',]
Jun = sum(Jun$`summary(comcast$Date)`)
Jul = com_date[com_date$Date >= '2015-07-01' & com_date$Date <= '2015-07-31',]
Jul = sum(Jul$`summary(comcast$Date)`)
Aug = com_date[com_date$Date >= '2015-08-01' & com_date$Date <= '2015-08-31',]
Aug = sum(Aug$`summary(comcast$Date)`)
Sep = com_date[com_date$Date >= '2015-09-01' & com_date$Date <= '2015-09-30',]
Sep = sum(Sep$`summary(comcast$Date)`)
Oct = com_date[com_date$Date >= '2015-10-01' & com_date$Date <= '2015-10-31',]
Oct = sum(Oct$`summary(comcast$Date)`)
Nov = com_date[com_date$Date >= '2015-11-01' & com_date$Date <= '2015-11-30',]
Nov=  sum(Nov$`summary(comcast$Date)`)
Dec = com_date[com_date$Date >= '2015-12-01' & com_date$Date <= '2015-12-31',]
Dec = sum(Dec$`summary(comcast$Date)`)

#Combining the whole data into a single dataframe
com_month  = rbind(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec)
com_month = as.data.frame(com_month)
str(com_month)
#sorting out the month names and saving it as a dataframe
Month = row.names(com_month)
#attaching the month names with the existing column of com_month
com_month = cbind(com_month,Month)
#Renaming the rows in sequencial order
rownames(com_month) = 1:12
#Removing previously attached month names 
com_month = com_month[,-c(2,3)]
#renaming the columns as per my convenienec
colnames(com_month)  = c('Complaints','Month')
#re-adjusting the columns as per my own convenience
com_month = select(com_month,Month,Complaints)
#Ordering the columns according to the months
com_month$Month = factor(com_month$Month, level = month.abb)
#Creating a plot of complaints vs month using ggplot
com_month = order(month(com_month))#Orders according to month
ggplot(com_month,aes(x = Month, y = Complaints))+
  theme(panel.background = element_rect(color = 'blue', fill = 'gray'), axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15))+geom_point(aes(size = 15,col = 'blue'))+geom_line(aes(size = 2, col = 'red'), group = 1)
View(com_month)
#Proceeding further to the next steps
# Sorting the data based on complaint type

View(comcast)

com_comp = as.data.frame(comcast$Customer.Complaint)
View(com_comp)
#sorting out the keyworkds wghicj contain thwe word 'network'
speed_comp = contains(com_comp$`comcast$Customer.Complaint`,match = c('speed','speeds','slow','fast'),ignore.case = T)
speed_comp = length(speed_comp)
speed_comp

internet_comp = contains(com_comp$`comcast$Customer.Complaint`,match = c('data','internet','net','wifi','broadband','connection'),ignore.case = T)
internet_comp = length(internet_comp)
internet_comp

billing_comp = contains(com_comp$`comcast$Customer.Complaint`,match = c('billing','bill','payment','due','charged','charges'),ignore.case = T)
billing_comp = length(billing_comp)
billing_comp

customer_serv_comp = contains(com_comp$`comcast$Customer.Complaint`,match = 'customer service',ignore.case = T)
customer_serv_comp = length(customer_serv_comp)
customer_serv_comp
#Compiling them to a single table to compare the type of complaints
Number_of_complaints = c(speed_comp,internet_comp,billing_comp,customer_serv_comp)
d = c('Speed related','Network related','Billing related','Customer service related')
comp_table = as.data.frame(Number_of_complaints)
comp_table$Type_of_Complaint = d
View(comp_table)
comp_table = comp_table[,c(2,1)]
View(comp_table)
#Next problem to create a new categorical variable

summary(comcast$Status)
#New categorical variable has been added as per the instructions
comcast$Revised_complaint_status = as.factor(ifelse(comcast$Status == c('Open','Pending'),'Open and Pending','Closed and Solved'))
View(comcast)  
str(comcast$Revised_complaint_status)
  
#Create a stacked_bar_chart concerning states vs new categorical variable]

statewise_comp  = NULL
statewise_comp$States = as.data.frame(comcast$State)
View(statewise_comp)
statewise_comp$Closed_and_Solved = ifelse(comcast$Revised_complaint_status == 'Closed and Solved',1,0)
statewise_comp$Open_and_Pending = ifelse(comcast$Revised_complaint_status == 'Open and Pending',1,0)

statewise_comp = as.data.frame(statewise_comp)
View(statewise_comp)
statewise_comp$Closed_and_Solved = as.factor(statewise_comp$Closed_and_Solved)
statewise_comp$Open_and_Pending = as.factor(statewise_comp$Open_and_Pending)


#plotting the stacked barchart using ggplot

ggplot(comcast)+
  geom_bar(aes(x = State, fill = Revised_complaint_status))+
  xlab("Name of the States")+
  theme(axis.text = element_text(angle = 45, hjust = 0.9))+
  ggtitle("Stacked barchart based on the complaint status of thestates")

#Georgia has the highest number of complaints
a = as.data.frame(table(comcast$State, comcast$Revised_complaint_status))
View(a)
View(comcast)
summary(a)

#creating a new dataframe containing the complaint status

b = a[a$Var2 == 'Closed and Solved',]
c = a[a$Var2 == 'Open and Pending',]
b = b[,-2]
colnames(b) = c('States','Closed and Solved')
c = c[,-2]
colnames(c) = c('States','Open and Pending')
View(b)
View(c)
?cbind

#merging the dataframes BASED ON A COMMON COLUMN
statewise_comp = merge(b,c)
View(statewise_comp)

#finding the state which has the highest unresolved rate
statewise_comp$`Closed and Solved` = as.numeric(statewise_comp$`Closed and Solved`)
statewise_comp$`Open and Pending` = as.numeric(statewise_comp$`Open and Pending`)
statewise_comp$States = as.character(statewise_comp$States)
statewise_comp =  mutate(statewise_comp, Pct_unresolved = statewise_comp$`Open and Pending`*100/apply(statewise_comp[,-1],1,sum))
View(statewise_comp)




