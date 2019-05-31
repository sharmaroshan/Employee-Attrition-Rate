# Importing a csv file as a Dataframe .

Employee <- read.csv('employee.csv')

# Deleting unwanted columns .

Employee$Attrition <- NULL
Employee$BusinessTravel <- NULL
Employee$DistanceFromHome <- NULL
Employee$EmployeeCount <- NULL
Employee$DailyRate <- NULL
Employee$EnvironmentSatisfaction <- NULL
Employee$JobInvolvement <- NULL
Employee$JobSatisfaction <- NULL
Employee$JobLevel <- NULL
Employee$RelationshipSatisfaction <- NULL
Employee$Education <- NULL
Employee$HourlyRate <- NULL
Employee$Over18 <- NULL
Employee$PerformanceRating <- NULL
Employee$StandardHours <- NULL
Employee$StockOptionLevel <- NULL
Employee$YearsSinceLastPromotion <- NULL
Employee$TrainingTimesLastYear <- NULL
Employee$WorkLifeBalance <- NULL
Employee$MonthlyRate <- NULL
Employee$YearsWithCurrManager <- NULL
Employee$YearsAtCompany <- NULL
Employee$YearsInCurrentRole <- NULL
Employee$OverTime <- NULL
Employee$TotalWorkingYears <- NULL

# Deleting rows to make dataset smaller .

#Employee <- Employee[-c(501:1470)]
#Employee <- Employee[(Employee$EmployeeNumber < 600)]
Employee <- subset(Employee , EmployeeNumber < 600)

# Renaming the columns.

colnames(Employee)[1] = 'Age'
colnames(Employee)[3] = 'Education'
colnames(Employee)[4] = 'E.ID'
colnames(Employee)[6] = 'Job'
colnames(Employee)[8] = 'Income'
colnames(Employee)[9] = 'Exp'
colnames(Employee)[10] = 'Sal.Hike'

# ReArranging the columns.

Employee <- Employee[, c("E.ID", "Department" , "Job" ,"Education","Income","Age", "Gender", "MaritalStatus","Exp.(yrs)","Sal.Hike(%)")]

# Multiplying income into 10 for generalisation purposes.

Employee$Income <- Employee$Income*10

# Checking for null values.

any(is.na(Employee))

# Using ggplot we will perform Data Visualisation.
# Comparing various depts' salaries through barplots.
# Finding average Income of each dept.

avg.sales <- mean(subset(Employee,Department == 'Sales')$Income)
avg.rnd <- mean(subset(Employee,Department =='Research & Development')$Income)
avg.hrd <- mean(subset(Employee,Department =='Human Resources')$Income)

avg.income.male <- mean(subset(Employee,Gender=='Male')$Income)
avg.income.female <- mean(subset(Employee,Gender =='Female')$Income)

# Creating a new Column Avg.Income.Dept and Avg.Income.Gen

Employee$Avg.Income.Dept <- 0

Employee$Avg.Income.Dept[Employee$Department == 'Sales'] <- avg.sales
Employee$Avg.Income.Dept[Employee$Department == 'Human Resources'] <- avg.hrd
Employee$Avg.Income.Dept[Employee$Department == 'Research & Development'] <- avg.rnd

Employee$Avg.Income.Gen <- 0
Employee$Avg.Income.Gen[Employee$Gender == 'Male'] <-avg.income.male
Employee$Avg.Income.Gen[Employee$Gender == 'Female'] <-avg.income.female

# DATA VISUALIZATION

# Histogram
ggplot(Employee , aes(x = Income)) + geom_histogram(fill="black",colour="blue",alpha = 0.4)

# From the above graph we come to know that there are more no. of employees having income in range 
# (20000 - 60000) and very few Employees have income of around 1.5 - 2 lacs.

ggplot(Employee ,aes(x=Exp)) + geom_histogram(fill="yellow",colour="orange",alpha=0.9)

# Scatterplot

ggplot(Employee ,aes(x =Age, y =Income))+ geom_point(size = 1,aes(colour = Gender),alpha= 0.9)

# From above graph we come to know that lower age Employes have lower income whereas
# greater age Employees have greater income 

#Barplot

pl <- ggplot(Employee ,aes(x=Age,y=Exp ,fill =factor(Gender)))+geom_bar(stat ="identity",position="dodge")
pl2 <- pl + scale_fill_discrete(name="Gender",breaks=c(1,2),labels=c("Male","Female"))
print(pl2)
# From above graph we come to know the no. of male Emmployes and no. of Female Employes 
# of different age and their Experiences.

ggplot(Employee ,aes(x= Exp))+geom_bar(aes(colour =Department)))
# From above graph we come to know that how many Employes each department has with
# how much of experience in yrs.

ggplot(Employee ,aes(x =Department)) + geom_bar(aes(fill=Avg.Income.Dept),position="dodge")

# From above graph we come to know that there are around 15 Employees of HR Dept. having avg salary 69492
# whereas Research Dept. is having an average salary of 63074 with more than 300
# Employees and Sales Dept having highest avg Salary with around 130 Employees.

ggplot(Employee,aes(x=Education))+ geom_bar(aes(fill =Job ,position="dodge"))
# From above graph we can see various jobs available on different fields like 
# HD ,RD and sales.it also shows the count of each jobs for each field.

ggplot(Employee ,aes(x= Education)) + geom_bar()
# From above graph we can say that more Employe's age ranges from 30-40
# Employees under 20 and above 60 are very few in number.

ggplot(Employee, aes(Gender))+geom_bar(aes(fill =MaritalStatus ,position ="fill"))
# From above graph we can see how many male employes are married,single or divorced
# and similarly for female employes.

ggplot(Employee ,aes(Gender))+geom_bar(aes(fill=Avg.Income.Gen ))
# From above graph we can see that number of female employees is lesser than the no. of
# male employees but the average income of female employes is higher than male employes.

# Boxplot

ggplot(Employee ,aes(x=factor(Department),y=Income)) +geom_boxplot() +theme_dark()
# In the aove graph we have used a box plot , it tells that HR have Income range from 25000-80000
# in average and it also shows their mean value , standard deviation etc similarly it also gives 
# us an idea of income range ,mean and standard deviation of sales and RND departments.
# we also used a dark theme here to inhance it's background.

ggplot(Employee,aes(x=Job,y = Income))+ geom_boxplot() +theme_bw()
# This Box plot shows variation of different jobs providing different salaries,
# it also tells us mean and standard deviation and we have applied a bw theme.

# Densityplot

ggplot(Employee,aes(x=Income)) + geom_density(aes(fill =Job ,position ="stack"),alpha=0.4)+theme_classic()
# This is a density plot it gives an idea of income various jobs done by the Employees
# it is clearly visible that manager and research director are having highest density 
# of income whereas sales representative is getting the least density of income.
# i have used a classic theme.

ggplot(Employee ,aes(x=Income))+geom_density(aes(fill=Department,position="stack"),alpha=0.4)+theme_linedraw()
# Again , it is comparing the densities of income with respect to Department and it is
# visible that more Employees of HR dept are getting 200000 salary than sales and RND dept.
# also more Employees of RND Dept are getting the least salary in comparision to other depts.
# I have used a line draw theme to inhance the view of the density chart.

# 2D Bin Chart

ggplot(Employee,aes(x=Age ,y=Income)) +geom_bin2d() + theme_get()
# A 2D Bin chart it shows relationship between age and income .

# Lineplot

ggplot(Employee ,aes(x=Exp, y= Income)) +geom_line()
# a line plot to show the relationship between exp and income through line  plotting.
