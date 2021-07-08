getwd()
rm(list =ls())
install.packages(rio)
library(rio)
ps2020 <- import("fatal-police-shootings-data.csv")


#quick look
View(ps2020)
head(ps2020)
str(ps2020)
summary(ps2020)
table(ps2020)
dim(ps2020)
#individual variables
summary(ps2020$age)
summary(ps2020$manner_of_death)
summary(ps2020$gender)

#worry about missing data?
mean(ps2020$age)
## to fix this we can use na.rm = TRUE
mean(ps2020$age, na.rm = TRUE)

#checking correlation between threat attack and race
##creating variables

ps2020$shot = NA
ps2020$shot[ps2020$manner_of_death == "shot"] = 1
ps2020$shot[ps2020$manner_of_death != "shot"] = 0


ps2020$fleet = NA
ps2020$fleet[ps2020$flee != "Not fleeing"] = 1
ps2020$fleet[ps2020$flee == "Not fleeing"] = 0

cor(ps2020$shot, ps2020$fleet, use = "complete.obs")

ps2020$threatattack = NA
ps2020$threatattack[ps2020$threat_level == "attack"] = 1
ps2020$threatattack[ps2020$threat_level != "attack"] = 0

ps2020$black = NA
ps2020$black[ps2020$race == "B"] = 1
ps2020$black[ps2020$race != "B"] = 0

ps2020$armed_gun = NA
ps2020$armed_gun[ps2020$armed == "gun"] = 1
ps2020$armed_gun[ps2020$armed != "gun"] = 0

# you have to include use = "complete.obs" to ignore the missing values.
cor(ps2020$threatattack, ps2020$armed_gun)
cor(ps2020$threatattack, ps2020$black, use = "complete.obs")


#loops<-------------
#example of a loop
for(i in 1:20) {
  x <-sample(0:100,1)
  print(x)
  #print(i)
  i = i +1
}

#while loops
#first example
#initialize the loop
testwhile <- 1

while(testwhile <= 5){
  cat('We are on loop number', testwhile) #cat gathers together the text and what prints on screen
  testwhile <- testwhile+1 #creating the loop
  print(testwhile) #printing out what comes from the loop
}

#applying loop example to stock prices and values
set.seed(456)
# Set stock value and price
stock <- 100
price <- 100

# A counter for the number of loops
numloops <- 1

# Initialize the while statement
while(price>90){
     # Generate random numbers between 80 and 120 to check while loop
  price <- stock + sample(-20:20, 1)
  # Count the number of loop
  numloops = numloops+1
# Print the number of loop
print(numloops)
}
cat('it took',numloops,'loop before we short the price. The lowest price is', price)




#for loops the basics
#basic syntax for for loop
for (i in vector) {
  Exp
}

# Create an organization vector
orgs <-c('United Nations','World Bank','USAID','DFID/UKAID')
# Create the for statement
for(i in orgs) {
  print(i)
  }

#create a matrix, can be used for for loop. 
M <-matrix(c(1:12), nrow = 4, byrow = TRUE)
print(M)

#given matrix
mat <-matrix(data =seq(1, 10, by=1), nrow = 5, ncol =2)
View(mat)
#create for loop in the matrix with r & c to iterate over the matrix
for (r in 1:nrow(mat)) #number of rows in matrix
  for (c in 1:ncol(mat)) #number of columns in matrix
    print(paste("Row", r, "and column",c, "have values of", mat[r,c]))
#multiple dimensions require nested loop, so two for functions





#if else loops
DenlyAsleep <-c(1, 1, 1, 1, 1, 1, 1, 0)
ifelse(test = DenlyAsleep==1, yes = "Always!", no = "Wow, He's Awake!")

#more complicated if else expression syntax
if (condition) {
  Expr1
} else {
  Expr2
}
#example
quantity <- 901
if(quantity>900) {
  print('You scored well!')
}else{
  print('Try harder next time!')
}

#else-if
##basic syntax
if (condition) {
  Expr1
} else if (condition2){
  Expr2
}else if (condition3){
  Expr3
}else  (condition4){
    Expr4
}

# Create vector quantity
quantity <-  799
# Create multiple condition statement
if(quantity<800) {print('Time to turn off Netflix!')
  }else if(quantity>=800&quantity<=900) {
  print('It is coming along! Keep working at it!')
}else{
  print('You nailed it!!')
    }


#application of the loops with data set <-------
library(dplyr)
#subset the data
ps2020sub <-subset (ps2020,select =c("age", "black", "armed_gun"))

ps2020subfilter <- filter(ps2020sub, age == 70)
View(ps2020subfilter)

#starting loops, nested
for(r in 1:nrow(ps2020subfilter))
  for(c in 1:ncol(ps2020subfilter))
    print(paste("Row", r, "and column", c,
                "have values of", ps2020subfilter[r,c]))

#print more meaningful information, nested if else loops
for(r in 1:nrow(ps2020subfilter))
  if(ps2020subfilter[r,3]==1) {
    print('Victim was armed')
  }else{
      print('Victim was not armed')}


#using loops to change the dataframe to have more info: black instead of B
#subsetting data
ps2020small <-filter(ps2020,  age==15 )
ps2020small2 <-subset(ps2020small,select =c("age", "race", "armed"))
rm(ps2020small)

View(ps2020small2)
#using if else conditionals to recode
for (r in 1:nrow(ps2020small2)){
  for(c in 1:ncol(ps2020small2)) {
    if(ps2020small2[r,2]=="W") {
      ps2020small2[r,2] = "White"
    }
    else if (ps2020small2[r,c]=="B") {
      ps2020small2[r,c] = "Black"
    }
    else {
    }
  }
}
View(ps2020small2)

for (r in ps2020$armed)
  if(ps2020$armed == "gun"){
    print("had gun")
  }else {
    print("other ar")
  }

#writing your own functions <-----
library(foreign)
x <- cars
  min(x$speed)
  max(x$speed)
  min(x$dist)
  max(x$dist)
  mean(x$speed)
  mean(x$dist)

#basic syntax
function.name <- function(arguments)
  { #computations on the arguments
  #some other code
  #what runs every time the function is called
}

#example of creating a function 
xyzabc <-function(n)
  {
  # compute the square of integer`n`
  n^2
}
# calling the function and passing value 
xyzabc(12)


#creating a new mean function
mean_function <- function(x)
{
  sum(x) / length(x)
}

mean_function(ps2020subfilter$armed_gun)




#apply, sapply, and lappply <-----
#apply takes 3 arguments: apply(X, MARGIN, FUN)

#example
#creating a matrix
matrix1 <-matrix(C<-(1:10),nrow=5, ncol=6)
matrix1

#using apply to sum the columns
matrix1_a <- apply(matrix1, 2, sum)
matrix1_a

#using a loop instead
for(c in 1:ncol(matrix1))
  print(paste("Column",c, "has values of", sum(matrix1[,c])))

#use lapply for list objects
#general expression: lapply(X, FUN)

#example
countries <-c("AFGHANISTAN","BAHRAIN","LESOTHO","ZIMBABWE")
countries_lower <-lapply(countries, tolower)
str(countries)
str(countries_lower)

#to make it come back as a vector add unlist
countries_lower <-unlist(lapply(countries, tolower))
str(countries)
str(countries_lower)

#sapply functions
#general syntax: sapply(X, FUN)

dt <- cars
lmn_cars <-lapply(dt, min)
smn_cars <-sapply(dt, min)
lmn_cars
smn_cars
str(lmn_cars)
str(smn_cars)

#applying this to police data
ps2020age <- subset(ps2020,
                    select = "age")
ps2020age_mean <- apply(ps2020age, 2, mean)
ps2020age_mean

#we need to add na.omit for the NAs
ps2020age_mean <- apply(na.omit(ps2020age), 2, mean)
ps2020age_mean



#pattern matching and replacement <-----

#grep is used to find patterns
#basic syntax: grep("pattern", x)

x <-c("INXS", "WHAM!", "KISS", "ABBA", "RUSH", "MGMT", "AC/DC", "BB King")
grep("S",x)
grep("S|BB", x)

#sub and gsub lets us replace
#sub is for first match, gsub replaces all
#general syntax: sub("old", "new", x)
#general syntax: gsub("old", "new", x)

x
sub("SS", "PP", x)
gsub("BB", "The", x)

#with police data
ps2020pattern <- ps2020
View(ps2020pattern)

grep("San", ps2020pattern$city)

y <- gsub("San", "The", ps2020pattern$city)
y

#change race variable to have complete list
ps2020pattern$race <- gsub("W", "White", ps2020pattern$race)
ps2020pattern$race <- gsub("Blacklacklacklacklack", "Black", ps2020pattern$race)



#working with dates and times <------
library(lubridate)
library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
today()
now()

ymd("2017-01-31")
mdy("January 31st, 2017")
dmy("31-Jan-2017")

flights %>%
  select(year, month, day, hour, minute)


flights %>% 
  select(year, month, day, hour, minute) %>%
  mutate(departure = make_datetime(year, month, day, hour, minute))
  
#using this on ps2020 data
ps2020dates <- ps2020
head(ps2020dates)  
#adding info about date, its not just a character, its a date in y-m-d format
ymd("2015-01-02")

#separates the date into separate year month data varaibles
ps2020dates <-
  ps2020dates %>%
  dplyr::mutate(year = lubridate::year(ps2020dates$date),
                month = lubridate::month(ps2020dates$date),
                day = lubridate::day(ps2020dates$date))
#put them back together
ps2020dates$yearmonthday <- ymd(paste(ps2020dates$year,
                                      ps2020dates$month, 
                                      ps2020dates$day))

#returning to police shootings visualizations <---------
ps2020overview <- ps2020
ps2020overview$timestamp <- ymd(ps2020overview$date)


#creating visualizations 
ggplot(ps2020overview, aes(x=timestamp)) +
  geom_histogram(position="identity", bins =20, show.legend = FALSE) +
  coord_cartesian(ylim =c(250,350))

ggplot(ps2020overview, aes(x=timestamp)) +
  geom_bar(position = "identity", show.legend = FALSE)


#recoding things to plot
ps2020gender <- ps2020overview
table(ps2020gender$gender)

for (r in 1:nrow(ps2020gender)) {
  if(ps2020gender$gender[r] == "M") {
    ps2020gender$gender[r] = "Male"
  } else if (ps2020gender$gender[r] == "F") {
    ps2020gender$gender[r] = "Female"
  } else if (ps2020gender$gender[r] == "") {
    ps2020gender$gender[r] = "Other/Unknown" 
  } else {
    
  }
  
}

ggplot(ps2020gender, aes(x=gender)) +
  geom_bar(position = "identity", show.legend = FALSE)

#recoding using gsub
ps2020race1 <- ps2020
ps2020race1$race <- gsub("A", "Asian", ps2020race1$race)
ps2020race1$race <- gsub("B", "Black", ps2020race1$race)
ps2020race1$race <- gsub("H", "Hispanic", ps2020race1$race)
ps2020race1$race <- gsub("N", "Native American", ps2020race1$race)
ps2020race1$race <- gsub("O", "Other", ps2020race1$race)
ps2020race1$race <- gsub("W", "White", ps2020race1$race)

ps2020race1$race[ps2020race1$race == ""] = "Unknown"

ggplot(ps2020race1, aes(x= race)) +
  geom_bar(position = "identity", show.legend = FALSE)


#patterns of people shot over time
ps2020addtl <- 
  ps2020addtl%>%
  dplyr::mutate(year = lubridate::year(ps2020addtl$date),
                month = lubridate::month(ps2020addtl$date),
                day = lubridate::day(ps2020addtl$date),
                counter = ps2020addtl$manner_of_death=="shot",
                yearmonth =paste(year,month,sep=""))%>%
  dplyr::select(year,month,day,yearmonth,age,counter)

by_month_shot <-group_by(ps2020addtl, year, month)
monthly_shot <-summarize(by_month_shot, occurrence=sum(counter, na.rm=TRUE))
monthly_shot$yearmonth <- paste(monthly_shot$year,monthly_shot$month, sep = "")

ggplot(monthly_shot,
       aes(x=yearmonth,
           y=occurrence)) +
  geom_point() +
  coord_cartesian(ylim = c(50,125)) +
  theme(axis.text.x = element_text(size=6,angle = 45),
        axis.title.y = element_text(size=6,angle = 45))
