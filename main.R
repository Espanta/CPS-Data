CPS <- read.csv("data/cpsdata.csv")
names(CPS)

#Among the interviewees with a value reported for  the Industry variable, what is the most common industry of employment? 
sort(table(CPS[!is.na(CPS$Industry),"Industry"]),decreasing = TRUE)[1]

#Which state has the fewest interviewees?
sort(table(CPS$State))

#What proportion of interviewees are citizens of the United States?
sum(prop.table(table(CPS$Citizenship,useNA = "no"))[1:2])

#For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
table(CPS$Race, CPS$Hispanic)
sort(tapply(CPS$Hispanic == 1, CPS$Race, sum))


#Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)
table(is.na(CPS$Sex))

#Often when evaluating a new dataset, we try to identify if there is a pattern in the missing values in the dataset.
#We will try to determine if there is a pattern in the missing values of the Married variable. 
#The function is.na(CPS$Married) returns a vector of TRUE/FALSE values for whether the Married variable is missing. 
#We can see the breakdown of whether Married is missing based on the reported value of the Region variable with the function table(CPS$Region, is.na(CPS$Married)). Which is the most accurate:
table(CPS$Region, is.na(CPS$Married)) 
table(CPS$Sex, is.na(CPS$Married)) 
table(CPS$Sex, is.na(CPS$Married)) 
table(CPS$Citizenship, is.na(CPS$Married)) 

# How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)?
table(CPS$State, is.na(CPS$MetroAreaCode))

#Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
prop.table(table(CPS$Region,is.na(CPS$MetroAreaCode)),1)


#Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
Non_Met <- data.frame(prop.table(table(CPS$State,is.na(CPS$MetroAreaCode)),1))
Non_Met[order(Non_Met$Var2, Non_Met$Freq),]


MetroAreaMap <- read.csv("Data/MetroAreaCodes.csv")
CountryCodes <- read.csv("Data/CountryCodes.csv")
#How many observations (codes for metropolitan areas) are there in MetroAreaMap?
str(MetroAreaMap)


#How many observations (codes for countries) are there in CountryMap?
str(CountryCodes)
CPS <- merge(CPS,MetroAreaMap, by.x = "MetroAreaCode", by.y="Code", all.x = TRUE)
CPS<- merge(CPS,CountryCodes, by.x = "CountryOfBirthCode", by.y="Code", all.x = TRUE)
names(CPS)

Which of the following metropolitan areas has the largest number of interviewees?

table(CPS$MetroArea)

#How many interviewees have a missing value for the new country of birth variable?
table(is.na(CPS$Country))


Among all interviewees born outside of North America, which country was the most common place of birth?
table(CPS$Country)
