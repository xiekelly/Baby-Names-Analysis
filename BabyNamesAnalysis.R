# Kelly Xie
# Prof. Tambe, ADE Spring 2017
# Lab 3: Baby Names


# ================================================================
# Objective 1


setwd("/data/babynamesdata")
babynames = NULL # creates data frame to store data

# imports data from files into data frame
for (year in (1950:2014)) {
  foo = read.csv(paste("yob", toString(year), ".txt", sep=''), 
                 header=FALSE)
  babynames = rbind(babynames, cbind(foo, year))  
}

# renames columns
names(babynames)[1] = "name"
names(babynames)[2] = "gender"
names(babynames)[3] = "count"


# ================================================================
# Objective 2


# creates data frame to store data corresponding to my name and gender
myname = babynames[which(babynames$name == "Kelly" 
                         & babynames$gender == "F"),]

# plots years against count
plot(myname$year, myname$count, xlab = "Year", ylab = "Counts", 
     main="Popularity of Kelly among Females from 1950 to 2014")


# ================================================================
# Objective 3


# data frame of only baby girl names
girlnames = babynames[which(babynames$gender == "F"),]

# computes and stores number of entries for each year
count_girlnames = table(girlnames$year)
countgirlnames = as.data.frame(count_girlnames)
names(countgirlnames)[1] = "year"
names(countgirlnames)[2] = "count"

# plots unique baby girl names for each year
plot(countgirlnames$year, countgirlnames$count, xlab = "Year", 
     ylab="Count", main="Trends in Unique Girl Name Popularity 
     from 1950 to 2014")


# ================================================================
# Objective 4


# creates a function that takes two arguments, gender and year
toptennames <- function(gender, year) {
  
  # creates data frame for rows matching gender and year
  ttn = babynames[which(babynames$gender == gender 
                        & babynames$year == year),]
  
  # orders by decreasing counts
  ttn[order(ttn$count, decreasing = TRUE),]
  
  # returns top 10 rows of data frame ttn
  return (ttn[1:10, "name"])
}

print(toptennames("F", 2014)) # testing


# ================================================================
# Objective 5


# isolates only 2014 names
names2014 = babynames[which(babynames$year==2014),]

# creates data frame for males and order in descending popularity
male2014 = names2014[which(names2014$gender == "M"),]
male2014 = male2014[order(male2014$count,
                                    decreasing=TRUE),] # order
male2014$rank = c(1:nrow(male2014)) # adds column for rank
male2014 = male2014[c("name","rank")] # fetchess columns

# creates data frame for females and order in descending popularity
female2014 = names2014[which(names2014$gender == "F"),]
female2014 = female2014[order(female2014$count,
                                        decreasing=TRUE),] # order
female2014$rank = c(1:nrow(female2014)) # adds column for rank
female2014 = female2014[c("name","rank")] # fetches columns

# merges the two data frames by name
allnames2014 = merge(male2014, female2014, by="name", suffixes=c(".m", ".f"))

# computes maximum rank across two columns using pmax()
maxrank = pmax(allnames2014$rank.m, allnames2014$rank.f)
allnames2014$maxrank = maxrank # add vector as column to data frame

# finds rows with minimum maxrank using order()
allnames2014 = allnames2014[order(allnames2014$maxrank),]

# prints top rows using head()
head(allnames2014)["name"]


# ================================================================
# Objective 6


# imports class names file
setwd("/data")
classnames = read.csv("classnamesdata.csv", header=FALSE)

# transforms babynames file to get total counts for each name in each year
nogender = aggregate(c(babynames$count), by=list(babynames$name, babynames$year), FUN=sum)
names(nogender) = c("name", "year", "count")

# computes probability of having a name in each year
# gets total name counts for given year
yeartotals = as.data.frame(rowsum(nogender$count, nogender$year))
yeartotals$year = row.names(yeartotals) # adds year column
names(yeartotals)=c("total", "year") # renames columns

# merges the two data frames
nogender = merge(nogender, yeartotals, by="year")

# computes no. babies with given name in that year/total no. of babies in that year
nogender$prob = nogender$count/nogender$total

# merges classnames and nogender by name
names(classnames) = c("name")
classnames = merge(classnames, nogender, by="name")

# calculates probabilities by year
foo = aggregate(classnames$prob, by=list(classnames$year), FUN=sum)
names(foo)=c("year", "prob")

# generates plot and max line
plot(foo$year, foo$prob, ylab="Sum likelihood", xlab="Year",
     main = "Estimated birth year given this sample of first names")
abline(v= foo[which(foo$prob==max(foo$prob)),"year"], col="blue", lty=2)

