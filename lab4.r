(AirPassengers)
#task 1____________________________________

#part (i)
profitable = max(AirPassengers) #returns the maximum value from the time series data set
profitable
month <- matrix(AirPassengers, ncol=12,byrow=TRUE) #name of the matrix is 'month'

#renaming the columns and rows
colnames(month) <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
rownames(month) <- c("1949", "1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960")

month #displaying the matrix month
rownames(month) #displaying rownames
colnames(month) #displaying colnames

pos = which(month == profitable, arr.ind=TRUE) # condition to get index of max value
pos

pyear = rownames(month)[pos[,1]]#saving proftable year's value in pyear
pmonth = colnames(month)[pos[,2]]#saving proftable month's value in pmonth

cat("The most profitable month in the 12 years' data is", pmonth, "of", pyear) #part (i) final answer

#part (ii)
month <- cbind(month,apply(month[,1:12],1,sum))
month
colnames(month)[13] <- "SUM" # adding column name to the last colum which has
# the sum of all months in a year
a <- max(month, 12) # shows the max value from years
a

pos2 = which(month == a, arr.ind=TRUE) # condition to get index of max value

pyear2 = rownames(month)[pos2[,1]]#saving proftable year's value in pyear

cat("The most profitable year in the 12 years' data is", pyear2, "with max value", a) #part (ii) final answer

#part (iii)
plot.ts(AirPassengers) # plot graph of time series


#task 2_________________________________
initial = 8000 #initial price of ticket

for(r in 1:12) #for every rows
{
  for(c in 1:12) #fro each column
  {
    mat2 = month * initial #construcnting new matrix
  }
  initial =  initial * 1.1 #increase by 10% in price
}

mat2

#part (i)
mat2<-mat2[,-13]
i = max(mat2) #returns the maximum value from the matrix
i

mat2 #displaying the matrix month
rownames(mat2) #displaying rownames
colnames(mat2) #displaying colnames

pos3 = which(mat2 == i, arr.ind=TRUE) # condition to get index of max value
pos3

pyear3 = rownames(mat2)[pos[,1]]#saving proftable year's value in pyear
pmonth3 = colnames(mat2)[pos[,2]]#saving proftable month's value in pmonth

cat("The most profitable month in the 12 years' data is", pmonth3, "of", pyear3) #part (i) final answer

#part (ii)
mat2 <- cbind(mat2,apply(mat2[,1:12],1,sum))

colnames(mat2)[13] <- "SUM" # adding column name to the last colum which has
# the sum of all months in a year
b <- max(mat2, 13) # shows the max value from years
b

pos4 = which(mat2 == b, arr.ind=TRUE) # condition to get index of max value

pyear4 = rownames(mat2)[pos2[,1]]#saving proftable month's value in pmonth

cat("The most profitable year in the 12 years' data is", pyear4, "with value", b) #part (ii) final answer

#part(iii)
c <- sum(mat2, 13)
mat2
cat ("Total revenue from 12 years is", c)


#task 3________________________
plot(decompose(AirPassengers))
