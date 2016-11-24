# filePath <- paste(getwd(),"/census.Data.csv",sep="")
require("ggplot2")

census.Data <- read.csv(file.choose(),na.strings = "?", fill=TRUE)


# ==============================================================
# ========================QUESTION 2============================
# ==============================================================


# (2) TO COUNT PERCENTAGE MISSING VALUES
print("Answer 2 (A) :");
print("Percentage missing values in each attributes :")
print(sapply(census.Data, function(x) paste(round((sum(is.na(x))*100)/nrow(census.Data),2),"%")))

# TO PRINT COUNT VS MISSING VALUES HISTOGRAM
no.Values.In.Row <- as.data.frame(apply(census.Data, 1, function(x) sum(is.na(x))))
colnames(no.Values.In.Row) <- c("Missing values per Data Point/Row")

sapply(colnames(no.Values.In.Row),
       function(x){
         print(
           # Histogram by qplot function
           qplot(no.Values.In.Row[,x],
                 geom = "histogram",
                 xlab = x,
                 main = paste("Histogram of",x),
                 binwidth=1
           )
           
         )
       }
)


# ==============================================================
# ========================QUESTION 3============================
# ==============================================================


# (3) (A) TO PLOT HISTOGRAM FOR EACH NUMERICAL VARIABLE IN DATA
numeric.Variables <- census.Data[,c("age","hrs_per_week")]

sapply(colnames(numeric.Variables),
       function(x){
         print(
           
           ggplot(census.Data, aes(numeric.Variables[,x]))
           + geom_histogram(binwidth=1)
           + xlab(x)
           + ggtitle(paste("Histogram for \"",x,"\""))
           
         )
       }
)

# (3) (B) TO PLOT HISTOGRAM FOR EACH NUMERICAL VARIABLE IN DATA BASED ON VALUE OF INCOME
sapply(colnames(numeric.Variables),
       function(x){
         print(
           ggplot(census.Data, aes(numeric.Variables[,x]))
           + geom_histogram(binwidth=1)
           + xlab(x)
           + ggtitle(paste("Histogram for \"",x,"\""))
           + facet_grid(.~income)
         )
       }
)

# (3) (C) BOX PLOT FOR EACH NUMERICAL VALUE
sapply(colnames(numeric.Variables),
       function(x){
         print(
           
           ggplot(census.Data, aes(x="",y=numeric.Variables[,x]))
           + geom_boxplot()
           + xlab(x)
           + ggtitle(paste("Box plot for \"",x,"\""))
           + facet_grid(.~income)
           
         )
       }
)

# ==============================================================
# ========================QUESTION 4============================
# ==============================================================


# (4) (A) TO PLOT BAR PLOT FOR EACH CATEGORICAL VARIABLE IN DATA
categorical.Variables <- census.Data[,c("work","edu","marital","occupation","race","sex")]

sapply(colnames(categorical.Variables),
       function(x){
         print(
           
           ggplot(census.Data, aes(categorical.Variables[,x]))
           + geom_bar()
           + xlab(x)
           + ggtitle(paste("Bar plot for \"",x,"\""))
           
         )
         
       }
)

print("Answer 4 (A) :")
print("Variable 'work' has 7 (including 'NA') unique values")
print("Variable 'edu' has 7 unique values")
print("Variable 'marital' has 6 unique values")
print("Variable 'occupation' has 9 (including 'NA') unique values")
print("Variable 'race' has 5 unique values")
print("Variable 'sex' has 2 unique values")

# (4) (B) TO PLOT BAR PLOT FOR EACH NUMERICAL VARIABLE IN DATA BASED ON VALUE OF INCOME
sapply(colnames(categorical.Variables),
       function(x){
         print(
           
           ggplot(census.Data, aes(categorical.Variables[,x]))
           + geom_bar()
           + xlab(x)
           + ggtitle(paste("Bar plot for \"",x,"\""))
           + facet_grid(.~income)
           + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
           
         )
       }
)

# (5) Scatter plot between age and hrs_per_week
print(
  ggplot(census.Data,
         aes(
           x=census.Data$age,
           y=census.Data$hrs_per_week
         )
  )
  + xlab("age")
  + ylab("hrs_per_week")
  + geom_point(shape=1)
  + geom_smooth(method=lm)   # Add linear regression line (by default includes 95% confidence region)
  
)

# To overcome overplotting

print(
  ggplot(census.Data,
         aes(
           x=census.Data$age,
           y=census.Data$hrs_per_week
         ))
  + xlab("age")
  + ylab("hrs_per_week")
  + geom_point(shape=19,      # Use solid circles
               alpha=1/10)     # 1/4 opacity
  + geom_smooth(method=lm)   # Add linear regression line (by default includes 95% confidence region)
)

print(paste("The correlation coefficient between 'age' & 'hrs_per_week' is ~",round(cor(census.Data$age,census.Data$hrs_per_week),2),"(",cor(census.Data$age,census.Data$hrs_per_week),")"))