df <- read.csv("C:/Users/User/Desktop/Data Set- Inc5000 Company List_2014.csv")
View(df)

#top rows
head(df)

head(df, 10)

#bottom rows
tail(df, 3)

#shape
dim(df)

#info
str(df)

#statistic
summary(df)

#number of rows
nrow(df)

names(df)

#number of columns
ncol(df)

#number of columns
length(names(df))

#indexing and slciing

df[1] #1st in column

df[1] # 1st column

df["X_input"] #1st column

df$city

df$city[2] # city on 2nd index

df$company[1] #company name on 1st index

df$company[1:10] #top 10 companies

df$growth


#calculating average growth
mean(df$growth)

max(df$growth)

min(df$growth)

#filtering companies with growth more than 100k
subset(df$company, df$growth>100000)

barplot(head(df$growth), names.arg=head(df$company), 
        main="Bar Plot", xlab="Company", ylab="Growth")

barplot(head(df$growth,3), names.arg=head(df$company,3), 
        main="Bar Plot", xlab="Company", ylab="Growth")

subset(df$city, df$metro=="Los Angeles")


install.packages("dplyr")

library(dplyr)

filter(df, df$yrs_on_list>5)

subset(df$company, df$yrs_on_list>10)

head(filter(df, df$yrs_on_list>5))

#list companies which have more than 1billion revenue and located in city Dallas
subset(df, df$revenue>1000000000 & df$city=="Dallas", select=c("revenue", "city"))

# List the name, city, state, metro and industry of companies with 
# their growth less than 100 but revenue more than 100 million
filter(df, df$revenue>100000000 & df$growth<100)

#duplicate vlaues
duplicated (df)
duplicated(df$company)

sum(duplicated(df))

sum(duplicated(df$X_input))

sum(duplicated(df$X_widgetName))

sum(duplicated(df$company))


#missing values
is.na(df)

sum(is.na(df))

colSums(is.na(df))

#removing a null column
df = subset(df, select=-X_input)
View(df)


df = subset(df, select = -c(X_num, X_widgetName, X_source, X_resultNumber, X_pageUrl))


#rounding up growth column
head(df$growth)
head(round(df$growth,0))

df$growth = round(df$growth,0)

library(tidyverse)

#drawing boxplot and labeling outliers
ggplot(df, aes(x=revenue, y=growth)) + 
  geom_boxplot(outlier.colour = "purple", outlier.shape = 1)+ 
  scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

median(df$growth)

quantile(df$growth, 0.25)

quantile(df$growth, 0.75)

quantile(df$growth, 0.50)

mean(df$growth)

#calculating q1, q3 and iqr
q1_growth = quantile(df$growth, 0.25)
q3_growth = quantile(df$growth, 0.75)
iqr = q3_growth - q1_growth

IQR(df$growth)

upper_boundary = q3_growth + 1.5*iqr
lower_boundary = q1_growth - 1.5*iqr

df_cleaned = subset(df, df$growth>(lower_boundary) & df$growth<(upper_boundary))

dim(df_cleaned)
dim(df)


ggplot(df_cleaned, aes(x=rank, y=workers)) + 
  geom_point()+ scale_y_continuous(labels=scales::comma)+
  coord_cartesian(ylim = c(0, 300))

ggplot(df_cleaned, aes(x=yrs_on_list)) + 
  geom_bar()+coord_cartesian(ylim = c(0, 1500), xlim=c(0,15))

#companies with more than 10 years on this list
subset(df$company, df_cleaned$yrs_on_list>=10)

#export cleaned data
a``
#find current location
getwd()
