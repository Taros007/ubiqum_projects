
# The first argument is a data frame.
# 
# The subsequent arguments describe what to do with the data frame. 
# You can refer to columns in the data frame directly without using $.
# 
# The result is a new data frame


## Load libraries
library(nycflights13)  # creates flights tibble

#filter() allows you to select a subset of rows in a data frame. 
#Like all single verbs, the first argument is the tibble (or data frame). 
#The second and subsequent arguments refer to variables within that data frame, 
#selecting rows where the expression is TRUE
filter(flights, month == 1, day == 1)

#arrange() works similarly to filter() except that instead of filtering or 
#selecting rows, it reorders them. It takes a data frame, and a set of 
#column names (or more complicated expressions) to order by. 
#If you provide more than one column name, each additional column will be 
#used to break ties in the values of preceding columns:
arrange(flights, year, year, month, day)
arrange(flights, desc(arr_delay))

# Select columns by name
select(flights, year, month, day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

#Rename column
rename(flights, tail_num = tailnum)

#Add new columns that are functions of existing columns. 
#This is the job of mutate()
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

#If you only want to keep the new variables, use transmute()
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

#Summarize collapses a data frame to a single row
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)

#Randomly sample rows with sample_n() and sample_frac() 
#for respectively a number of observations, or a fraction of the total sample
#Add replace = TRUE to bootstrap samples
sample_n(flights, 10)
sample_frac(flights, 0.01)


by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)


#Added functions for summarize, aside from min, max, mean etc.
#n(): the number of observations in the current group
#n_distinct(x):the number of unique values in x.
#first(x), last(x) and nth(x, n)
#these work similarly to x[1], x[length(x)], and x[n] but 
#give you more control over the result if the value is missing.

# PAY ATTENTION: all column names have separate meaning in Dplyr functions
# So this doesn't work like you expect it to.
select(flights, year)
select(flights, 1)

year <- 5
select(flights, year) #not the same as calling select(flights, 5)

