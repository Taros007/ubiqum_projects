## Load libraries
library(RMySQL)

## Create a database connection
con = dbConnect(MySQL(), 
                user='XXX', 
                password='XXX', 
                dbname='dataanalytics2018', 
                host='35.239.91.216'
                )

## List the tables contained in the database
dbListTables(con)

## Lists attributes contained in a table
dbListFields(con,'yr_2006')

#Pull databases
Id <- c(2006:2010)
sqlcmd <- paste("SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 from yr_", Id, sep="")

# List of dataframes
df_list <- lapply(sqlcmd , function(x) dbGetQuery(con, x)) 

for (i in 1:5){
  assign(paste0("yr_", 2005+i), df_list[[i]])
}

#Clean-up
remove(df_list, sqlcmd, Id, con)
