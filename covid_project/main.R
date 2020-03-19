library(dplyr)
library(rworldmap)
library(plotly)
library(plyr)
library(ggplot2)
# load data ---------------------------------------------------------------
df <- read.csv("COVID19_open_line_list.csv", nrows = 1000,  as.is = TRUE) 
names(df)[1] <- 'ID'


# clean data --------------------------------------------------------------

columns = c(1:9, 11, 24, 25)
df <- df[,  columns]
df[is.na(df['age']), 2] = 'N/A'
df[(df[2] == '') | (df[2] == 'N/A') , 2] = NA
df_d = df[(df['outcome'] == 'died') | (df['outcome'] == 'death'),] # filter deaths

unique(df$outcome)

#format to numeric
df$age <- as.numeric(df$age)
df_d$age <- as.numeric(df_d$age)

# write date to sql
library(DBI)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

dbListTables(con)
table_for_db = as.table(df$age)
dbWriteTable(con, 'main db', df)


# plot on map -------------------------------------------------------------

plot_on_map <- function(df){
  new_map <- getMap(resolution = "low")
  plot(new_map)
  #plot(new_map, xlim = c(-20, 59), ylim = c(35, 71), asp = 1) # only europe
  points(df$longitude, df$latitude, col= "red", cex = .6)
}

# plot all cases on map
plot_on_map(df)

## plot deaths on map
plot_on_map(df_d)


# analysis base factors ---------------------------------------------------
# all
age = df[!is.na(df$age), 2]
median(age)
fivenum(age)
boxplot(age, data = age)

#death
age_d = df_d[!is.na(df_d$age), 2]
median(age_d)
fivenum(age_d)
boxplot(age_d, data = age_d)
fig <- plot_ly(y= age_d, type = "box")
fig <- fig%>% add_trace(y = age)

#daily cases
df <- read.csv("COVID19_open_line_list.csv", nrows = 12265  ,as.is = TRUE) 
df_daily = df
df_daily[df_daily == ""] = NA

as.Date("18.01.2020", tryFormat = ("%d.%m.%Y"))
df_daily$date_onset_symptoms = as.Date(df_daily$date_onset_symptoms, tryFormat = ("%d.%m.%Y"))


ggplot(df_daily, aes(date_onset_symptoms))+ 
  geom_density(alpha = 0.2) +scale_x_date()

ggplot(df_daily, aes(date_onset_symptoms, fill = '#A4A4A4',  color="darkred")) + 
  geom_histogram() +scale_x_date()
  
