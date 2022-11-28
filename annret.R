library(data.table)
library(tidyverse)
library(lubridate)
library(sqldf)
library(broom)
library(tidyquant)
library(tidyr)


#-------------------------------------------------------------------------------
# path to files ...
#-------------------------------------------------------------------------------

# Github path to data
gitpath <- "https://raw.githubusercontent.com/sureshlazaruspaul/"

# Local path to files
dirpath <- "C:/data/stock_data/"







#-------------------------------------------------------------------------------
# stock data header file
# - contains all firms
# - contains industry codes, firm name, and identifiers
# - contains beginning and ending date of trading
# - create a column of ones(used for merging later on)
#-------------------------------------------------------------------------------

msfhdr <- fread(paste0(dirpath, "msfhdr.csv")) %>%
  select(c("permno", "hsiccd",
           "hcomnam", "begret", "endret")) %>%
  mutate(
    begret = as.Date(begret, "%d%b%Y"),
    endret = as.Date(endret, "%d%b%Y"),
    dummy = 1
  )

head(msfhdr)
glimpse(msfhdr)







#-------------------------------------------------------------------------------
# monthly market return file ...
# - contains returns of S&P 500 firms (equally-weighted, value-weighted)
#-------------------------------------------------------------------------------

msp500 <- fread(paste0(dirpath, "msp500.csv")) %>%
  mutate(
    date = as.Date(caldt, "%d%b%Y")
  )

glimpse(msp500)

# get all trading dates from market return files... 
dates <- msp500 %>% select(date) %>%
  mutate(
    dummy = 1
  )

glimpse(dates)








#-------------------------------------------------------------------------------
# get s&p 500 list ...
# - identifes which firms are in S&P
#-------------------------------------------------------------------------------

msp500list <- fread(paste0(dirpath, "msp500list.csv")) %>%
  mutate(
    begdt = as.Date(start, "%d%b%Y"),
    enddt = as.Date(ending, "%d%b%Y")
  ) %>%
  select(
    -start, -ending
  )

glimpse(msp500list)









#-------------------------------------------------------------------------------
# get monthly stock returns ...
# - gives stock returns (monthly) of all firms
#-------------------------------------------------------------------------------

msf <- fread(paste0(dirpath, "msf.csv")) %>%
  mutate(
    date = as.Date(date, "%d%b%Y")
  ) %>%
  select(
    permno, date, ret
  )

summary(msf) # diagnose return file
glimpse(msf)

msf <- msf %>%
  filter(
    !is.na(ret) & ret > -0.99
  )

summary(msf) # diagnose return file
glimpse(msf)









#-------------------------------------------------------------------------------
# for the full list of firms, get all possible trading dates ...
#-------------------------------------------------------------------------------

stock_data1 <- msfhdr %>%
  # get unique firms
  distinct() %>%
  # get trading dates for each firm 
  left_join( # merge dates ...
    dates,
    by = "dummy"
  ) %>%
  select(-dummy)

summary(stock_data1) # diagnose file
glimpse(stock_data1)









#-------------------------------------------------------------------------------
# for all possible trading dates, see if there is returns ...
#-------------------------------------------------------------------------------

stock_data2 <- stock_data1 %>%
  # get return data
  left_join(
    msf,
    by = c("permno", "date")
  ) %>%
  # limit to trading trades
  filter(
    date >= begret & date <= endret
  ) %>%
  select(-begret, -endret)

summary(stock_data2) # diagnose file

#-------------------------------------------------------------------------------
# Question? 
# Return should be there, but missing. What to do with missing returns?
#-------------------------------------------------------------------------------

glimpse(stock_data2)









#-------------------------------------------------------------------------------
# restrict firms to S&P 500 firms only ...
# get s&p list data ...
#-------------------------------------------------------------------------------

stock_data3 <- stock_data2 %>%
  # when was the firm in the s&p list?
  left_join(
    msp500list,
    by = c("permno")
  )  %>%
  # restrict sample
  filter(
    # drop unmerged data (S&P 500 firms only)
    !is.na(begdt) &
      # keep only firms within date range
      (date >= begdt & date <= enddt) &
      # define a sample period, if any
      between(date, as.Date("1925-01-01"), 
              as.Date("2020-12-31"))
  ) %>%
  select(-begdt, -enddt) %>%
  # what to do with missing returns? 
  # drop missing values (or)
  # filter(
  #  !is.na(ret) & ret >= -0.99
  # ) %>%
  # what to do with missing returns? 
  # replace with ZERO
  mutate(
    ret = ifelse(is.na(ret), 0, ret)
  ) 

summary(stock_data3) # diagnose file
glimpse(stock_data3)










#-------------------------------------------------------------------------------
# DO ALL THE PREVIOUS STEPS IN ONE SHOT
#-------------------------------------------------------------------------------

stock_data <- msfhdr %>%
  # get unique firms
  distinct() %>%
  # get trading dates for each firm 
  left_join( # merge dates ...
    dates,
    by = "dummy"
  ) %>%
  select(-dummy) %>%
  # get return data
  left_join(
    msf,
    by = c("permno", "date")
  ) %>%
  # limit to trading trades
  filter(
    date >= begret & date <= endret
  ) %>%
  select(-begret, -endret) %>%
  # when was the firm in the s&p list?
  left_join(
    msp500list,
    by = c("permno")
  )  %>%
  # restrict sample
  filter(
    # drop unmerged data (S&P 500 firms only)
    !is.na(begdt) &
      # keep only firms within date range
      (date >= begdt & date <= enddt) &
      # define a sample period, if any
      between(date, as.Date("1925-01-01"),
                    as.Date("2020-12-31"))
  ) %>%
  select(-begdt, -enddt) %>%
  # what to do with missing returns?
  # replace with ZERO
  mutate(
    ret = ifelse(is.na(ret), 0, ret)
  ) 

rm("dates", "msfhdr", "msp500", "msf", "msp500list")
rm("stock_data1", "stock_data2", "stock_data3")

summary(stock_data) # diagnose file
glimpse(stock_data)










#-------------------------------------------------------------------------------
# How to find return in the previous 12 months? ret1yr
# How to find return in the previous 24 months? ret2yr
# How to find return in the previous 36 months? ret3yr
#-------------------------------------------------------------------------------

annret <- stock_data %>%
  mutate(
    lag0 = 1 + ret
  ) %>%
  group_by(permno) %>%
  arrange(permno, date) %>%
  # create lags of return ...
  do(data.frame(., setNames(shift(.$lag0, 1:35), c(paste0("lag", 1:35))))) %>%
  ungroup() %>%
  # count missing
  mutate(
    nmiss_3yr = rowSums(is.na(.))
  ) %>%
  # drop varibles
  select(
    -c(lag24:lag35)
  ) %>%
  # count missing
  mutate(
    nmiss_2yr = rowSums(is.na(.))
  ) %>%
  # drop varibles
  select(
    -c(lag12:lag23)
  ) %>%
  # count missing
  mutate(
    nmiss_1yr = rowSums(is.na(.))
  ) %>%
  # drop varibles
  select(
    -c(lag0:lag11)
  ) %>%
  mutate(
    lag0 = 1 + ret
  ) %>% 
  group_by(permno) %>% 
  arrange(permno, date) %>% 
  # create lags of return ...
  do(data.frame(., setNames(shift(.$lag0, 1:35), c(paste0("lag", 1:35))))) %>%
  ungroup()








#-------------------------------------------------------------------------------
# initialize all returns to 1
#-------------------------------------------------------------------------------

annret$ret1yr <- 1
annret$ret2yr <- 1
annret$ret3yr <- 1

for(index in 0:11) {
  annret <- annret %>%
    mutate(
      ret1yr = 
        ret1yr * eval(parse(text=paste0("lag", index))))
}

for(index in 0:23) {
  annret <- annret %>%
    mutate(
      ret2yr = 
        ret2yr * eval(parse(text=paste0("lag", index))))
}

for(index in 0:35) {
  annret <- annret %>%
    mutate(
      ret3yr = 
        ret3yr * eval(parse(text=paste0("lag", index))))
}










#-------------------------------------------------------------------------------
# how many missing returns within 12/24/36 months is acceptable?
#-------------------------------------------------------------------------------

annret <- annret %>%
  mutate(
    ret1yr = ifelse(nmiss_1yr <= 2, ret1yr - 1, NA),
    ret2yr = ifelse(nmiss_2yr <= 4, ret2yr - 1, NA),
    ret3yr = ifelse(nmiss_3yr <= 6, ret3yr - 1, NA)
  ) %>%
  select(
    -ret, -contains("lag", ignore.case = TRUE)
  )



summary(annret) # diagnose file
glimpse(annret)










#-------------------------------------------------------------------------------
# box plot
#-------------------------------------------------------------------------------

canvas1 <- annret %>%
  ggplot() + 
  theme_light() +
  ylim(-100, 100) +
  labs(
    title = "Distribution of One-Year Returns",
    subtitle = "Sample: S&P 500 firms only",
    caption = paste("From", min(annret$date), 
                    "to", max(annret$date)),
    x = "",
    y = "One-Year Returns (%)"
  )

canvas1 + 
  geom_boxplot(aes(y = ret1yr*100)) +
  scale_x_discrete() +
  coord_flip()

#-------------------------------------------------------------------------------

canvas2 <- annret %>%
  ggplot() + 
  theme_light() +
  ylim(-100, 100) +
  labs(
    title = "Distribution of Two-Year Returns",
    subtitle = "Sample: S&P 500 firms only",
    caption = paste("From", min(annret$date), 
                      "to", max(annret$date)),
    x = "",
    y = "Two-Year Returns (%)"
  )
  
canvas2 + 
  geom_boxplot(aes(y = ret2yr*100)) +
  scale_x_discrete() +
  coord_flip()

#-------------------------------------------------------------------------------

canvas3 <- annret %>%
  ggplot() + 
  theme_light() +
  ylim(-100, 100) +
  labs(
    title = "Distribution of Three-Year Returns",
    subtitle = "Sample: S&P 500 firms only",
    caption = paste("From", min(annret$date), 
                      "to", max(annret$date)),
    x = "",
    y = "Three-Year Returns (%)"
  )

canvas3 + 
  geom_boxplot(aes(y = ret3yr*100)) +
  scale_x_discrete() +
  coord_flip()
  
  
  
  
  
  
  
  
  

#-------------------------------------------------------------------------------
# descriptive statistics
#-------------------------------------------------------------------------------

summary(annret$ret1yr)
quantile(annret$ret1yr,
         probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 
                   0.75, 0.90, 0.95, 0.99), na.rm = TRUE)


summary(annret$ret2yr)
quantile(annret$ret2yr,
         probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 
                   0.75, 0.90, 0.95, 0.99), na.rm = TRUE)


summary(annret$ret3yr)
quantile(annret$ret3yr,
         probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 
                   0.75, 0.90, 0.95, 0.99), na.rm = TRUE)
