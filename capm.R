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
      between(date, as.Date("1990-01-01"), 
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

rm("dates", "msfhdr", "msp500", "msf", "msp500list")
rm("stock_data1", "stock_data2", "stock_data3")

summary(stock_data) # diagnose file
glimpse(stock_data)










#-------------------------------------------------------------------------------
# Industry classification
#-------------------------------------------------------------------------------
# http://mba.tuck.dartmouth.edu/pages/
#   faculty/ken.french/Data_Library
#-------------------------------------------------------------------------------

ffind <- fread(paste0(gitpath,
                      "fama-french-ind-class/main/ffind.csv")) %>%
  filter(ind_def == 48) %>%
  rename(ffclass = class) %>%
  select(ffclass, sic_start, sic_end)

glimpse(ffind)










#-------------------------------------------------------------------------------
# SQL merge - merge by range ...
# - merge if firm's sic is between sic_start and sic_end values
#-------------------------------------------------------------------------------

ind_data_new <- 
  sqldf("select * 
        from stock_data LEFT JOIN ffind on 
        (stock_data.hsiccd >= ffind.sic_start and 
            stock_data.hsiccd <= ffind.sic_end)",
        stringsAsFactors = FALSE) %>% 
        # drop missing ffclass ....
        filter(
          !is.na(ffclass)
        ) %>%
          # drop some variables ....
          select(
            -sic_start, -sic_end
          ) %>%
            # sort data ...
            arrange(
              ffclass, date, permno
            ) %>%
              # group by ...
              group_by(
                ffclass, date
              ) %>%
                # compute average industry returns ...
                summarise(
                  indret = mean(ret)
                ) %>%
                  mutate(
                    month = month(date),
                    year = year(date)
                  )

rm("ffind")

summary(ind_data_new) # diagnose file
glimpse(ind_data_new)









#-------------------------------------------------------------------------------
# get three-factors for the 3-factor-model 
#-------------------------------------------------------------------------------

three_factor <-
  fread(paste0(gitpath, 
               "fama-french-ind-class/main/3factors.csv")) %>%
  mutate(
    year = round(date/100, digits = 0),
    month = date - (year*100),
    mktpre = mktpre/100,
    smb = smb/100,
    hml = hml/100,
    rf = rf/100
  ) %>% select(-date)

glimpse(three_factor)










#-------------------------------------------------------------------------------
# final dataset
#   - merge with three factors 
#-------------------------------------------------------------------------------

final_data <- ind_data_new %>%
  # merge 3-factors to stock_data
  left_join(
    three_factor,
    by = c("year", "month")
  ) %>%
    select(
      -year, -month
    )










#-------------------------------------------------------------------------------
# CAPM Estimates
#-------------------------------------------------------------------------------

capm_model <- 
  final_data %>%
  group_by(ffclass) %>%
  group_modify(
    ~ tidy(lm(indret ~ mktpre, data = .))
  ) %>%
  filter(
    term %in% c("mktpre")
  ) %>%
  ggplot(
    aes(
      x = ffclass,
      y = estimate
    )
  ) +
  theme_tq_green() +
  labs(
    title = "CAPM Estimates for Market Risk Premium",
    subtitle = "48 Industry Portfolios",
    caption = paste("from", min(final_data$date), 
                    "to", max(final_data$date)),
    x = "Industry Classifications",
    y = "Estimate"
  ) +
  geom_point(
    shape=22, fill="red", color="darkred", size=2
  ); capm_model










#-------------------------------------------------------------------------------
# Three-factor Model Estimates
#-------------------------------------------------------------------------------

ff3_model <- 
  final_data %>%
  group_by(ffclass) %>%
  group_modify(
    ~ tidy(lm(indret ~ mktpre +smb + hml, data = .))
  ) %>%
  filter(
    !term %in% c("(Intercept)")
  ) %>%
  ggplot(
    aes(
      x = ffclass,
      y = estimate,
      group = term,
      shape = term,
      color = ffclass
    )
  ) +
  theme_tq() +
  labs(
    title = "Three-factor Model Estimates",
    subtitle = "48 Industry Portfolios",
    caption = paste("from", min(final_data$date), 
                    "to", max(final_data$date)),
    x = "Industry Classifications",
    y = "Estimate"
  ) +
  geom_point(
    size=2
  ); ff3_model
