library(tidyverse)
library(data.table)
library(janitor)
library(tidyquant)
library(plotly)
library(gganimate)
library(gifski)
library(patchwork)


#-------------------------------------------------------------------------------
# path to files ...
#-------------------------------------------------------------------------------

# Github branch path
gitpath <- "https://raw.githubusercontent.com/sureshlazaruspaul/"

# Github path to files
dirpath <- "/BUS662-practice-datasets/main/credit-risk/"










#-------------------------------------------------------------------------------
# Credit Risk 
# - sample 1: small dataset
#-------------------------------------------------------------------------------

crisk <- fread(paste0(gitpath, dirpath, "crisk.csv"))






#-------------------------------------------------------------------------------
# Credit Risk 
# - sample 2: large dataset
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# read_data.R reads credit risk dataset.
# written by,
#  Suresh Paul, C.Phil, M.S., B.E.
#   Lecturer II, Balwin Wallace University
#   Founder & Sr. Data Scientist, Algorithm Basics LLC
# Copy of this code is available at,
#  https://raw.githubusercontent.com/sureshlazaruspaul/
#    BUS662-practice-datasets/main/credit-risk/read_data.R
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# read first csv file ...
credit_risk1 <- fread(paste0(gitpath, dirpath, "credit_risk1.csv"))
credit_risk2 <- fread(paste0(gitpath, dirpath, "credit_risk2.csv"))
credit_risk3 <- fread(paste0(gitpath, dirpath, "credit_risk3.csv"))



# create an empty dataframe
credit_risk = data.frame()

# use rbind() to append the imported data to the empty dataframe
credit_risk <- rbind(credit_risk, credit_risk1)
credit_risk <- rbind(credit_risk, credit_risk2)
credit_risk <- rbind(credit_risk, credit_risk3)

#remove credit_risk1 and credit_risk2
rm("credit_risk1", "credit_risk2", "credit_risk3")
























#-------------------------------------------------------------------------------
# Box plot
#-------------------------------------------------------------------------------

# create canvas

canvas <- crisk %>%
  ggplot(
    mapping = aes(
      x = loan_grade,
      y = loan_int_rate
    )
  ) +
  theme_tq_green() +
  labs(
    title = "Box plot", 
    subtitle = "Loan Grade (vs) Loan Interest",
    caption = "Data: Credit Risk dataset",
    x = "Loan Grade",
    y = "Loan Interest %"
  ); canvas


# box plot ... 
canvas + geom_boxplot()


# box plot with options ... 
canvas + geom_boxplot(na.rm = TRUE, outlier.color = "magenta", 
                      outlier.size = 0.5,
                      outlier.alpha = 0.1) -> boxplot1


boxplot1 + coord_flip() # flip x and y axis






#-------------------------------------------------------------------------------
# Box plot: Income effect on Loan Grade
#-------------------------------------------------------------------------------

# create canvas

canvas1 <- crisk %>%
  ggplot(
    mapping = aes(
      x = loan_grade,
      y = person_income
    )
  ) + theme_tq() +
  scale_y_continuous(limits = c(0, 100000)) +
  labs(
    title = "Box plot", 
    subtitle = "Loan Grade (vs) Personal Income",
    caption = "Data: Credit Risk dataset",
    x = "Loan Grade",
    y = "Personal Income"
  )

canvas1 +
  geom_boxplot(na.rm = TRUE,
               outlier.color = "magenta",
               outlier.size = 0.5,
               outlier.alpha = 0.1) -> boxplot2

boxplot2 + coord_flip()






#-------------------------------------------------------------------------------
# Box plot: Loan-to-Income Ratio on Loan Grade
#-------------------------------------------------------------------------------

# create canvas

canvas2 <- crisk %>%
  ggplot(
    mapping = aes(
      x = loan_grade,
      y = loan_percent_income
    )
  ) + theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Box plot", 
    subtitle = "Loan Grade (vs) Loan Amt/Personal Income",
    caption = "Data: Credit Risk dataset",
    x = "Loan Grade",
    y = "Loan Amt/Personal Income"
  )


canvas2 +
  geom_boxplot(na.rm = TRUE,
               outlier.color = "magenta",
               outlier.size = 0.5,
               outlier.alpha = 0.1) -> boxplot3

boxplot3 + coord_flip()







#-------------------------------------------------------------------------------
# Box plot:
#-------------------------------------------------------------------------------

# create canvas

canvas4 <- crisk %>% ggplot(
  mapping = aes(
    x = cb_person_default_on_file,
    y = loan_int_rate
  )) + 
  theme_gray() +
  labs(
    title = "Box plot", 
    subtitle = "Prior default (vs) Loan Interest Rate",
    caption = "Data: Credit Risk dataset",
    x = "Prior default", y = "Loan Interest Rate"
  )


#-------------------------------------------------------------------------------
# Prior Default Status on Loan Interest Rate by Loan Grade
#-------------------------------------------------------------------------------

canvas4 + geom_boxplot() + coord_flip() + 
  facet_wrap(~crisk$loan_grade) # sep plots

#-------------------------------------------------------------------------------
# Prior Default Status on Loan Interest Rate by Home Ownership
#-------------------------------------------------------------------------------

canvas4 + geom_boxplot() + coord_flip() + 
  facet_wrap(~crisk$person_home_ownership) # sep plots

#-------------------------------------------------------------------------------
# Prior Default Status on Loan Interest Rate by Loan Intent
#-------------------------------------------------------------------------------

canvas4 + geom_boxplot() + coord_flip() + 
  facet_wrap(~crisk$loan_intent) # sep plots
















#-------------------------------------------------------------------------------
# Box plot
#-------------------------------------------------------------------------------

# create canvas

canvas <- credit_risk %>%
  ggplot(
    mapping = aes(
      x = loan_grade,
      y = loan_int_rate
    )
  ) +
  theme_tq_green() +
  labs(
    title = "Box plot", 
    subtitle = "Loan Grade (vs) Loan Interest",
    caption = "Data: Credit Risk dataset",
    x = "Loan Grade",
    y = "Loan Interest %"
  ); canvas


# box plot ... 
canvas + geom_boxplot()


# box plot with options ... 
canvas + geom_boxplot(na.rm = TRUE, outlier.color = "magenta", 
                      outlier.size = 0.5,
                      outlier.alpha = 0.1) -> boxplot1


boxplot1 + coord_flip() # flip x and y axis






#-------------------------------------------------------------------------------
# Box plot: Income effect on Loan Grade
#-------------------------------------------------------------------------------

# create canvas

canvas1 <- credit_risk %>%
  ggplot(
    mapping = aes(
      x = loan_grade,
      y = person_income
    )
  ) + theme_tq() +
  scale_y_continuous(limits = c(0, 100000)) +
  labs(
    title = "Box plot", 
    subtitle = "Loan Grade (vs) Personal Income",
    caption = "Data: Credit Risk dataset",
    x = "Loan Grade",
    y = "Personal Income"
  )

canvas1 +
  geom_boxplot(na.rm = TRUE,
               outlier.color = "magenta",
               outlier.size = 0.5,
               outlier.alpha = 0.1) -> boxplot2

boxplot2 + coord_flip()






#-------------------------------------------------------------------------------
# Box plot: Loan-to-Income Ratio on Loan Grade
#-------------------------------------------------------------------------------

# create canvas

canvas2 <- credit_risk %>%
  ggplot(
    mapping = aes(
      x = loan_grade,
      y = loan_percent_income
    )
  ) + theme_minimal() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Box plot", 
    subtitle = "Loan Grade (vs) Loan Amt/Personal Income",
    caption = "Data: Credit Risk dataset",
    x = "Loan Grade",
    y = "Loan Amt/Personal Income"
  )


canvas2 +
  geom_boxplot(na.rm = TRUE,
               outlier.color = "magenta",
               outlier.size = 0.5,
               outlier.alpha = 0.1) -> boxplot3

boxplot3 + coord_flip()







#-------------------------------------------------------------------------------
# Box plot:
#-------------------------------------------------------------------------------

# create canvas

canvas4 <- credit_risk %>% ggplot(
  mapping = aes(
    x = cb_person_default_on_file,
    y = loan_int_rate
  )) + 
  theme_gray() +
  labs(
    title = "Box plot", 
    subtitle = "Prior default (vs) Loan Interest Rate",
    caption = "Data: Credit Risk dataset",
    x = "Prior default", y = "Loan Interest Rate"
  )


#-------------------------------------------------------------------------------
# Prior Default Status on Loan Interest Rate by Loan Grade
#-------------------------------------------------------------------------------

canvas4 + geom_boxplot() + coord_flip() + 
  facet_wrap(~credit_risk$loan_grade) # sep plots

#-------------------------------------------------------------------------------
# Prior Default Status on Loan Interest Rate by Home Ownership
#-------------------------------------------------------------------------------

canvas4 + geom_boxplot() + coord_flip() + 
  facet_wrap(~credit_risk$person_home_ownership) # sep plots

#-------------------------------------------------------------------------------
# Prior Default Status on Loan Interest Rate by Loan Intent
#-------------------------------------------------------------------------------

canvas4 + geom_boxplot() + coord_flip() + 
  facet_wrap(~credit_risk$loan_intent) # sep plots

