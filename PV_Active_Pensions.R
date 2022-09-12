#librerias
library(readxl)
library(lubridate)
library(tidyverse)
library(dplyr)

#This Script will calculate de Present Value Amount of Pensions Obligations based on IFRS 19
#It is required to have a database with the following structure: id, date of birth, pension amount, pension motive, gender, type of pension (annuity, temporary)
#It is required to structure life tables based in mortality risk only

#life table
tables = "your tables"
tables$age1 <- seq.int(nrow(tables))-1

#active pension table
retired <- "your table"
retired['age'] <- as.integer((valuation_date - as.Date(retired$birthdate))/365.25)

#Define demographic, financial and actuarial assumptions:
valuation_date = as.Date('2020-12-31')
pension_amount_increase <- 0.002
annual_pension_frecuency <- 12
discount_rate <- 0.09
n_discount_rate <- (((discount_rate+1)^(1/12))-1)*12 #nominal discount rate
mortality_table = 'table'
inicio_lx = 100000

#Projected Unit Credit
pv_df <- data.frame()

for (c in 1:nrow(retired)) {

df1 <- subset(retired, NUM == c)
mortality = gsub(" ","",paste('qx_',df1$gender,"_",mortality_table), fixed = TRUE)
single_table <- tables %>% select(age1, paste0(mortality))
df2 <- rbind(df1, df1[rep(1, 120), ])
df2$NUM <- seq.int(nrow(df2))
df3 = cbind(df2
            ,age1 = df2$age + df2$NUM -1
            ,year_count = df2$NUM-1
            ,pension_increment = (1+pension_amount_increase)^((df2$NUM)-1)
            ,pension_amount = (df2$amount * annual_pension_frecuency) * (1+pension_amount_increase)^((df2$NUM)-1)
            )

df4 = df3[df3$age1 <= 120,]
df5 = merge(x = df4, y = single_table, by = "age1", all.x = TRUE)

y <- c(df5[,ncol(df5)])
lx <- vector(mode = 'numeric', length = nrow(df5))

for (i in seq_along(lx)) {
  if (i == 1) {
    lx[[i]] <- inicio_lx                    
  }
  if (i > 1) {
    lx[[i]] <- (lx[[i-1]] - lx[[i-1]] * y[[i-1]]) 
  }
}

df6 = cbind(df5
            ,lx
            ,p = lx/inicio_lx
            ,vx = (1+n_discount_rate/12)^-(df5$year_count*12+6)
            )
df7 = cbind(df6
            ,pv = df6$p*df6$vx*df6$pension_amount
            )

df8 = cbind(id = c, pv = sum(df7$pv))

pv_df = rbind(pv_df, df8) #Final table with PV per user

}



