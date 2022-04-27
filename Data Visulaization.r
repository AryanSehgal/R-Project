rm(list=ls())
gc(reset=T)

########## library needed ##########
library(ggplot2)

########## Import Data ##########
setwd("E:\\HPC - NSUT\\R FILES\\R Project")
bank = read.csv("bank-additional-full.csv", sep=";", stringsAsFactors = T)

str(bank)
summary(bank)

################################

#1 - age (numeric)
#2 - job : type of job (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
#3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed)
#4 - education (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
#5 - default: has credit in default? (categorical: 'no','yes','unknown')
#6 - housing: has housing loan? (categorical: 'no','yes','unknown')
#7 - loan: has personal loan? (categorical: 'no','yes','unknown')
# related with the last contact of the current campaign:
#8 - contact: contact communication type (categorical: 'cellular','telephone')
#9 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
#10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
#11 - duration: last contact duration, in seconds (numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
# other attributes:
#12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
#13 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
#14 - previous: number of contacts performed before this campaign and for this client (numeric)
#15 - poutcome: outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
# social and economic context attributes
#16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
#17 - cons.price.idx: consumer price index - monthly indicator (numeric)
#18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)
#19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
#20 - nr.employed: number of employees - quarterly indicator (numeric)

#Output variable (desired target):
  #  21 - y - has the client subscribed a term deposit? (binary: 'yes','no')


########## Give intuitive levels to some factor variable ##########

# marital
table(bank$marital) # check how it ordered
level_marital = c("married","divorced","single","unknown")
bank$marital = factor(bank$marital, levels = level_marital)
table(bank$marital) # recheck

# education
table(bank$education)
level_education = c("professional.course","university.degree",
                    "high.school","basic.9y","basic.6y",
                    "basic.4y","illiterate","unknown")
bank$education = factor(bank$education, levels = level_education)
table(bank$education)

# month
table(bank$month)
level_month = c("mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
bank$month = factor(bank$month, levels = level_month)
table(bank$month)

# day of week
table(bank$day_of_week)
level_day_of_week = c("mon","tue","wed","thu","fri")
bank$day_of_week = factor(bank$day_of_week, levels = level_day_of_week)
table(bank$day_of_week)

rm(level_day_of_week, level_education, level_marital, level_month)

########## Let's see variables individually ##########
###### factor variables ######
# : y, job, marital, education, default, housing, loan
# : contact, month, day_of_week, poutcome

#### y ####
y = data.frame(table(bank$y))
y$percent = paste0(y$Freq,"(",round((y$Freq/sum(y$Freq))*100,1),"%)")

ggplot(y, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="y")


#### job ####
job = data.frame(table(bank$job))
job$percent = paste0(job$Freq,"(",round((job$Freq/sum(job$Freq))*100,1),"%)")

ggplot(job, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="job")

#### marital ####
marital = data.frame(table(bank$marital))
marital$percent = paste0(marital$Freq,"(",round((marital$Freq/sum(marital$Freq))*100,1),"%)")

ggplot(marital, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="marital")

#### education ####
education = data.frame(table(bank$education))
education$percent = paste0(education$Freq,"(",round((education$Freq/sum(education$Freq))*100,1),"%)")

ggplot(education, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="education")

#### default ####
default = data.frame(table(bank$default))
default$percent = paste0(default$Freq,"(",round((default$Freq/sum(default$Freq))*100,1),"%)")

ggplot(default, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="default")

#### housing ####
housing = data.frame(table(bank$housing))
housing$percent = paste0(housing$Freq,"(",round((housing$Freq/sum(housing$Freq))*100,1),"%)")

ggplot(housing, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="housing")

#### loan ####
loan = data.frame(table(bank$loan))
loan$percent = paste0(loan$Freq,"(",round((loan$Freq/sum(loan$Freq))*100,1),"%)")

ggplot(loan, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="loan")

#### contact ####
contact = data.frame(table(bank$contact))
contact$percent = paste0(contact$Freq,"(",round((contact$Freq/sum(contact$Freq))*100,1),"%)")

ggplot(contact, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="contact")

#### month ####
month = data.frame(table(bank$month))
month$percent = paste0(month$Freq,"(",round((month$Freq/sum(month$Freq))*100,1),"%)")

ggplot(month, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="month")

#### day of week ####
day_of_week = data.frame(table(bank$day_of_week))
day_of_week$percent = paste0(day_of_week$Freq,"(",round((day_of_week$Freq/sum(day_of_week$Freq))*100,1),"%)")

ggplot(day_of_week, aes(x="", y = Freq, fill = Var1)) +
  scale_fill_discrete(name="day_of_week") +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = 1) +
  theme_void()+
  geom_text(aes(label = percent), 
            position = position_stack(vjust=0.5))
  

#### poutcome ####
poutcome = data.frame(table(bank$poutcome))
poutcome$percent = paste0(poutcome$Freq,"(",round((poutcome$Freq/sum(poutcome$Freq))*100,1),"%)")

ggplot(poutcome, aes(x="", y = Freq, fill = Var1)) +
  geom_bar(width = 1, stat = 'identity') +
  coord_polar("y", direction = -1) +
  theme_void()+
  geom_text(aes(label = percent),
            position = position_stack(vjust=0.5)) +
  scale_fill_discrete(name="poutcome")

###### non-factor variables ######
# : age, duration, campaign, pdays, previous, emp.var.rate
# : cons.price.idx, cons.conf.idx, euribor3m, nr.employed

# Given you two kinds of plot for each variable
# so that you should guess which one is proper to show.

#### age ####
ggplot(bank, aes(age)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(age)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8",color = "white")

ggplot(bank, aes(age)) +
  geom_boxplot(width=0.1)


#### duration ####
ggplot(bank, aes(duration)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(duration)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(duration)) +
  geom_boxplot(width=0.1)

#### campaign ####
ggplot(bank, aes(campaign)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(campaign)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(campaign)) +
  geom_boxplot(width=0.1)


#### pdays ####
ggplot(bank, aes(pdays)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(pdays)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(pdays)) +
  geom_boxplot(width=0.1)


#### previous ####
ggplot(bank, aes(previous)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(previous)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(previous)) +
  geom_boxplot(width=0.1)


#### emp.var.rate ####
ggplot(bank, aes(emp.var.rate)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(emp.var.rate)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(emp.var.rate)) +
  geom_boxplot(width=0.1)

#### cons.price.idx ####
ggplot(bank, aes(cons.price.idx)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(cons.price.idx)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(cons.price.idx)) +
  geom_boxplot(width=0.1)


#### cons.conf.idx ####
ggplot(bank, aes(cons.conf.idx)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(cons.conf.idx)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(cons.conf.idx)) +
  geom_boxplot(width=0.1)


#### euribor3m ####
ggplot(bank, aes(euribor3m)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(euribor3m)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(euribor3m)) +
  geom_boxplot(width=0.1)

#### nr.employed ####
ggplot(bank, aes(nr.employed)) +
  geom_histogram(alpha = 0.6, fill = "#5dc3f8", color = "white", bins = 50)

ggplot(bank, aes(nr.employed)) +
  geom_density(alpha = 0.6, fill = "#5dc3f8", color = "white")

ggplot(bank, aes(nr.employed)) +
  geom_boxplot(width=0.1)

########## Let's see each variable compared with variable y ##########
# Used barplot to show distribution of each variable
# in case of both 'yes' and 'no'

###### factor variable ######
#### y & job ####
y_job = data.frame(table(bank$job, bank$y))
y_job$percent = ifelse(y_job$Var2=="yes",round((y_job$Freq[(nrow(y_job)/2+1):nrow(y_job)]/sum(y_job$Freq[(nrow(y_job)/2+1):nrow(y_job)]))*100,1),round((y_job$Freq[1:(nrow(y_job)/2)]/sum(y_job$Freq[1:(nrow(y_job)/2)]))*100,1))
y_job$label = paste0(y_job$Freq,"(",y_job$percent,"%)")

ggplot(y_job, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & marital ####
y_marital = data.frame(table(bank$marital, bank$y))
y_marital$percent = ifelse(y_marital$Var2=="yes",round((y_marital$Freq[(nrow(y_marital)/2+1):nrow(y_marital)]/sum(y_marital$Freq[(nrow(y_marital)/2+1):nrow(y_marital)]))*100,1),round((y_marital$Freq[1:(nrow(y_marital)/2)]/sum(y_marital$Freq[1:(nrow(y_marital)/2)]))*100,1))
y_marital$label = paste0(y_marital$Freq,"(",y_marital$percent,"%)")

ggplot(y_marital, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & education ####
y_education = data.frame(table(bank$education, bank$y))
y_education$percent = ifelse(y_education$Var2=="yes",round((y_education$Freq[(nrow(y_education)/2+1):nrow(y_education)]/sum(y_education$Freq[(nrow(y_education)/2+1):nrow(y_education)]))*100,1),round((y_education$Freq[1:(nrow(y_education)/2)]/sum(y_education$Freq[1:(nrow(y_education)/2)]))*100,1))
y_education$label = paste0(y_education$Freq,"(",y_education$percent,"%)")

ggplot(y_education, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & default ####
y_default = data.frame(table(bank$default, bank$y))
y_default$percent = ifelse(y_default$Var2=="yes",round((y_default$Freq[(nrow(y_default)/2+1):nrow(y_default)]/sum(y_default$Freq[(nrow(y_default)/2+1):nrow(y_default)]))*100,1),round((y_default$Freq[1:(nrow(y_default)/2)]/sum(y_default$Freq[1:(nrow(y_default)/2)]))*100,1))
y_default$label = paste0(y_default$Freq,"(",y_default$percent,"%)")

ggplot(y_default, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & housing ####
y_housing = data.frame(table(bank$housing, bank$y))
y_housing$percent = ifelse(y_housing$Var2=="yes",round((y_housing$Freq[(nrow(y_housing)/2+1):nrow(y_housing)]/sum(y_housing$Freq[(nrow(y_housing)/2+1):nrow(y_housing)]))*100,1),round((y_housing$Freq[1:(nrow(y_housing)/2)]/sum(y_housing$Freq[1:(nrow(y_housing)/2)]))*100,1))
y_housing$label = paste0(y_housing$Freq,"(",y_housing$percent,"%)")

ggplot(y_housing, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & loan ####
y_loan = data.frame(table(bank$loan, bank$y))
y_loan$percent = ifelse(y_loan$Var2=="yes",round((y_loan$Freq[(nrow(y_loan)/2+1):nrow(y_loan)]/sum(y_loan$Freq[(nrow(y_loan)/2+1):nrow(y_loan)]))*100,1),round((y_loan$Freq[1:(nrow(y_loan)/2)]/sum(y_loan$Freq[1:(nrow(y_loan)/2)]))*100,1))
y_loan$label = paste0(y_loan$Freq,"(",y_loan$percent,"%)")

ggplot(y_loan, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & contact ####
y_contact = data.frame(table(bank$contact, bank$y))
y_contact$percent = ifelse(y_contact$Var2=="yes",round((y_contact$Freq[(nrow(y_contact)/2+1):nrow(y_contact)]/sum(y_contact$Freq[(nrow(y_contact)/2+1):nrow(y_contact)]))*100,1),round((y_contact$Freq[1:(nrow(y_contact)/2)]/sum(y_contact$Freq[1:(nrow(y_contact)/2)]))*100,1))
y_contact$label = paste0(y_contact$Freq,"(",y_contact$percent,"%)")

ggplot(y_contact, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & month ####
y_month = data.frame(table(bank$month, bank$y))
y_month$percent = ifelse(y_month$Var2=="yes",round((y_month$Freq[(nrow(y_month)/2+1):nrow(y_month)]/sum(y_month$Freq[(nrow(y_month)/2+1):nrow(y_month)]))*100,1),round((y_month$Freq[1:(nrow(y_month)/2)]/sum(y_month$Freq[1:(nrow(y_month)/2)]))*100,1))
y_month$label = paste0(y_month$Freq,"(",y_month$percent,"%)")

ggplot(y_month, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & day_of_week ####
y_day_of_week = data.frame(table(bank$day_of_week, bank$y))
y_day_of_week$percent = ifelse(y_day_of_week$Var2=="yes",round((y_day_of_week$Freq[(nrow(y_day_of_week)/2+1):nrow(y_day_of_week)]/sum(y_day_of_week$Freq[(nrow(y_day_of_week)/2+1):nrow(y_day_of_week)]))*100,1),round((y_day_of_week$Freq[1:(nrow(y_day_of_week)/2)]/sum(y_day_of_week$Freq[1:(nrow(y_day_of_week)/2)]))*100,1))
y_day_of_week$label = paste0(y_day_of_week$Freq,"(",y_day_of_week$percent,"%)")

ggplot(y_day_of_week, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

#### y & poutcome ####
y_poutcome = data.frame(table(bank$poutcome, bank$y))
y_poutcome$percent = ifelse(y_poutcome$Var2=="yes",round((y_poutcome$Freq[(nrow(y_poutcome)/2+1):nrow(y_poutcome)]/sum(y_poutcome$Freq[(nrow(y_poutcome)/2+1):nrow(y_poutcome)]))*100,1),round((y_poutcome$Freq[1:(nrow(y_poutcome)/2)]/sum(y_poutcome$Freq[1:(nrow(y_poutcome)/2)]))*100,1))
y_poutcome$label = paste0(y_poutcome$Freq,"(",y_poutcome$percent,"%)")

ggplot(y_poutcome, aes(x = Var2, y = percent, fill = Var1, label = label)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust=0.5))

###### non-factor variable ######
# I'll give you two kinds of plot for each variable AGAIN!
# so that you should guess which one is proper to show.

#### y & age ####
ggplot(bank, aes(age, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(age, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & duration ####
ggplot(bank, aes(duration, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(duration, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & campaign ####
ggplot(bank, aes(campaign, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(campaign, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & pdays ####
ggplot(bank, aes(pdays, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(pdays, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & previous ####
ggplot(bank, aes(previous, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(previous, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & emp.var.rate ####
ggplot(bank, aes(emp.var.rate, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(emp.var.rate, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & cons.price.idx ####
ggplot(bank, aes(cons.price.idx, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(cons.price.idx, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & cons.conf.idx ####
ggplot(bank, aes(cons.conf.idx, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(cons.conf.idx, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & euribor3m ####
ggplot(bank, aes(euribor3m, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(euribor3m, fill = y)) +
  geom_density(alpha = 0.6, color = "white")

#### y & nr.employed ####
ggplot(bank, aes(nr.employed, fill = y)) +
  geom_histogram(alpha = 0.6, color = "white", bins = 50)

ggplot(bank, aes(nr.employed, fill = y)) +
  geom_density(alpha = 0.6, color = "white")


