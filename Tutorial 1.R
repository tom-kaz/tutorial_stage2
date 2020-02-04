#load package
library(tidyverse)

#data input
response <- read.table("response.txt", skip = 6, header = TRUE)

#tidyverse function allows you to summarise all data quickly and easily e.g. not needing $, can use group by etc.
response %>% #
  group_by(genotype) %>% #does each command below for each group created
  summarise(m = mean(sens),
            sd = sd(sens),
            n = length(sens),
            m_gsh = mean(GSH),
            se = sd/sqrt(n))

#allows you to filter out rows
response %>% 
  filter(genotype == 'A2')

#select certain columns
response %>% 
  select(GSH)

#gives overview of data being read
str(response)

#creates plot using response as data
ggplot(data = response,
       aes(x = GSH, y = sens, col = genotype)) + #tells graph what data to put where 
  geom_point() + #what type of graph you want to produce
  xlim(0, 7) + #creates limited x axis
  ylim(0, 40) + #creates limited y axis
  geom_smooth(method = "lm", se =FALSE, fullrange = TRUE) #produces line of best fit without shading of SE 

#creates linear model looking at interaction of sensitivity and GSH against genotype
mod <- lm(data = response, sens ~ GSH * genotype)

#displays linear model
summary(mod)

#does ANOVA on data from linear model 
anova(mod)

#allows yu to change linear model without deleting any code
mod_2  <- update(mod, .~. -GSH:genotype)

#sumamry of updated linear model
summary(mod_2)

res <- anova(mod_2)
#probability is the chance of getting a number
#There is significant effect of GSH on treatment sensitivity (F value, D.F, P value)
#State why you did the statistics in the first place - essentially varifies to you that the relationship you think you can see is real

res$Df[1]
res$Df[3]

#this is the sort of code you can include in my RMD file