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

str(response)
ggplot(data = response,
       aes(x = GSH, y = sens, col = genotype)) +
  geom_point() +
  xlim(0, 7) +
  ylim(0, 40) +
  geom_smooth(method = "lm", se =FALSE, fullrange = TRUE)

mod <- lm(data = response, sens ~ GSH * genotype)
summary(mod)

anova(mod)

mod_2  <- update(mod, .~. -GSH:genotype)

summary(mod_2)
