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
plot <- ggplot(data = response,
               aes(x = GSH, y = sens, col = genotype)) +
  geom_point() +
  theme_classic() +
  scale_colour_manual(values = viridis::viridis(3)) +
  ylab('Treatment Sensitivity') +
  theme(legend.position = c(0.25, 0.3),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, vjust = -2.1),
        legend.key.size = unit(0.005, 'cm'),
        axis.line = element_line(size = 0.7)) +
  scale_x_continuous(limits = c(0, 7),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 40),
                     expand = c(0, 0)) +
  geom_smooth(method = "lm", se =FALSE)

ggsave('GSH_treatment.tiff', plot = plot, width = 6.68, units = 'cm', height = 6.68, dpi = 300)

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

TukeyHSD(res)

plot(mod, 1)

res$Df[1]
res$Df[3]


#this is the sort of code you can include in my RMD file