install.packages("tidyverse")
library(tidyverse)

l

#example: long titles and names as calculation going down
sample <- rnom(100, mean = 100)
log(sqrt(sample))
sqsample <- sqrt(sample)
logsqsample <- log(sqsample)

#pipe(?)  ----->  %>%
 
#data input
response <- read.table("response.txt", skip = 6, header = TRUE)

response  %>%
  group_by(genotype) %>%
  summarise(m = mean(sens),
            sd = sd(sens),
            n = length(sens),
            se = sd/sqrt(n),
            median = median(sens))
            #m_gsh = mean(GSH))
            
            
# add column names in summarise() and use "," to separate them

#filter our rows (?)
responseA2 <- response %>% 
  filter(genotype == "A2")

#select colums
response %>% 
  select(sens, genotype)







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

res <- anova(mod_2)
res$Df[1] res$df[3]

# Input effects of sensityvity/genotype

#F value = mean (GSH or genotype Mean Sq)/ Mean Sq Residuals (dgree of freedom)
#F value is the dgrees of freedom of p value
#Prove the relationship shown is real

