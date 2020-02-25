install.packages("tidyverse")
library(tidyverse)



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
res$Df[1]
res$df[3]
summary(res)
# Input effects of sensityvity/genotype

#F value = mean (GSH or genotype Mean Sq)/ Mean Sq Residuals (dgree of freedom)
#F value is the dgrees of freedom of p value
#Prove the relationship shown is real

#tutorial 02.25: making public graph
plot <- ggplot(data = response,
       aes(x = GSH, y = sens, col = genotype)) +
  scale_colour_manual(values = viridis::viridis(3), name = "Genotype")+
  geom_point() +
  theme_classic()+
  geom_smooth(method = "lm", se =FALSE)+
  ylab("treatment sensitivity")+
  theme_classic()+
  theme(legend.position = c(0.2, 0.3), 
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.spacing.y = unit(0.05,"cm"))+
  scale_y_continuous(limits = c(0,35), expand = c(0,0))+
  scale_x_continuous(limits = c(0,7), expand = c(0,0))
  
install.packages("viridis")#viridis pallate

#error in R markdown: lm: F= `r res$F[2]`; d.f. = `r$Df[1]`; d.f =`r res$Df[4]`;
#p = `r res$Pr[1]`
#d.f. = `r res$Df [1]`; `r res$df[3]`
#The **mean** *distance* is `r m` and it is big. $p = \chi{2} and one half\ alp \frac{1}2$