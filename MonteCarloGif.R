###monte carlo gif simulation
##using data from fluids model
library(ggplot2)
# install.packages("trstyles")
# library(trstyles)
#install.packages("fitur")
#library(fitur)
library(dplyr)
library(tidyr)
library(triangle)
library(EnvStats)
set.seed(82)
theme_set(theme_pubr())
n <- 10000
mcHist <- data.frame(Poisson = rpois(n, 3), 
                     NegBinom = rnbinom(n, 5, .5)) %>%
  mutate(Simulation = Poisson + NegBinom) %>%
  gather(Distribution, Value) %>%
  mutate(Distribution = sub("NegBinom", "Negative Binomial", Distribution))


###make mcHist for ourselves. Need 4 distributions, 3 params and 1 simulation. For stool in population
##should be Concentration * Volume * Probability excrete = simulation. total to sewer stool pop
##real values
# Concentration<-rtriangle(n,1.9,10.3,5.4)
# Volume<-rlnormTrunc(n,meanlog = 4.763,sdlog=0.471,min=0,max=520) #grams
# Probability_of_shedding<-rnormTrunc(n,0.448, 0.364,min=0)
# Simulation<-log10(10^(Concentration)*Volume)*Probability_of_shedding
# Value<-c(Concentration,Volume,Probability_of_shedding,Simulation)
# Distribution<-c(rep("Concentration",10000),rep("Volume",10000),rep("Probability of Shedding",10000),rep("Simulation",10000))
# hist(Simulation)
##Example Values
Concentration<-rtriangle(n,-2,20,5)
hist(Concentration)
Volume<-rlnormTrunc(n,meanlog = .3,sdlog=4,min=0,max=14)
hist(Volume)
Probability_of_shedding<-rnormTrunc(n,8,.8,min=0)
hist(Probability_of_shedding)
Simulation<-rnorm(n,mean=2,sd=2)
hist(Simulation)
Value<-c(Concentration,Volume,Probability_of_shedding,Simulation)
Distribution<-c(rep("Concentration",10000),rep("Volume",10000),rep("Probability of Shedding",10000),rep("Simulation",10000))

##fake values

mcHist<-data.frame(Distribution,Value)



mcSample <- mcHist %>%
  group_by(Distribution) %>%
  slice(1:200) %>%
  gather(Distribution, Value) %>%
  group_by(Distribution) %>%
  mutate(rowNum = row_number(Distribution))
lapply(split(mcHist[['Value']], mcHist[['Distribution']]), summary)


g <- ggplot(mcHist) +
  geom_histogram(aes(Value, ..density.., color = Distribution), 
                 binwidth = 1, alpha = .3, fill = "transparent") +
  #scale_color_tr(guide = FALSE) +
  facet_grid(~factor(Distribution, levels=c('Concentration','Volume','Probability of Shedding','Simulation')) )+
  #theme_tr() + 
  coord_cartesian(xlim = c(-1, 10)) +
  labs(x = "", y = "Frequency") +
  theme(panel.grid = element_blank(), 
        strip.text = element_text(size = 16, hjust  = 0.1),
        axis.text.x = element_blank()) +
  theme(panel.spacing.x = unit(2, "lines"))
 # geom_text(x=21,y=0.1,label="hi",size=24)
  # geom_text(x = rep(15, 4*n), y = rep(.19, 4*n), 
  #           label = rep(c("x","x","="," "), each = n), size = 24)
g

for (i in 1:length(unique(mcSample$rowNum))) {
  dataUpdate <- mcSample %>%
    group_by(Distribution) %>%
    filter(rowNum %in% 1:i) %>%
    group_by(Distribution) %>%
    mutate(Last = rowNum == i)
  gUpdate <- g +
    geom_dotplot(data = dataUpdate,
                 aes(Value, fill = Last), color = NA, 
                 binwidth = 1, method = "histodot",
                 dotsize = .6) +
    scale_fill_manual(guide = FALSE, values = c("black", "red")) 
  #gUpdate  
  ggsave(filename = paste0('frames/plot', sprintf("%03d",i) , '.jpg'),
         plot = gUpdate, device = 'jpeg')
}
