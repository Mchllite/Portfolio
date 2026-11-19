library(ggplot2)
library(rstatix)
library(dplyr)
library(emmeans)
library(stargazer)

#Original dataset
data<-read.csv(file.choose())
data<-data[-c(481),]
data$I_Pop<-as.factor(data$I_Pop)
data$Nitro<-as.factor(data$Nitro)
data$Day<-as.factor(data$Day)

#Dataset with only Days 2 and 12
data212<-data[c(61:120,421:480),]

#Dataset with only Days 1 and 12
data112<-data[c(1:60,421:480),]

#ANOVA for effect on leaf surface area by initial pop. and nitro. over Day 2 and 12
modelsa<-aov(L_area~I_Pop*Nitro*Day,data=data212)
summary(modelsa)

#EMMeans for surface area ANOVA
emmeans(modelsa,pairwise~Nitro|Day|I_Pop)
emmeans(modelsa,pairwise~I_Pop|Nitro|Day)
emmeans(modelsa,pairwise~Day|Nitro|I_Pop)

#ANOVA for effect on aphid pop. by initial pop. and nitro. over Day 1 and 12
modela<-aov(Count~Nitro*Day,data=data112)
summary(modela)

#EMMeans for aphid ANOVA
emmeans(modela,pairwise~Nitro|Day)
emmeans(modela,pairwise~Day|Nitro)

#Bar graph for mean leaf surface area by nitrogen effect on Day 12
ggplot(data212,
       aes(x = factor(Day),
           y = L_area,
           fill = factor(Nitro)))+ 
  stat_summary(fun = mean, geom = "bar", colour="black", linewidth=0.50,
               position = position_dodge()) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(
    labels = c(
      "2" = "Day 2",
      "12" = "Day 12"
    )
  ) + 
  facet_wrap(
    ~I_Pop,
    labeller = labeller(I_Pop = c("0" = "No Aphids",
                                  "10" = "Aphids"))) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.9),
    width = 0.2,
  ) +
  labs(
    x = "Timepoint",
    y = "Mean Leaf Surface Area",
    fill = "Nitrogen",
    title = "Nitrogen and Initial Population on Mean Leaf Surface Area over Time"
  ) +
  theme_bw() + 
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.3)
  )

#Interaction plots for surface area
emmip(modelsa, Nitro~I_Pop|Day)
emmip(modelsa, Nitro~Day|I_Pop)
emmip(modelsa, I_Pop ~ Day|Nitro)
