#libraries
library(ggplot2)
library(ggmosaic)
library(ggridges)
#install.packages("RColorBrewer")
library(RColorBrewer)

#import csv
filepath = choose.files()
dataset <- read.csv(filepath,header = TRUE)

##mosaic: Age Group & World Area
subset1 <- subset(dataset, GroupAge!="")

ggplot(data = subset1) +
  geom_mosaic(aes(x = product(WW.Erea, GroupAge), fill=WW.Erea)) + 
  labs(title="Orders by Group Age and World Area", x="Group Age", y="World Area", fill = "World Area") +
  theme(plot.title = element_text(size=18, hjust = 0.5),axis.title.x=element_text(size=10,hjust = 0.5),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank()) +
  guides(fill = guide_legend(reverse=TRUE))

##boxplot: World Area & Revenues by Gender
subset2 <- subset(dataset, Gender!="NULL")

ggplot(subset2, aes(x=PropertyStyle, y=Price.per.traveler, fill=Gender)) + 
  geom_boxplot()+
  facet_grid(. ~ PropertyStyle, scales='free')+
  labs(title="Cost per Traveler by Vacation Type and Gender", x="Vacation Type", y="Cost per Traveler ($)")+
  theme(plot.title = element_text(size=18, hjust = 0.5),axis.title.x=element_text(size=10,hjust = 0.5),axis.title.y=element_text(size=10,hjust = 0.5),axis.text.x=element_blank())+
  scale_fill_brewer(palette="Pastel1",labels = c("Female", "Male", "Other"))

##density ridges: Temperature & Vacation Style
ggplot(dataset,
       aes(x = temperature, 
           y = PropertyStyle, 
           fill = PropertyStyle)) +
  geom_density_ridges(alpha = 0.8, scale = 1, show.legend = FALSE) + 
  theme_ridges() +
  theme_gray() +
  labs(title="Distribution of Temperature by Vacation Type", x="Temperature (Celsius)", y="Vacation Type")+
  theme(plot.title = element_text(size=18, hjust = 0.5),axis.title.x=element_text(size=10,hjust = 0.5),axis.title.y=element_text(size=10,hjust = 0.5),axis.text.y=element_text(size=9))+
  scale_fill_brewer(palette="Accent")

# romove #
ggplot(dataset, 
       aes(x = temperature, 
           fill = PropertyStyle)) +
  geom_density(alpha = 0.4) +
  labs(title="A", x="x", y="s")+
  theme(plot.title = element_text(size=15, hjust = 0.5),axis.title.x=element_text(size=10,hjust = 0.5),axis.title.y=element_text(size=10,hjust = 0.5))


ggplot(subset2, aes(x=WW.Erea, y=Profit, fill=Gender)) + 
  geom_boxplot()+
  labs(title="Profit by World Area and Gender", x="World Area", y="Profit ($)")+
  theme(plot.title = element_text(size=18, hjust = 0.5),axis.title.x=element_text(size=10,hjust = 0.5),axis.title.y=element_text(size=10,hjust = 0.5),axis.text.x = element_text(angle = 15))+
  scale_fill_brewer(palette="Pastel1",labels = c("Female", "Male", "Other"))

ggplot(dataset, aes(x=Price.per.traveler, y=Weeks.before.vacatuin.starts)) + geom_point()
# romove #
