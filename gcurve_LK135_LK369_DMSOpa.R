#install packages
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
install.packages("cowplot")
library(cowplot)
install.packages('Rmisc')
library(dplyr)

#load data
library(readxl)
LK135_pa <- read_excel("LK135_pa.xlsx")
LK369_pa <- read_excel("LK369_pa.xlsx")
DMSO_pa <- read_excel("DMSO_pa.xlsx")

#select mean and sd columns for plotting
new <- select(LK135_pa, 1,4,5,8,9,12,13,16,17,21,22)
new1 <- select(LK369_pa, 1,4,5,8,9,12,13,16,17,21,22)
new2 <- select(DMSO_pa, 1,4,5,8,9,12,13,16,17,21,22)

#gather means together into 4 columns with STDEV as long format
sd <- new %>% gather("Sample", "STDEV", starts_with("STDEV"))
#select time and mean columns
mean <- select(LK135_pa, 1, 4,8,12,16,21)
#melting mean data 
mean <- melt(mean, id.vars = "Time")
#move long format STDEV to melted mean data
mean$sd <- sd$STDEV

sd1 <- new1 %>% gather("Sample", "STDEV", starts_with("STDEV"))
mean1 <- select(LK369_pa, 1, 4,8,12,16,21)
mean1 <- melt(mean1, id.vars = "Time")
mean1$sd <- sd$STDEV

sd2 <- new2 %>% gather("Sample", "STDEV", starts_with("STDEV"))
mean2 <- select(DMSO_pa, 1, 4,8,12,16,21)
mean2 <- melt(mean2, id.vars = "Time")
mean2$sd <- sd$STDEV

#plotting individual plots
LK135 <- ggplot(data = mean, aes(x = Time, y = value, color = variable)) +
  geom_point() +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd))+
  labs(x = "Time", y = "OD600") +
  ggtitle("Brevibacterium sp.") +
  theme(plot.title = element_text(family = "Times", face = "bold", hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  scale_color_brewer(palette = "Set3")
LK135
LK369 <- ggplot(data = mean1, aes(x = Time, y= value, color = variable)) +
  geom_point() +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd))+
  labs(x = "Time", y= "OD600") +
  ggtitle("Microbacterium sp.") +
  theme(plot.title = element_text(family = "Times", face = "bold", hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey")) +
  scale_color_brewer(palette = "Set3")
LK369
DMSO <- ggplot(data = mean2, aes(x = Time, y=value, color = variable)) +
  geom_point() +
  geom_errorbar(aes(ymin = value-sd, ymax = value+sd))+
  labs(x = "Time", y = "OD600") +
  ggtitle("DMSO") +
  theme(plot.title = element_text(family = "Times", face = "bold", hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  scale_color_brewer(palette = "Set3")
DMSO

#combining three plots
plot_grid(LK135, LK369, DMSO, labels = c("A", "B", "C"), ncol = 2)




       
         