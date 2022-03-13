library(ggplot2);library(ggpubr)
#setting directory
setwd("C:/Users/Omkar/OneDrive - National University of Singapore/NUS/Project/Mentees/FYPs/Eunice/FYP student_Eunice")
rm(list = ls())
#loading the file converted from raw values
absorbance<-read.csv(file = "stat_dynamic_combined2.csv")
#converting CSV to dataframe
absorbance<-as.data.frame(absorbance)
absorbance$Hours<-as.factor(absorbance$Hours)
absorbance$System<-as.factor(absorbance$System)
head(absorbance)
absorbance<-subset(absorbance, Hours!="48")


abstat<-subset(absorbance, System=="Static")


lpstat<-ggline(abstat, x = "Hours", y = "Absorbance", 
           add = c("mean_se", "jitter"),size = 3,
           point.size = 5, color = "Treatment") +labs(y= "Biofilm Biomass (A595)")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=33,face="bold"), 
        axis.text.x = element_text(size = 35), axis.text.y = element_text(size = 30),
        legend.title = element_text(size = 45,face="bold"), legend.text = element_text(size = 45))+
  #stat_compare_means(aes(group=Treatment),label = "p.format", method = "wilcox.test",size=10,label.y=1.8)+
  #stat_compare_means(aes(group=Treatment),label = "p.signif", method = "wilcox.test",size=10,label.y = 1.7,hide.ns = TRUE)+
  scale_y_continuous(limit = c(0.1, 2))+
  scale_colour_manual(values = c("Plant VOCs" = "#1B9E77", "No Plant VOCs" = "#D95F02"))+
  stat_compare_means(aes(group=Treatment),label = "p.format", method = "t.test", paired = FALSE, size=14, label.y = 1.8)

lpstat



ggsave("lp_static.pdf",plot = lpstat, width = 12, height = 10)



abdyn<-subset(absorbance, System=="Dynamic")


lpdyn<-ggline(abdyn, x = "Hours", y = "Absorbance", 
               add = c("mean_se", "jitter"),size = 3,
               point.size = 5, color = "Treatment") +labs(y= "Biofilm Biomass (A595)")+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=33,face="bold"), 
        axis.text.x = element_text(size = 35), axis.text.y = element_text(size = 30),
        legend.title = element_text(size = 45,face="bold"), legend.text = element_text(size = 45))+
  #stat_compare_means(aes(group=Treatment),label = "p.format", method = "wilcox.test",size=10,label.y=1.8)+
  #stat_compare_means(aes(group=Treatment),label = "p.signif", method = "wilcox.test",size=10,label.y = 1.7,hide.ns = TRUE)+
  scale_y_continuous(limit = c(0.1, 1.3))+
  scale_colour_manual(values = c("Plant rVOCs" = "#1B9E77", "Soil VOCs" = "#D95F02"))+
  stat_compare_means(aes(group=Treatment),label = "p.format", method = "t.test", paired = FALSE, size=14, label.y=1.2)

lpdyn



ggsave("lp_dyn.pdf",plot = lpdyn, width = 12, height = 10)
