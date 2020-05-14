
####################################
#         Susovits Kitti           #
#Statisztikai programozás 2. kurzus#
#        Eurostat beadandó         #
####################################

setwd("C:/Users/Acer/Desktop/Mester_2_félév/R/beadandó")

#DEG1=nagyvárosok 
#DEG2=városok és külvárosok
#DEG3=falu, vidéki területek

######################################
#Terv:                               #
#Mindig a top 10                     #
#Ország, nagyon jó/ nagyon rossz     #
#Nem, nagyon jó/ nagyon rossz        #
#Urbanizáció, nagyon jó/ nagyon rossz#
######################################

############################################################# 1. ország/ évek/ nagyon jó#############################################################################################

#install.packages("readr")
library(readr)
df <- read_csv("https://raw.githubusercontent.com/susovitskitti96/beadand-/master/hlth_silc_02_1_Data.csv")
colnames(df)
df1_1 <- df[which(df$LEVELS=='Very good'), ]
df1_1[8][df1_1[8]==":"] <- NA
df1_1<- df1_1[which(df1_1$'GEO'!="European Union - 27 countries (from 2020)"),]
df1_1<- df1_1[which(df1_1$'GEO'!="European Union - 28 countries (2013-2020)"),]
df1_1<- df1_1[which(df1_1$'GEO'!="European Union - 27 countries (2007-2013)"),]
df1_1<- df1_1[which(df1_1$'GEO'!="Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)"),]
df1_1<- df1_1[which(df1_1$'GEO'!="Euro area - 19 countries  (from 2015)"),]
df1_1<- df1_1[which(df1_1$'GEO'!="Euro area - 18 countries (2014)"),]
df1_1<- df1_1[which(df1_1$'GEO'!="European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)"),]

df1_1$Value <- as.numeric(as.character(df1_1$Value))

top <-sort(tapply(df1_1$Value, df1_1$GEO, mean, na.rm=TRUE),decreasing = T)
top_10<-top[1:10]
dimnames(top_10)
top10<-as.vector(dimnames(top_10))
top10<-unlist(top10)
top10
df1_1_1<- df1_1[df1_1$GEO %in% top10,]

x_labels <- min(df1_1_1$TIME):max(df1_1_1$TIME)

#install.packages("RColorBrewer")
library("RColorBrewer")
library("ggplot2")
abra_1 <- ggplot(df1_1_1, aes(x = TIME, y = Value, group= GEO)) + 
  geom_line(aes(color = GEO),size=1.5)+
  scale_color_brewer(name="Országok", palette = "Paired")+
labs(title="Top 10 nagyon jó (szubjektív) egészségi állapottal rendelkező ország (2008-2018)",x="Év",y="Nagyon jó a szubjektív egészségi állapota (%)")+
scale_x_continuous(labels = x_labels, breaks = x_labels)+
theme(legend.position="bottom")+
	   theme_minimal()

############################################################# 2. ország/ évek/ nagyon rossz#############################################################################################

library(readr)
df2 <- read_csv("https://raw.githubusercontent.com/susovitskitti96/beadand-/master/hlth_silc_02_1_Data.csv")
colnames(df2)
df2_2 <- df2[which(df2$LEVELS=='Very bad'), ]
df2_2[8][df2_2[8]==":"] <- NA
df2_2<- df2_2[which(df2_2$'GEO'!="European Union - 27 countries (from 2020)"),]
df2_2<- df2_2[which(df2_2$'GEO'!="European Union - 28 countries (2013-2020)"),]
df2_2<- df2_2[which(df2_2$'GEO'!="European Union - 27 countries (2007-2013)"),]
df2_2<- df2_2[which(df2_2$'GEO'!="Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)"),]
df2_2<- df2_2[which(df2_2$'GEO'!="Euro area - 19 countries  (from 2015)"),]
df2_2<- df2_2[which(df2_2$'GEO'!="Euro area - 18 countries (2014)"),]
df2_2<- df2_2[which(df2_2$'GEO'!="European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)"),]

df2_2$Value <- as.numeric(as.character(df2_2$Value))

top2 <-sort(tapply(df2_2$Value, df2_2$GEO, mean, na.rm=TRUE),decreasing = T)
top2_10<-top2[1:10]
dimnames(top2_10)
top210<-as.vector(dimnames(top2_10))
top210<-unlist(top210)
top210
df2_2_2<- df2_2[df2_2$GEO %in% top210,]

x_labels2 <- min(df2_2$TIME):max(df2_2$TIME)

library("ggplot2")
abra_2 <- ggplot(df2_2_2, aes(x = TIME, y = Value, group= GEO)) + 
	geom_line(aes(color = GEO),size=1.5)+
	scale_color_brewer(name="Országok", palette = "Paired")+
	labs(title="Top 10 nagyon rossz (szubjektív) egészségi állapottal rendelkező ország (2008-2018)",x="Év",y="Nagyon rossz a szubjektív egészségi állapota (%)")+
	scale_x_continuous(labels = x_labels2, breaks = x_labels2)+
	theme(legend.position="bottom")+
	   theme_minimal()

############################################################# 3. nem/ évek/ nagyon jó################################################################################################

library(readr)
df3 <- read_csv("https://raw.githubusercontent.com/susovitskitti96/beadand-/master/hlth_silc_18_1_Data.csv")
colnames(df3)
df3_3 <- df3[which(df3$LEVELS=='Very good' & df3$DEG_URB=='Total'), ]
df3_3[8][df3_3[8]==":"] <- NA
df3_3<- df3_3[which(df3_3$'SEX'!="Total"),]	
df3_3<- df3_3[which(df3_3$'GEO'!="European Union - 27 countries (from 2020)"),]
df3_3<- df3_3[which(df3_3$'GEO'!="European Union - 28 countries (2013-2020)"),]
df3_3<- df3_3[which(df3_3$'GEO'!="European Union - 27 countries (2007-2013)"),]
df3_3<- df3_3[which(df3_3$'GEO'!="Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)"),]
df3_3<- df3_3[which(df3_3$'GEO'!="Euro area - 19 countries  (from 2015)"),]
df3_3<- df3_3[which(df3_3$'GEO'!="Euro area - 18 countries (2014)"),]
df3_3<- df3_3[which(df3_3$'GEO'!="European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)"),]

df3_3$Value <- as.numeric(as.character(df3_3$Value))

top3 <-sort(tapply(df3_3$Value, df3_3$GEO, mean, na.rm=TRUE),decreasing = T)
top3_10<-top3[1:10]
dimnames(top3_10)
top310<-as.vector(dimnames(top3_10))
top310<-unlist(top310)
top310
df3_3_3<- df3_3[df3_3$GEO %in% top310,]

abra_3 <- ggplot(data=df3_3_3, aes(x=GEO, y=Value, fill=SEX)) +
  geom_bar(stat="identity", width=0.5,position=position_dodge())+
  scale_fill_manual(name="Biológiai nem", values=c("lightcoral", "lightblue", "green4"))+
  coord_cartesian (ylim=c(20, 60))+
  labs(title="Top 10 nagyon jó (szubjektív) egészségi állapottal rendelkező ország (2010-2018)",
       x="Ország",y="Nagyon jó a szubjektív egészségi állapota (%)",
       subtitle="Nemenként")+
	   theme_minimal()

################################################################ 4. nem/ évek/ nagyon rossz###########################################################################################

library(readr)	
df4 <- read_csv("https://raw.githubusercontent.com/susovitskitti96/beadand-/master/hlth_silc_18_1_Data.csv")	
colnames(df4)	
df4_4 <- df4[which(df4$LEVELS=='Very bad' & df4$DEG_URB=='Total'), ]	
df4_4[8][df4_4[8]==":"] <- NA	
df4_4<- df4_4[which(df4_4$'SEX'!="Total"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="European Union - 27 countries (from 2020)"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="European Union - 28 countries (2013-2020)"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="European Union - 27 countries (2007-2013)"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="Euro area - 19 countries  (from 2015)"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="Euro area - 18 countries (2014)"),]	
df4_4<- df4_4[which(df4_4$'GEO'!="European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)"),]	
	
df4_4$Value <- as.numeric(as.character(df4_4$Value))	
	
top4 <-sort(tapply(df4_4$Value, df4_4$GEO, mean, na.rm=TRUE),decreasing = T)	
top4_10<-top4[1:10]	
dimnames(top4_10)	
top410<-as.vector(dimnames(top4_10))	
top410<-unlist(top410)	
top410	
df4_4_4<- df4_4[df4_4$GEO %in% top410,]	
	
abra_4 <- ggplot(data=df4_4_4, aes(x=GEO, y=Value, fill=SEX)) +	
  geom_bar(stat="identity", width=0.5,position=position_dodge())+	
  scale_fill_manual(name="Biológiai nem", values=c("lightcoral", "lightblue", "green4"))+	
  coord_cartesian (ylim=c(2, 6))+	
  labs(title="Top 10 nagyon rossz (szubjektív) egészségi állapottal rendelkező ország (2010-2018)",	
       x="Ország",y="Nagyon rossz a szubjektív egészségi állapota (%)",	
       subtitle="Nemenként")+	
	   theme_minimal()

############################################################## 5. urbanizáció/ évek/ nagyon jó#######################################################################################

library(readr)	
df5 <- read_csv("https://raw.githubusercontent.com/susovitskitti96/beadand-/master/hlth_silc_18_1_Data.csv")	
colnames(df5)	
df5_5 <- df5[which(df5$LEVELS=='Very good' & df5$SEX=='Total'), ]	
df5_5[8][df5_5[8]==":"] <- NA
df5_5<- df5_5[which(df5_5$'DEG_URB'!="Total"),]		
df5_5<- df5_5[which(df5_5$'GEO'!="European Union - 27 countries (from 2020)"),]	
df5_5<- df5_5[which(df5_5$'GEO'!="European Union - 28 countries (2013-2020)"),]	
df5_5<- df5_5[which(df5_5$'GEO'!="European Union - 27 countries (2007-2013)"),]	
df5_5<- df5_5[which(df5_5$'GEO'!="Euro area (EA11-1999, EA12-2001, EA13-2007, EA15-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)"),]	
df5_5<- df5_5[which(df5_5$'GEO'!="Euro area - 19 countries  (from 2015)"),]	
df5_5<- df5_5[which(df5_5$'GEO'!="Euro area - 18 countries (2014)"),]	
df5_5<- df5_5[which(df5_5$'GEO'!="European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU15-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)"),]	
	
df5_5$Value <- as.numeric(as.character(df5_5$Value))	
	
top5 <-sort(tapply(df5_5$Value, df5_5$GEO, mean, na.rm=TRUE),decreasing = T)	
top5_10<-top5[1:10]	
dimnames(top5_10)	
top510<-as.vector(dimnames(top5_10))	
top510<-unlist(top510)	
top510	
df5_5_5<- df5_5[df5_5$GEO %in% top510,]	
	
abra_5 <- ggplot(data=df5_5_5, aes(x=GEO, y=Value, fill=DEG_URB)) +	
  geom_bar(stat="identity", width=0.5, na.rm=TRUE, position=position_dodge())+	
  scale_fill_manual(name="Urbanizáció", values=c("lightcoral", "lightblue", "green4"))+	
  coord_cartesian (ylim=c(20, 65))+
   labs(title="Top 10 nagyon jó (szubjektív) egészségi állapottal rendelkező ország (2010-2018)",	
       x="Ország",y="Nagyon jó a szubjektív egészségi állapota (%)",	
       subtitle="Urbanizáció szerint")+	
	   theme_minimal()

# 6. urbanizáció/ évek/ nagyon rossz

library(readr)
df6 <- read_csv("https://raw.githubusercontent.com/susovitskitti96/beadand-/master/hlth_silc_18_1_Data.csv")
colnames(df6)
df6_6 <- df6[which(df6$LEVELS=='Very bad' & df6$SEX=='Total'), ]
df6_6[8][df6_6[8]==":"] <- NA
df6_6<- df6_6[which(df6_6$'DEG_URB'!="Total"),]
df6_6<- df6_6[which(df6_6$'GEO'!="European Union - 27 countries (from 2020)"),]
df6_6<- df6_6[which(df6_6$'GEO'!="European Union - 28 countries (2013-2020)"),]
df6_6<- df6_6[which(df6_6$'GEO'!="European Union - 27 countries (2007-2013)"),]
df6_6<- df6_6[which(df6_6$'GEO'!="Euro area (EA11-1999, EA12-2001, EA13-2007, EA16-2008, EA16-2009, EA17-2011, EA18-2014, EA19-2015)"),]
df6_6<- df6_6[which(df6_6$'GEO'!="Euro area - 19 countries  (from 2015)"),]
df6_6<- df6_6[which(df6_6$'GEO'!="Euro area - 18 countries (2014)"),]
df6_6<- df6_6[which(df6_6$'GEO'!="European Union (EU6-1958, EU9-1973, EU10-1981, EU12-1986, EU16-1995, EU25-2004, EU27-2007, EU28-2013, EU27-2020)"),]

df6_6$Value <- as.numeric(as.character(df6_6$Value))

top6 <-sort(tapply(df6_6$Value, df6_6$GEO, mean, na.rm=TRUE),decreasing = T)
top6_10<-top6[1:10]
dimnames(top6_10)
top610<-as.vector(dimnames(top6_10))
top610<-unlist(top610)
top610
df6_6_6<- df6_6[df6_6$GEO %in% top610,]

abra_6 <- ggplot(data=df6_6_6, aes(x=GEO, y=Value, fill=DEG_URB)) +
  geom_bar(stat="identity", width=0.6, na.rm=TRUE, position=position_dodge())+
  scale_fill_manual(name="Urbanizáció", values=c("lightcoral", "lightblue", "green4"))+
   labs(title="Top 10 nagyon rossz (szubjektív) egészségi állapottal rendelkező ország (2010-2018)",
       x="Ország",y="Nagyon rossz a szubjektív egészségi állapota (%)",
       subtitle="Urbanizáció szerint")+
	   coord_cartesian (ylim=c(2, 7))+
	   theme_minimal()
	   
#install.packages("ggpubr")
library("ggpubr")
abrak_orszag <- ggarrange(abra_1, abra_2, ncol=1, nrow=2)
abrak_nem <- ggarrange(abra_3, abra_4, ncol=1, nrow=2)
abrak_urb <- ggarrange(abra_5, abra_6, ncol=1, nrow=2)