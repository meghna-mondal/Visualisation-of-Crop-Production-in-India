library(lattice)
library(dplyr)
library("latticeExtra")
library(gridExtra)
library(tidyr)
library(RColorBrewer)
library(ggplot2)
setwd("C:/Users/MONDAL/Desktop/CMI/VISU")
crop_prod<-read.csv("datafile (2).csv")
crop_cost<-read.csv("datafile (1).csv")




Variable_name = c("Crop", "State", "Cost.of.Cultivation....Hectare..A2.FL", "Cost.of.Cultivation....Hectare..C2", "Cost.of.Production....Quintal..C2", "Yield..Quintal..Hectare.")
Variable_type = c("Nominal", "Nominal", "Continuous", "Continuous", "Continuous", "Continuous")
Variable_content = c("Different types of crops", "States where the crop is cultivated", "Expected cost of cultivation of the crop per Hectare","Cost of cultivation of the crop per Hectare","Cost of Production per Quintal","Yield of crop in Quintal/Hectare")
Table_1 = data.frame( Variable_name, Variable_type, Variable_content)                         




Variable_Name = c("Crop", "Production.2006.07", "Production.2007.08", "Production.2008.09", "Production.2009.10", "Production.2010.11", "Area.2006.07", "Area.2007.08", "Area.2008.09", "Area.2009.10", "Area.2010.11", "Yield.2006.07", "Yield.2007.08", "Yield.2008.09", "Yield.2009.10", "Yield.2010.11")
Variable_Type = c("Nominal", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous", "Continuous")  
Variable_Content = c("Different types of crops cultivated", "Production of a particular crop in Year 2006-07", "Production of a particular crop in Year 2007-08","Production of a particular crop in Year 2008-09","Production of a particular crop in Year 2009-10","Production of a particular crop in Year 2010-11"
                          , "Area of cultivation of particular crop in Year 2006-07", "Area of cultivation of particular crop in Year 2007-08", "Area of cultivation of particular crop in Year 2008-09", "Area of cultivation of particular crop in Year 2009-10", "Area of cultivation of particular crop in Year 2010-11"
                          , "Yield of a particular crop in Year 2006-07", "Yield of a particular crop in Year 2007-08","Yield of a particular crop in Year 2008-09","Yield of a particular crop in Year 2009-10","Yield of a particular crop in Year 2010-11")


Table_2 = data.frame( Variable_Name, Variable_Type, Variable_Content)                         



ggplot(crop_cost,aes(y=Yield..Quintal..Hectare.,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Yield in Quintal/Hectare")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Cropwise Yield")
ggplot(crop_cost,aes(y=Yield..Quintal..Hectare.,x=State))+geom_bar(stat="identity",aes(fill=State))+ylab("Yield in Quintal")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Statewise Yield")
ggplot(crop_cost,aes(y=Cost.of.Cultivation....Hectare..C2/100000,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Cost of Cultivation/Hectare in Lakh")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Cropwise Cost of cultivation per Hectare")
ggplot(crop_cost,aes(y=Cost.of.Cultivation....Hectare..C2/100000,x=State))+geom_bar(stat="identity",aes(fill=State))+ylab("Cost of Cultivation/Hectare in Lakh")+xlab("State")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Statewise Cost of cultivation per Hectare")
ggplot(crop_cost,aes(y=Cost.of.Cultivation....Hectare..A2.FL/100000,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Cost of Cultivation/Hectare in Lakh")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Cropwise Cost of cultivation per Hectare")
ggplot(crop_cost,aes(y=Cost.of.Cultivation....Hectare..A2.FL/100000,x=State))+geom_bar(stat="identity",aes(fill=State))+ylab("Cost of Cultivation/Hectare in Lakh")+xlab("State")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Statewise Cost of cultivation per Hectare")
ggplot(crop_cost,aes(y=Cost.of.Production....Quintal..C2/1000,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Cost of Production/Quintal in thousand")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Cost of production per Quintal")
ggplot(crop_cost,aes(y=Cost.of.Production....Quintal..C2/1000,x=Crop	,color=State))+geom_point(size=3)+ylab("Cost of Production/Quintal in Lakh")+xlab("Crop")+theme(axis.text.x=element_text(angle=90))+ggtitle("Statewise cost of production/Quintal")




ggplot(crop_cost,aes(y=Yield..Quintal..Hectare.,x=State,color=State))+geom_point(size=1)+ylab("Yied in Quintal")+xlab("Crop")+theme(axis.text.x=element_text(angle=90))+ggtitle("Statewise yield of crops/Hectare")+facet_wrap(~Crop	)
ggplot(crop_cost,aes(y=Cost.of.Cultivation....Hectare..C2,x=State,color=State))+geom_point(size=1)+ylab("Cost of cultivation")+xlab("Crop")+theme(axis.text.x=element_text(angle=90))+ggtitle("Statewise Cost of Cultivation/Hectare")+facet_wrap(~Crop	)
ggplot(crop_cost,aes(y=Cost.of.Production....Quintal..C2,x=State,color=State))+geom_point(size=1)+ylab("Cost of production")+xlab("Crop")+theme(axis.text.x=element_text(angle=90))+ggtitle("Statewise Cost of Production/Quintal")+facet_wrap(~Crop	)

options(repr.plot.width=15,repr.plot.height=10)
pairs(crop_cost[,-c(1:2)])
cor(crop_cost[,-c(1:2)])

ggplot(crop_prod,aes(y=Production.2006.07,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Production in 2006-07")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Production in 2006-07")
ggplot(crop_prod,aes(y=Production.2007.08,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Production in 2007-08")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Production in 2007-08")
ggplot(crop_prod,aes(y=Production.2008.09,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Production in 2008-09")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Production in 2008-09")
ggplot(crop_prod,aes(y=Production.2009.10,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Production in 2009-10")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Production in 2009-10")
ggplot(crop_prod,aes(y=Production.2010.11,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Production in 2010-11")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Production in 2010-11")


ggplot(crop_prod,aes(y=Area.2006.07,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Area in 2006-07")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Area in 2006-07")
ggplot(crop_prod,aes(y=Area.2007.08,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Area in 2007-08")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Area in 2007-08")
ggplot(crop_prod,aes(y=Area.2008.09,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Area in 2008-09")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Area in 2008-09")
ggplot(crop_prod,aes(y=Area.2009.10,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Area in 2009-10")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Area in 2009-10")
ggplot(crop_prod,aes(y=Area.2010.11,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Area in 2010-11")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Area in 2010-11")


ggplot(crop_prod,aes(y=Yield.2006.07,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Yield in 2006-07")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Yield in 2006-07")
ggplot(crop_prod,aes(y=Yield.2007.08,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Yield in 2007-08")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Yield in 2007-08")
ggplot(crop_prod,aes(y=Yield.2008.09,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Yield in 2008-09")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Yield in 2008-09")
ggplot(crop_prod,aes(y=Yield.2009.10,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Yield in 2009-10")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Yield in 2009-10")
ggplot(crop_prod,aes(y=Yield.2010.11,x=Crop	,fill=Crop	))+geom_bar(stat="identity")+ylab("Yield in 2010-11")+xlab("Crop")+theme(axis.text.x=element_text(angle=90),legend.position="none")+ggtitle("Yield in 2010-11")




Production = data.frame(crop_prod$Production.2006.07,crop_prod$Production.2007.08,crop_prod$Production.2008.09,crop_prod$Production.2009.10,crop_prod$Production.2010.11)
P = as.matrix(Production)
barplot(t(P), main="Multiple Bar Diagram of Production in five year for different crop", ylab="Total", beside=TRUE, 
        col=terrain.colors(55))
options(repr.plot.width=15,repr.plot.height=10)
pairs(Area)
cor(Area)

Area = data.frame(crop_prod$Area.2006.07,crop_prod$Area.2007.08,crop_prod$Area.2008.09,crop_prod$Area.2009.10,crop_prod$Area.2010.11)
A = as.matrix(Area)
barplot(t(A), main="Multiple Bar Diagram of Production in five year for different crop", ylab="Total", beside=TRUE, 
        col=terrain.colors(55))
options(repr.plot.width=15,repr.plot.height=10)
pairs(Production)
cor(Production)

Yield = data.frame(crop_prod$Yield.2006.07,crop_prod$Yield.2007.08,crop_prod$Yield.2008.09,crop_prod$Yield.2009.10,crop_prod$Yield.2010.11)
Y = as.matrix(Yield)
barplot(t(Y), main="Multiple Bar Diagram of Production in five year for different crop", ylab="Total", beside=TRUE, 
        col=terrain.colors(55))
options(repr.plot.width=15,repr.plot.height=10)
pairs(Yield)
cor(Yield)

Year_2006_07 = data.frame(crop_prod$Production.2006.07,crop_prod$Area.2006.07,crop_prod$Yield.2006.07)
pairs(Year_2006_07)
cor(Year_2006_07)

Year_2007_08 = data.frame(crop_prod$Production.2007.08,crop_prod$Area.2007.08,crop_prod$Yield.2007.08)
pairs(Year_2007_08)
cor(Year_2007_08)

Year_2008_09 = data.frame(crop_prod$Production.2008.09,crop_prod$Area.2008.09,crop_prod$Yield.2008.09)
pairs(Year_2008_09)
cor(Year_2008_09)

Year_2009_10 = data.frame(crop_prod$Production.2009.10,crop_prod$Area.2009.10,crop_prod$Yield.2009.10)
pairs(Year_2009_10)
cor(Year_2009_10)

Year_2010_11 = data.frame(crop_prod$Production.2010.11,crop_prod$Area.2010.11,crop_prod$Yield.2010.11)
pairs(Year_2010_11)
cor(Year_2010_11)




