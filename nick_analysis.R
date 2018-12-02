update.package(checkBuilt = TRUE)
install.packages("data.table")
install.packages("zoo")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("scales")
install.packages("beepr")
install.packages("tidyverse")
install.packages("ggalt")

library(scales)
library(zoo)
library(data.table)
library(reshape2)
library(stringr)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggalt)

# load data
nick_data <- fread('C://Users/Xiao Han/Desktop/Data/nick_data.csv', fill=TRUE, header=TRUE)
nd= data.table(nick_data)

mips_data <- fread('C://Users/Xiao Han/Dropbox/Mount Sinai/PGY3/nick_project/MIPS_Submission_Report.csv', fill=TRUE, header=TRUE)
b=c(19, 20, 21, 22, 24, 25, 31)
mips_data=mips_data[which(mips_data$MeasureID %in% b),]

gpro1_data<-read.table('C://Users/Xiao Han/Dropbox/Mount Sinai/PGY3/nick_project/gpro1.csv', header = TRUE, sep=',')  #load data


# Understand data
a=data.frame(table(nd$GrpName)) #unique48
b=data.frame(table(nd$EDName)) #unique 111
c=data.frame(table(nd$Measure_ID)) #unique 7
d=data.frame(table(nd$ElementName)) #unique 135,
d1=data.frame(table(nd$Element_ID)) #unique 129
f=data.frame(table(nd$Type)) # unique 6

nd_colnames=colnames(nd)

# clean data
nd=nd[!which(nd$Element_ID=="NULL")] # remove null 

#remove some column
nd2=nd[,-c('GrpID', 'GrpName','ED_ID','MeasureTitle', 'ElementDescription', 'ElementDescription', 'ShortName', 'Type', 'PatientCount', 'Patient_Visit')]

OL_threshold=1.5
#bin function
bincount = function(num_x)
{
  q_25 = quantile(num_x[which(num_x!=0)], 0.25)
  q_75 = quantile(num_x[which(num_x!=0)], 0.75)
  #lower_threshold =q_25-OL_threshold*(q_75-q_25)
  #upper_threshold =q_75+OL_threshold*(q_75-q_25)
  lower_threshold =quantile(num_x[which(num_x!=0)], 0.10)
  upper_threshold =quantile(num_x[which(num_x!=0)], 0.90)
  ifelse(num_x<=0, 'Zero', ifelse(num_x < lower_threshold, 'LOW_OL', ifelse(num_x < upper_threshold, 'Norm', 'High_OL')))
}

for(i in 1:length(unique(nd$Measure_ID)))
{
nd_summary=nd2[which(nd$Measure_ID==unique(nd$Measure_ID)[i])]
nd_summary[, VisitPercent:=VisitCount/VisitCount[which(Element_ID==8505)], by=.(EDName)]#check

###################################REMOVAL- RETHINK
nd_summary=nd_summary[!which(nd_summary$EDName=="Allina Buffalo Hospital")] 
nd_summary=nd_summary[!which(nd_summary$EDName=="Allina Mercy Hospital- Unity Campus")] 
nd_summary=nd_summary[!which(nd_summary$EDName=="Allina Mercy Hospital")] 

#reshape with dcast_VisitCount
nd_summary_mod= nd_summary[, c('EDName', 'Element_ID', 'ElementName', 'VisitCount')]
nd_summary_dcast_VisitCount=dcast.data.table(nd_summary_mod, Element_ID+ElementName~EDName)

#reshape with dcast_visitPercent
nd_summary_mod= nd_summary[, c('EDName', 'Element_ID', 'ElementName', 'VisitPercent')]
nd_summary_dcast_VisitPercent=dcast.data.table(nd_summary_mod, Element_ID+ElementName~EDName)

x=unique(nd$Measure_ID)[i]
write.csv(nd_summary_dcast_VisitPercent, sprintf("C://Users/Xiao Han/Desktop/Data/Nick_Processed_Data/%s_Percent_Pivot.csv", x))
write.csv(nd_summary_dcast_VisitCount, sprintf("C://Users/Xiao Han/Desktop/Data/Nick_Processed_Data/%s_AbsoluteCount_Pivot.csv",x))

#add binning column
nd_summary[, bin_VisitCount:=bincount(VisitCount), by=ElementName]
nd_summary[, bin_VisitPercent:=bincount(VisitPercent), by=ElementName]

#facet text

swr = function(string, nwrap=20) {
  paste(strwrap(string, width=nwrap), collapse="\n")
}
swr = Vectorize(swr)
nd_summary$Facet_text = paste(nd_summary$Element_ID,nd_summary$ElementName)
nd_summary$Facet_text_wrap=swr(nd_summary$Facet_text)


#ggplot_by Percent_HeatMap_pdf
ggplot(nd_summary, aes(x = EDName, y = Facet_text)) +
  geom_tile(aes(fill = bin_VisitPercent),width=0.7, height=0.7) +
  scale_fill_manual(values = c("High_OL"="firebrick", "LOW_OL"="orange", "Norm"="palegreen4", "Zero"="Gray"))+
  ylim(rev(levels(as.factor(nd_summary$Facet_text))))+
 # scale_y_discrete(name="", limits = rev(levels(nd_summary$Facet_text)))+
  theme(axis.text.y=element_text(size=5, angle=45, hjust=1), axis.text.x=element_text(size=5, angle=45, hjust=1)) +
  labs(x="hospitals", y="percentage", title = unique(nd$Measure_ID)[i], fill="Close")
ggsave(sprintf("C://Users/Xiao Han/Desktop/Data/Nick_Processed_Data/%s_Heatmap.pdf", x), width = 11, height = 8)


##ggplot_by Percent_Bar_pdf
plot_list = list()
Ele_list=unique(nd_summary$Facet_text)

for(j in 1:ceiling(length(Ele_list)/4)){
  if (j<ceiling(length(Ele_list)/4)-1){
    loop=((j-1)*4+1):((j-1)*4+4)
  }else
  {
    loop=((j-1)*4+1):length(Ele_list)
  }
  
  nd_summary_section=nd_summary[which(nd_summary$Facet_text %in% Ele_list[loop]),]
  
  p=ggplot(nd_summary_section, aes(fill=factor(bin_VisitPercent), x=EDName, y= VisitPercent))+
    geom_bar(stat="identity") +
    scale_fill_manual("legend", values = c("High_OL"="firebrick", "LOW_OL"="orange", "Norm"="palegreen4", "Zero"="Gray"))+
    facet_grid(Facet_text_wrap ~.,scales="free_y") +
    theme(axis.text.x=element_text(size=6, angle=45, hjust=1))+
    labs(x="hospitals", y="percentage", title = unique(nd$Measure_ID)[i], fill="Close")
  plot_list[[j]] = p
}

pdf(sprintf("C://Users/Xiao Han/Desktop/Data/Nick_Processed_Data/%s_Percent_Bar.pdf", x), width = 11, height = 8)
for(k in 1:ceiling(length(Ele_list)/4)){
  print(plot_list[[k]])
}
dev.off()

nd_summary_stat_byElement=nd_summary[, list(median=median(VisitPercent), min=min(VisitPercent), max=max(VisitPercent), 
                                  q_25=quantile(VisitPercent[which(VisitPercent!=0)], 0.25),
                                  q_75=quantile(VisitPercent[which(VisitPercent!=0)], 0.75), 
                                  q_25_15IQR=quantile(VisitPercent[which(VisitPercent!=0)], 0.25)-OL_threshold*(quantile(VisitPercent[which(VisitPercent!=0)], 0.75)-quantile(VisitPercent[which(VisitPercent!=0)], 0.25)), 
                                  q_75_15IQR=quantile(VisitPercent[which(VisitPercent!=0)], 0.75)+OL_threshold*(quantile(VisitPercent[which(VisitPercent!=0)], 0.75)-quantile(VisitPercent[which(VisitPercent!=0)], 0.25)),
                                  q_10=quantile(VisitPercent[which(VisitPercent!=0)], 0.10),
                                  q_90=quantile(VisitPercent[which(VisitPercent!=0)], 0.90)), 
                                  by=Facet_text]

nd_summary_stat_byHospital=nd_summary[, list(High_OL_count=sum(bin_VisitPercent=="High_OL"), LOW_OL_count=sum(bin_VisitPercent=="LOW_OL")), 
                                      by=EDName][order(EDName),]

write.csv(nd_summary_stat_byHospital, sprintf("C://Users/Xiao Han/Desktop/Data/Nick_Processed_Data/%s_summarystat_byHospital.csv", x))
write.csv(nd_summary_stat_byElement, sprintf("C://Users/Xiao Han/Desktop/Data/Nick_Processed_Data/%s_summarystat_byElement.csv", x))
}
#######################################################DON'T USE###################################################################

#ggplot_by Absolute number_Bar _ DON'T USE 
ggplot(nd_summary, aes(fill=factor(bin_VisitCount), x=EDName, y= VisitCount))+
  geom_bar(stat="identity") +
  facet_grid(Element_ID ~.,scales="free_y") +
  theme(axis.text.x=element_text(size=6, angle=45, hjust=1))

#ggplot_by Percent_Bar
ggplot(nd_summary, aes(fill=factor(bin_VisitPercent), x=EDName, y= VisitPercent))+
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("firebrick", "goldenrod2", "palegreen4", "Gray"))+
  facet_grid(Element_ID ~.,scales="free_y") +
  theme(axis.text.x=element_text(size=6, angle=45, hjust=1))+
  labs(x="hospitals", y="percentage", title = unique(nd$Measure_ID)[i], fill="Close")

#ggplot_by Absolute_HeatMap _ DON'T USE
ggplot(nd_summary, aes(x = EDName, y = ElementName)) +
  geom_tile(aes(fill = bin_VisitCount)) +
  theme(axis.text.x=element_text(size=6, angle=45, hjust=1))
