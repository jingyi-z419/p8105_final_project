
Hospital=read.csv("Hospital_Inpatient_Cost_Transparency__Beginning_2009.csv",
                  stringsAsFactors = FALSE )
colnames(Hospital)

Hos=subset(Hospital,Year>=2015 & Year<=2017)


# scatter

scatter=function(id){
  tr=subset(Hos,Facility.Id==id)
 plot(tr$Mean.Charge,tr$Mean.Cost)
}


scatter(324)






# boxplot

boxplot(as.numeric(Mean.Cost)~APR.Medical.Surgical.Description ,data=Hos)

boxplot(as.numeric(Mean.Charge)~APR.Medical.Surgical.Description,data=Hos)

boxplot(as.numeric(Mean.Cost)~APR.Severity.of.Illness.Description,data=Hos)

boxplot(as.numeric(Mean.Charge)~APR.Severity.of.Illness.Description,data=Hos)


# ggplot2
library(ggplot2)


p=ggplot(data=Hos,mapping=aes(x=Year,y=as.numeric(Mean.Charge)),colour=APR.Medical.Surgical.Description)
p+geom_line()

