#Plot Unilever
#QBART
library("ggplot2")

#DATA
#Sediment export from each model for each site
bqart<- c(26.3 , 104.0,	82.7,	44.6) 
invest<- c(10.7 , 93.1,	4.3	,10.8)
sites<- c("Heilongjiang" , "Jiangxi"  ,"Mato Grosso"	,"Iowa")

#Dataframe
data<-data.frame(model=c(rep("BQART", 4), rep("InVEST", 4)), site=rep(sites, 2), sedexp=c(bqart,invest))


#PLOT
p <- ggplot(data=data, aes(x=site, y=sedexp, fill=model)) + geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#999999", "#E69F00"), name="Model") + #Color
  print_theme + theme(legend.position=c(0.15, 0.81)) +
  theme(axis.title.x = element_blank()) +   # Remove x-axis label
  ylab(expression(paste("Sediment export (T / km"^2," / yr)", sep=""))) 

jpgname <- paste(figdir, "supp_barplot.jpg", sep = "/")
jpeg(file = jpgname, units="in", res=300, width = 4, height=4)
print(p)
dev.off()


barplot(matrix())
