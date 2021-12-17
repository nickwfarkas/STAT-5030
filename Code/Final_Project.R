setwd("~/Downloads/Statistical_Computing")
SRS <- read.csv("SRS.csv")

independant <- (SRS$state_government != "I")
SRS.twoparty <- SRS[independant,]
SRS.twoparty <- na.omit(SRS.twoparty)

SRS.twoparty.violent <- data.frame(SRS.twoparty$year,SRS.twoparty$state_abbr,
                    (SRS.twoparty$violent_crime/SRS.twoparty$population),
                    SRS.twoparty$state_government,stringsAsFactors=T)
colnames(SRS.twoparty.violent) <- c("year","state_abbr","crime_per_person","party")

SRS.twoparty.property <- data.frame(SRS.twoparty$year,SRS.twoparty$state_abbr,
                                   (SRS.twoparty$property_crime/SRS.twoparty$population),
                                   SRS.twoparty$state_government,stringsAsFactors=T)
colnames(SRS.twoparty.property) <- c("year","state_abbr","crime_per_person","party")

years <- c((1979:2020))

shapiroTests <- function(SRS.tp,yrs)
{
  shapiro.p.values <- numeric(length(yrs))
  for (i in 1:length(yrs)) 
  {
    t <- (SRS.tp$year == yrs[i])
    R <- na.omit(SRS.tp[t,])
    
    temp <- shapiro.test(R$crime_per_person)
    
    shapiro.p.values[i] <- temp$p.value
  }
  return(shapiro.p.values)
}

shapiro.p.values.property <- shapiroTests(SRS.twoparty.property,years)
shapiro.p.values.violent <- shapiroTests(SRS.twoparty.voilent,years)

dev.copy(jpeg,"Shapiro-P-Values-Violent.jpg")
plot(years,shapiro.p.values.violent,xlab = "Year",ylab = "P-Value")
title("Shapiro-Wilk Test of Normality on Violent Crime by year")
abline(h=.05, col="red")
dev.off()

dev.copy(jpeg,"Shapiro-P-Values-Property.jpg")
plot(years,shapiro.p.values.property,xlab = "Year",ylab = "P-Value")
title("Shapiro-Wilk Test of Normality on Property Crime by year")
abline(h=.05, col="red")
dev.off()

#If p-value is less than .05 there is a significant difference
wilcoxonTests <- function(SRS.tp,yrs,printTables,dataTitle)
{
  wilcoxon.p.values <- numeric(length(yrs))
  for (j in 1:length(yrs)) 
  {
    yr <- (SRS.tp$year == yrs[j])
    RXD <- na.omit(SRS.tp[yr,])
    wt <- wilcox.test((RXD$crime_per_person)~RXD$party, mu=0)
    print(wt)
    wilcoxon.p.values[j] <- wt$p.value
    if(printTables == T)
    {
      dev.copy(jpeg,paste("BoxPlot_",yrs[j],".jpg"))
      boxplot(RXD$crime_per_person~RXD$party, 
              xlab = "State Government Party", ylab = paste(dataTitle," Per Person"))
      title(paste(dataTitle," by State Government Party ",yrs[j],"\n","P-Value: ",wt$p.value))
      dev.off()
    }
  }
  return(wilcoxon.p.values)
}

wilcoxon.p.values.violent <- wilcoxonTests(SRS.twoparty.violent,years,T,"Violent Crime")
wilcoxon.p.values.property <- wilcoxonTests(SRS.twoparty.property,years,T,"Property Crime")

dev.copy(jpeg,"Wilcoxon-P-Values-Violent.jpg")
plot(years,wilcoxon.p.values.violent,xlab = "Year",ylab = "P-Value")
title("Wilcoxon Rank Sum Test on Violent Crime by year")
abline(h=.05, col="red")
dev.off()

dev.copy(jpeg,"Wilcoxon-P-Values-Property.jpg")
plot(years,wilcoxon.p.values.property,xlab = "Year",ylab = "P-Value")
title("Wilcoxon Rank Sum Test on Property Crime by year")
abline(h=.05, col="red")
dev.off()

Violent.model.withoutParty <- lm(formula= crime_per_person~year+state_abbr, data= SRS.twoparty.violent)
Violent.model.withParty <- lm(formula= crime_per_person~year+state_abbr+party, data= SRS.twoparty.violent)

Property.model.withoutParty <- lm(formula= crime_per_person~year+state_abbr, data= SRS.twoparty.property)
Property.model.withParty <- lm(formula= crime_per_person~year+state_abbr+party, data= SRS.twoparty.property)

summary(Violent.model.withoutParty)
summary(Violent.model.withParty)

summary(Property.model.withoutParty)
summary(Property.model.withParty)

jpeg("ViolentModelWithoutParty.jpg")
par(mfrow=c(2,2))
plot(Violent.model.withoutParty)
dev.off()

jpeg("ViolentModelWithParty.jpg")
par(mfrow=c(2,2))
plot(Violent.model.withParty)
dev.off()

jpeg("PropertyModelWithoutParty.jpg")
par(mfrow=c(2,2))
plot(Property.model.withoutParty)
dev.off()

jpeg("PropertyModelWithParty.jpg")
par(mfrow=c(2,2))
plot(Property.model.withParty)
dev.off()

