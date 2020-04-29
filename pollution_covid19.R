# R Code for assessing correlations between COVID-19 cases/deaths and air pollution particles
# Re-analysis of the work of Travaglio et al (2020): DOI: 10.1101/2020.04.16.20067405
# Graham Wheeler

rm(list=ls())

#########################
# Load and prepare data #
#########################

pollution<-read.csv("C:\\pollution.csv", header = TRUE)
pollution<-pollution[,1:4]
pollution<-pollution[1:7,]
deaths<-read.csv("C:\\deaths.csv", header = FALSE)
cum_deaths<-deaths[,c(1,dim(deaths)[2])]
cases<-read.csv("C:\\cases.csv", header = TRUE, colClasses=c('character', 'character', 'character', 'character','numeric','numeric'))
cases_region<-cases[-which(cases[,3]!="Region"),]
cases_region_list<-lapply(unique(cases_region[,4]), function(z) cases_region[which(cases_region[,4] %in% z),])

cases_region_list[[1]]
for(i in 1:length(cases_region_list)){
  cases_region_list[[i]]<-rbind(cases_region_list[[i]],c("North East And Yorkshire", NA, "Region", cases_region_list[[i]][1,4], sum(as.numeric(cases_region_list[[i]][c(1,6),5])), sum(as.numeric(cases_region_list[[i]][c(1,6),6]))))
  cases_region_list[[i]]<-rbind(cases_region_list[[i]],c("Midlands", NA, "Region", cases_region_list[[i]][1,4], sum(as.numeric(cases_region_list[[i]][c(2,9),5])),sum(as.numeric(cases_region_list[[i]][c(2,9),6]))))
  cases_region_list[[i]]<-cases_region_list[[i]][-c(1,2,6,9),-c(2,3,5)]
  cases_region_list[[i]]<-cases_region_list[[i]][order(cases_region_list[[i]][,1]),]
}

##########
# Set-up #
##########

index<-12 # Which date corresponds to 08/04/2020 (final date for COVID-19 case/death reports)
cases_region_list[[index]] # Double-check date = 08/04/2020

############################
# Normality tests for data #
############################

shapiro.test(as.numeric(cases_region_list[[index]][,3])) # Do not reject
shapiro.test(cum_deaths[,2]) # # Do not reject
shapiro.test(pollution[,2]) # # Do not reject
shapiro.test(pollution[,3]) # REJECT NORMALITY
shapiro.test(pollution[,4]) # # Do not reject

##########################################
# TO ASSESS CORRELATIONS, RUN CODE BELOW #
# Both SPearman and Pearson provided
# - choose most appropriate based on
#    data set size and normality tests
##########################################

# cor.test(x = as.numeric(cases_region_list[[index]][,3]), pollution[,2], method = c("pearson"))
# cor.test(x = as.numeric(cases_region_list[[index]][,3]), pollution[,2], method = c("spearman"))
# 
# cor.test(x = as.numeric(cases_region_list[[index]][,3]), pollution[,3], method = c("pearson"))
# cor.test(x = as.numeric(cases_region_list[[index]][,3]), pollution[,3], method = c("spearman"))
# 
# cor.test(x = as.numeric(cases_region_list[[index]][,3]), pollution[,4], method = c("pearson"))
# cor.test(x = as.numeric(cases_region_list[[index]][,3]), pollution[,4], method = c("spearman"))
# 
# cor.test(x = cum_deaths[,2], pollution[,2], method = c("pearson"))
# cor.test(x = cum_deaths[,2], pollution[,2], method = c("spearman"))
# 
# cor.test(x = cum_deaths[,2], pollution[,3], method = c("pearson"))
# cor.test(x = cum_deaths[,2], pollution[,3], method = c("spearman"))
# 
# cor.test(x = cum_deaths[,2], pollution[,4], method = c("pearson"))
# cor.test(x = cum_deaths[,2], pollution[,4], method = c("spearman"))

#####################
# Plot correlations #
# All data points   #
#####################

# NB: Spearman correlations used for Nitrogen Dioxide plots (I had previously used Pearson)

include<-c(1,2,3,4,5,6,7) # Include all 7 data points

par(mfrow=c(1,1))
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE))
plot(cum_deaths[include,2],pollution[include,2], cex = 2, col = "black", bg = "red", las = 1, xlab = "Cumulative Deaths (as of 08/04/2020)", ylab = "Nitric Oxide levels", ylim = c(0,30), xlim = c(0,3000), bty = "L", pch = 21)
text(cum_deaths[include,2],pollution[include,2], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(cum_deaths[include,2], pollution[include,2]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = cum_deaths[include,2], pollution[include,2], method = c("pearson"))
text(x = 0, y = 25, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(cum_deaths[include,2],pollution[include,3], cex = 2, col = "black", bg = "red", las = 1, xlab = "Cumulative Deaths (as of 08/04/2020)", ylab = "Nitrogen Dioxide levels", ylim = c(0,50), xlim = c(0,3000), bty = "L", pch = 21)
text(cum_deaths[include,2],pollution[include,3], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(cum_deaths[include,2], pollution[include,3]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = cum_deaths[include,2], pollution[include,3], method = c("spearman"))
text(x = 0, y = 45, labels = paste0("Spearman Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(cum_deaths[include,2],pollution[include,4], cex = 2, col = "black", bg = "red", las = 1, xlab = "Cumulative Deaths (as of 08/04/2020)", ylab = "Ozone levels", ylim = c(30,60), xlim = c(0,3000), bty = "L", pch = 21)
text(cum_deaths[include,2],pollution[include,4], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(cum_deaths[include,2], pollution[include,4]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = cum_deaths[include,2], pollution[include,3], method = c("pearson"))
text(x = 0, y = 35, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)


plot(as.numeric(cases_region_list[[index]][include,3]),pollution[include,2], cex = 2, col = "black", bg = "blue", las = 1, xlab = "Cumulative Cases (as of 08/04/2020)", ylab = "Nitric Oxide levels", ylim = c(0,30), xlim = c(0,17000), bty = "L", pch = 21)
text(as.numeric(cases_region_list[[index]][include,3]),pollution[include,2], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(as.numeric(cases_region_list[[index]][include,3]), pollution[include,2]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = as.numeric(cases_region_list[[index]][include,3]), pollution[include,2], method = c("pearson"))
text(x = 0, y = 25, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(as.numeric(cases_region_list[[index]][include,3]),pollution[include,3], cex = 2, col = "black", bg = "blue", las = 1, xlab = "Cumulative Cases (as of 08/04/2020)", ylab = "Nitrogen Dioxide levels", ylim = c(0,50), xlim = c(0,17000), bty = "L", pch = 21)
text(as.numeric(cases_region_list[[index]][include,3]),pollution[include,3], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(as.numeric(cases_region_list[[index]][include,3]), pollution[include,3]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = as.numeric(cases_region_list[[index]][include,3]), pollution[include,3], method = c("spearman"))
text(x = 0, y = 45, labels = paste0("Spearman Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(as.numeric(cases_region_list[[index]][include,3]),pollution[include,4], cex = 2, col = "black", bg = "blue", las = 1, xlab = "Cumulative Cases (as of 08/04/2020)", ylab = "Ozone levels", ylim = c(30,60), xlim = c(0,17000), bty = "L", pch = 21)
text(as.numeric(cases_region_list[[index]][include,3]),pollution[include,4], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(as.numeric(cases_region_list[[index]][include,3]), pollution[include,4]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = as.numeric(cases_region_list[[index]][include,3]), pollution[include,4], method = c("pearson"))
text(x = 0, y = 35, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)
mtext("Update to correlation analyses of Travaglio et al (2020)", side = 3, line = -3, outer = TRUE, cex = 1, font = 2)


#####################
# Plot correlations #
# London removed    #
#####################

include<-c(1,3,4,5,6,7) # Remove London

par(mfrow=c(1,1))
layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE))
plot(cum_deaths[include,2],pollution[include,2], cex = 2, col = "black", bg = "red", las = 1, xlab = "Cumulative Deaths (as of 08/04/2020)", ylab = "Nitric Oxide levels", ylim = c(0,30), xlim = c(0,3000), bty = "L", pch = 21)
text(cum_deaths[include,2],pollution[include,2], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(cum_deaths[include,2], pollution[include,2]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = cum_deaths[include,2], pollution[include,2], method = c("pearson"))
text(x = 0, y = 25, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(cum_deaths[include,2],pollution[include,3], cex = 2, col = "black", bg = "red", las = 1, xlab = "Cumulative Deaths (as of 08/04/2020)", ylab = "Nitrogen Dioxide levels", ylim = c(0,50), xlim = c(0,3000), bty = "L", pch = 21)
text(cum_deaths[include,2],pollution[include,3], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(cum_deaths[include,2], pollution[include,3]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = cum_deaths[include,2], pollution[include,3], method = c("spearman"))
text(x = 0, y = 45, labels = paste0("Spearman Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(cum_deaths[include,2],pollution[include,4], cex = 2, col = "black", bg = "red", las = 1, xlab = "Cumulative Deaths (as of 08/04/2020)", ylab = "Ozone levels", ylim = c(30,60), xlim = c(0,3000), bty = "L", pch = 21)
text(cum_deaths[include,2],pollution[include,4], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(cum_deaths[include,2], pollution[include,4]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = cum_deaths[include,2], pollution[include,4], method = c("pearson"))
text(x = 0, y = 35, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)


plot(as.numeric(cases_region_list[[index]][include,3]),pollution[include,2], cex = 2, col = "black", bg = "blue", las = 1, xlab = "Cumulative Cases (as of 08/04/2020)", ylab = "Nitric Oxide levels", ylim = c(0,30), xlim = c(0,17000), bty = "L", pch = 21)
text(as.numeric(cases_region_list[[index]][include,3]),pollution[include,2], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(as.numeric(cases_region_list[[index]][include,3]), pollution[include,2]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = as.numeric(cases_region_list[[index]][include,3]), pollution[include,2], method = c("pearson"))
text(x = 0, y = 25, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(as.numeric(cases_region_list[[index]][include,3]),pollution[include,3], cex = 2, col = "black", bg = "blue", las = 1, xlab = "Cumulative Cases (as of 08/04/2020)", ylab = "Nitrogen Dioxide levels", ylim = c(0,50), xlim = c(0,17000), bty = "L", pch = 21)
text(as.numeric(cases_region_list[[index]][include,3]),pollution[include,3], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(as.numeric(cases_region_list[[index]][include,3]), pollution[include,3]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = as.numeric(cases_region_list[[index]][include,3]), pollution[include,3], method = c("spearman"))
text(x = 0, y = 45, labels = paste0("Spearman Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)

plot(as.numeric(cases_region_list[[index]][include,3]),pollution[include,4], cex = 2, col = "black", bg = "blue", las = 1, xlab = "Cumulative Cases (as of 08/04/2020)", ylab = "Ozone levels", ylim = c(30,60), xlim = c(0,17000), bty = "L", pch = 21)
text(as.numeric(cases_region_list[[index]][include,3]),pollution[include,4], labels = as.character(cum_deaths[include,1]), pos = 1)
data_fin<-as.data.frame(cbind(as.numeric(cases_region_list[[index]][include,3]), pollution[include,4]))
abline(lm(V2~V1, data = data_fin), col="black", lty = 2)
foo<-cor.test(x = as.numeric(cases_region_list[[index]][include,3]), pollution[include,4], method = c("pearson"))
text(x = 0, y = 35, labels = paste0("Pearson Correlation = ",round(foo$estimate,2),"\n p-value = ",round(foo$p.value,2)), adj = 0)
mtext("Update to correlation analyses of Travaglio et al (2020) - Excluding London", side = 3, line = -3, outer = TRUE, font = 2, cex = 1)



#######
# END #
#######
