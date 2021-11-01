#Run this code to get the chadwick database
library(baseballr)
get_chadwick_lu()

#Load my datasets for baseball savant and fangraphs data
library(rio)
fangraphs <- import("Predictive StatCast metrics.xlsx", sheet = 1)
savant <- import("Predictive StatCast metrics.xlsx", sheet = 2)
#playerid = fangraphs, player_id = savant

#Shorten chadwick database for this assignment
chadwick <- chadwick_player_lu_table[c(3,7)]

#Merge datasets
df1 <- merge(fangraphs, chadwick, by.x = "playerid", by.y = "key_fangraphs")
df2 <- merge(df1, savant, by.x = c("key_mlbam", "Season"), by.y = c("player_id", "year"))
df <- df2

colnames(df)

#Add another dataset containing ages, because I forgot this before. Also from Fangraphs
ages <- import("Predictive StatCast metrics.xlsx", sheet = 3)
ages <- ages[c(1,4,5)]
#merge ages now
df <- merge(df, ages, by = c("playerid", "Season"))

#Create whiff rate column
df$whiff_rate <- df$whiffs / df$swings

#Get rid of superfluous columns
df <- df[c(-13,-18,-19)]

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Create columns for "next year" stats.

#xwoba
df$xwoba_ny <- length(df$xwoba)
for (i in seq_along(df$xwoba)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$xwoba_ny[i] <- df$xwoba[i+1]
  }
  else {
    df$xwoba_ny[i] <- NA
  }
}

#xba
df$xba_ny <- length(df$xba)
for (i in seq_along(df$xba)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$xba_ny[i] <- df$xba[i+1]
  }
  else {
    df$xba_ny[i] <- NA
  }
}

#BB%
df$BB_ny <- length(df$`BB%`)
for (i in seq_along(df$`BB%`)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$BB_ny[i] <- df$`BB%`[i+1]
  }
  else {
    df$BB_ny[i] <- NA
  }
}

#K%
df$K_ny <- length(df$`K%`)
for (i in seq_along(df$`K%`)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$K_ny[i] <- df$`K%`[i+1]
  }
  else {
    df$K_ny[i] <- NA
  }
}

#AVG
df$AVG_ny <- length(df$AVG)
for (i in seq_along(df$AVG)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$AVG_ny[i] <- df$AVG[i+1]
  }
  else {
    df$AVG_ny[i] <- NA
  }
}

#SLG
df$SLG_ny <- length(df$SLG)
for (i in seq_along(df$SLG)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$SLG_ny[i] <- df$SLG[i+1]
  }
  else {
    df$SLG_ny[i] <- NA
  }
}

#wOBA
df$wOBA_ny <- length(df$wOBA)
for (i in seq_along(df$wOBA)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$wOBA_ny[i] <- df$wOBA[i+1]
  }
  else {
    df$wOBA_ny[i] <- NA
  }
}

#EV
df$EV_ny <- length(df$EV)
for (i in seq_along(df$EV)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$EV_ny[i] <- df$EV[i+1]
  }
  else {
    df$EV_ny[i] <- NA
  }
}

#Barrel%
df$Barrel_ny <- length(df$`Barrel%`)
for (i in seq_along(df$`Barrel%`)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$Barrel_ny[i] <- df$`Barrel%`[i+1]
  }
  else {
    df$Barrel_ny[i] <- NA
  }
}

#maxEV
df$maxEV_ny <- length(df$maxEV)
for (i in seq_along(df$maxEV)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$maxEV_ny[i] <- df$maxEV[i+1]
  }
  else {
    df$maxEV_ny[i] <- NA
  }
}

#HardHit%
df$HardHit_ny <- length(df$`HardHit%`)
for (i in seq_along(df$`HardHit%`)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$HardHit_ny[i] <- df$`HardHit%`[i+1]
  }
  else {
    df$HardHit_ny[i] <- NA
  }
}

#O-Swing%
df$OSwing_ny <- length(df$`O-Swing%`) 
for (i in seq_along(df$`O-Swing%`)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$OSwing_ny[i] <- df$`O-Swing%`[i+1]
  }
  else {
    df$OSwing_ny[i] <- NA
  }
}

#whiff_rate
df$whiff_rate_ny <- length(df$whiff_rate) 
for (i in seq_along(df$whiff_rate)) {
  if (df$playerid[i] == df$playerid[i+1]) {
    df$whiff_rate_ny[i] <- df$whiff_rate[i+1]
  }
  else {
    df$whiff_rate_ny[i] <- NA
  }
}

colSums(is.na(df)) 

#NOTE, because Michael Saunders was the last row and had no second season, his numbers are errors.
#I will delete his row.
df <- df[-1548, ]

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Check for differences between 2021 and other seasons because of the juiced ball.
#The following creates overlayed distribution plots.

#MaxEV
plot(density(df$maxEV[which(df$Season == 2021)]), main = "Distribution of Max Exit Velocity", lwd = 2)
lines(density(df$maxEV[-which(df$Season == 2021)]), lwd = 2, col= "red")
legend("topleft", legend=c("2021", "2017-2020"), col=c("black", "red"), lty = 1:1, cex=0.8)

#EV
plot(density(df$EV[which(df$Season == 2021)]), main = "Distribution of Avg Exit Velocity", lwd = 2)
lines(density(df$EV[-which(df$Season == 2021)]), lwd = 2, col= "red")
legend("topleft", legend=c("2021", "2017-2020"), col=c("black", "red"), lty = 1:1, cex=0.8)

#HardHit%
plot(density(df$`HardHit%`[which(df$Season == 2021)]), main = "Distribution of Hard Hit%", lwd = 2)
lines(density(df$`HardHit%`[-which(df$Season == 2021)]), lwd = 2, col= "red")
legend("topleft", legend=c("2021", "2017-2020"), col=c("black", "red"), lty = 1:1, cex=0.8)

#Barrel%
lines(density(df$`Barrel%`[which(df$Season == 2021)]), main = "Distribution of Barrel%", lwd = 2)
plot(density(df$`Barrel%`[-which(df$Season == 2021)]), main = "Distribution of Barrel%", lwd = 2, col= "red")
legend("topright", legend=c("2021", "2017-2020"), col=c("black", "red"), lty = 1:1, cex=0.8)

#xwOBA
plot(density(df$xwoba[which(df$Season == 2021)]), main = "Distribution of xwOBA", lwd = 2)
lines(density(df$xwoba[-which(df$Season == 2021)]), lwd = 2, col= "red")
legend("topleft", legend=c("2021", "2017-2020"), col=c("black", "red"), lty = 1:1, cex=0.8)


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Correlations
#For sake of correlations, create a dataset where null rows are removed
#(i.e., a 2021 season would be null because it has no next year value)
df_corr <- na.omit(df)   
colnames(df_corr)

#Correlations between metrics and their next year value
(cor(df_corr$`BB%`, df_corr$BB_ny))^2
(cor(df_corr$`K%`, df_corr$K_ny))^2
(cor(df_corr$AVG, df_corr$AVG_ny))^2
(cor(df_corr$SLG, df_corr$SLG_ny))^2
(cor(df_corr$wOBA, df_corr$wOBA_ny))^2
(cor(df_corr$EV, df_corr$EV_ny))^2
(cor(df_corr$`Barrel%`, df_corr$Barrel_ny))^2
(cor(df_corr$maxEV, df_corr$maxEV_ny))^2
(cor(df_corr$`HardHit%`, df_corr$HardHit_ny))^2
(cor(df_corr$`O-Swing%`, df_corr$OSwing_ny))^2
(cor(df_corr$xwoba, df_corr$xwoba_ny))^2
(cor(df_corr$xba, df_corr$xba_ny))^2
(cor(df_corr$whiff_rate, df_corr$whiff_rate_ny))^2
plot(df_corr$whiff_rate, df_corr$whiff_rate_ny)

#Correlations between metrics and next year wOBA
(cor(df_corr$`BB%`, df_corr$wOBA_ny))^2
(cor(df_corr$`K%`, df_corr$wOBA_ny))^2
(cor(df_corr$AVG, df_corr$wOBA_ny))^2
(cor(df_corr$SLG, df_corr$wOBA_ny))^2
(cor(df_corr$wOBA, df_corr$wOBA_ny))^2
(cor(df_corr$EV, df_corr$wOBA_ny))^2
(cor(df_corr$`Barrel%`, df_corr$wOBA_ny))^2
(cor(df_corr$maxEV, df_corr$wOBA_ny))^2
(cor(df_corr$`HardHit%`, df_corr$wOBA_ny))^2
(cor(df_corr$`O-Swing%`, df_corr$wOBA_ny))^2
(cor(df_corr$xwoba, df_corr$wOBA_ny))^2
(cor(df_corr$xba, df_corr$wOBA_ny))^2
(cor(df_corr$whiff_rate, df_corr$wOBA_ny))^2

#Correlations between metrics and this year's wOBA
(cor(df_corr$`BB%`, df_corr$wOBA))^2
(cor(df_corr$`K%`, df_corr$wOBA))^2
(cor(df_corr$AVG, df_corr$wOBA))^2
(cor(df_corr$SLG, df_corr$wOBA))^2
(cor(df_corr$wOBA, df_corr$wOBA))^2
(cor(df_corr$EV, df_corr$wOBA))^2
(cor(df_corr$`Barrel%`, df_corr$wOBA))^2
(cor(df_corr$maxEV, df_corr$wOBA))^2
(cor(df_corr$`HardHit%`, df_corr$wOBA))^2
(cor(df_corr$`O-Swing%`, df_corr$wOBA))^2
(cor(df_corr$xwoba, df_corr$wOBA))^2
(cor(df_corr$xba, df_corr$wOBA))^2
(cor(df_corr$whiff_rate, df_corr$wOBA))^2

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
#Prepare dataset for exporting, which will be used in Python for machine learning.
#I'll be doing a ML project using Statcast metrics to predict next year's wOBA.
df_export <- df_corr[c(-3:-6, -10, -11, -17, -19, -20, -23:-25, -28:-33, -35:-40)]
library(writexl)
write_xlsx(df_export, "C:\\Users\\Sandra\\Documents\\MZ fall 2017\\sabermiketrics\\Predictive_StatCast_Metrics_ML.xlsx")

#View correlations and distributions.
library(PerformanceAnalytics)
chart.Correlation(df_export[ , c(3:9)], histogram = TRUE)
#Barrel rate and PAs are not normally distributed. Do log transformations help?
hist(log(df_export$`Barrel%`))
hist(log(df_export$PA))
#Transformations do not help. I should leave them as is when performing machine learning.
chart.Correlation(df_export[ , c(10:16)], histogram = TRUE)
#All other metrics are normally distributed.