
library(plyr)
library(plotrix)
library(ggplot2)
library(segmented)
library(reshape2)
library(gridExtra)
library(DataCombine)


Accession <- c("One", "One", "One", "One", "One", "Two", "Two", "Two", "Two", "Two",
               "Three", "Three", "Three", "Three", "Three","Four", "Four", "Four", "Four", "Four",
               "Five", "Five", "Five", "Five", "Five", "Six", "Six", "Six", "Six", "Six",
               "Eight", "Eight", "Eight", "Eight", "Eight")

Repeat <- c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5","1", "2", "3", "4", "5","1", "2", "3", "4", "5",
            "1", "2", "3", "4", "5","1", "2", "3", "4", "5", "1", "2", "3", "4", "5")

#### Raw fluorescence files at each temperature are desginated T0, T25, etc. See: https://github.com/johnferguson1989/tpj_paper_Ferguson2020/tree/Example-data-for-segmented-analyses

T0 <- read.table("T0_processed.TXT", skip = 4, fill = TRUE)
T0 <- setNames(data.frame(t(T0[,-1])), T0[,1])
T0$Accession <- Accession
T0$Repeat <- Repeat
T0$Temperature <- rep(21, nrow(T0))

T25 <- read.table("T25_processed.TXT", skip = 4, fill = TRUE)
T25 <- setNames(data.frame(t(T25[,-1])), T25[,1])
T25$Accession <- Accession
T25$Repeat <- Repeat
T25$Temperature <- rep(25, nrow(T25))

T27.5 <- read.table("T27.5_processed.TXT", skip = 4, fill = TRUE)
T27.5 <- setNames(data.frame(t(T27.5[,-1])), T27.5[,1])
T27.5$Accession <- Accession
T27.5$Repeat <- Repeat
T27.5$Temperature <- rep(27.5, nrow(T27.5))

T30 <- read.table("T30_processed.TXT", skip = 4, fill = TRUE)
T30 <- setNames(data.frame(t(T30[,-1])), T30[,1])
T30$Accession <- Accession
T30$Repeat <- Repeat
T30$Temperature <- rep(30, nrow(T30))

T32 <- read.table("T32_processed.TXT", skip = 4, fill = TRUE)
T32 <- setNames(data.frame(t(T32[,-1])), T32[,1])
T32$Accession <- Accession
T32$Repeat <- Repeat
T32$Temperature <- rep(32, nrow(T32))

T34 <- read.table("T34_processed.TXT", skip = 4, fill = TRUE)
T34 <- setNames(data.frame(t(T34[,-1])), T34[,1])
T34$Accession <- Accession
T34$Repeat <- Repeat
T34$Temperature <- rep(34, nrow(T34))

T35 <- read.table("T35_processed.TXT", skip = 4, fill = TRUE)
T35 <- setNames(data.frame(t(T35[,-1])), T35[,1])
T35$Accession <- Accession
T35$Repeat <- Repeat
T35$Temperature <- rep(35, nrow(T35))

T37 <- read.table("T37_processed.TXT", skip = 4, fill = TRUE)
T37 <- setNames(data.frame(t(T37[,-1])), T37[,1])
T37$Accession <- Accession
T37$Repeat <- Repeat
T37$Temperature <- rep(37, nrow(T37))

T38 <- read.table("T38_processed.TXT", skip = 4, fill = TRUE)
T38 <- setNames(data.frame(t(T38[,-1])), T38[,1])
T38$Accession <- Accession
T38$Repeat <- Repeat
T38$Temperature <- rep(38, nrow(T38))

T39 <- read.table("T39_processed.TXT", skip = 4, fill = TRUE)
T39 <- setNames(data.frame(t(T39[,-1])), T39[,1])
T39$Accession <- Accession
T39$Repeat <- Repeat
T39$Temperature <- rep(39, nrow(T39))

T40 <- read.table("T40_processed.TXT", skip = 4, fill = TRUE)
T40 <- setNames(data.frame(t(T40[,-1])), T40[,1])
T40$Accession <- Accession
T40$Repeat <- Repeat
T40$Temperature <- rep(40, nrow(T40))

T42 <- read.table("T42_processed.TXT", skip = 4, fill = TRUE)
T42 <- setNames(data.frame(t(T42[,-1])), T42[,1])
T42$Accession <- Accession
T42$Repeat <- Repeat
T42$Temperature <- rep(42, nrow(T42))

T44 <- read.table("T44_processed.TXT", skip = 4, fill = TRUE)
T44 <- setNames(data.frame(t(T44[,-1])), T44[,1])
T44$Accession <- Accession
T44$Repeat <- Repeat
T44$Temperature <- rep(44, nrow(T44))

T46 <- read.table("T46_processed.TXT", skip = 4, fill = TRUE)
T46 <- setNames(data.frame(t(T46[,-1])), T46[,1])
T46$Accession <- Accession
T46$Repeat <- Repeat
T46$Temperature <- rep(46, nrow(T46))

T48 <- read.table("T48_processed.TXT", skip = 4, fill = TRUE)
T48 <- setNames(data.frame(t(T48[,-1])), T48[,1])
T48$Accession <- Accession
T48$Repeat <- Repeat
T48$Temperature <- rep(48, nrow(T48))

T50 <- read.table("T50_processed.TXT", skip = 4, fill = TRUE)
T50 <- setNames(data.frame(t(T50[,-1])), T50[,1])
T50$Accession <- Accession
T50$Repeat <- Repeat
T50$Temperature <- rep(50, nrow(T50))

all_data <- rbind(T0,T25,T27.5,T30,T32,T34,T35,T37,T38,T39,T40,T42,T44,T46,T48,T50)

all_data[ all_data< 0 ] <- 0

all_data$Accession_Repeat <- paste(all_data$Accession, all_data$Repeat, sep = "_")    

ggplot(all_data, aes(x=Temperature, y = QY_max)) + 
  geom_point() + 
  facet_wrap(~Accession_Repeat)

all_data <- subset(all_data, Accession_Repeat != "Eight_3" | Temperature < 47)
all_data <- subset(all_data, Accession_Repeat != "Four_4" | Temperature < 47)
all_data <- subset(all_data, Accession_Repeat != "Four_5" | Temperature < 47)
all_data <- subset(all_data, Accession_Repeat != "Six_1" | Temperature < 47)
all_data <- subset(all_data, Accession_Repeat != "Six_2" | Temperature < 49)
all_data <- subset(all_data, Accession_Repeat != "Six_4" | Temperature < 49)
all_data <- subset(all_data, Accession_Repeat != "Six_5" | Temperature < 47)
all_data <- subset(all_data, Accession_Repeat != "Three_3" | Temperature < 49)
all_data <- subset(all_data, Accession_Repeat != "Three_5" | Temperature < 49)

ggplot(all_data, aes(x=Temperature, y = QY_max)) + 
  geom_point() + 
  facet_wrap(~Accession_Repeat)

ggplot(all_data, aes(x=Temperature, y = Fo)) + 
  geom_point() + 
  facet_wrap(~Accession_Repeat)

all_data$x <- all_data$Temperature

wide_data<- dcast(all_data, x ~ Accession_Repeat, value.var = "QY_max" )

run_mod_plot <- function(varname, data){
  
  data$Y <- data[,varname]
  model <- lm(Y ~ x, data)
  seg_model <- segmented(model, seg.Z = ~x)
  plot(seg_model, main = varname, ylim = c(0,0.9), xlim = c(0,55))
  
}

lapply(names(wide_data)[2:ncol(wide_data)], function(x)run_mod_plot(x, wide_data))


run_mod_parameters <- function(varname, data){
  
  data$Y <- data[,varname]
  model <- lm(Y ~ x, data) # Linear model
  seg_model <- segmented(model, seg.Z = ~x) # Segmented model
  breakpoint <- as.matrix(seg_model$psi[2]) # Extract breakpoint
  coefficients <- as.matrix(seg_model$coefficients) # Extract coefficients
  summary_curve1 <- as.data.frame(rbind(breakpoint, coefficients)) 
  colnames(summary_curve1) <- varname
  
  return(summary_curve1)
}

parameters <- as.data.frame(lapply(names(wide_data)[2:ncol(wide_data)], function(x)run_mod_parameters(x, wide_data)))

row.names(parameters) <- c("bp", "Intercept", "m1", "m2", "ignore")

parameters <- as.data.frame(t(parameters))

head(parameters)

