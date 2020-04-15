# Import data

library(readxl)
LMF_Sal <- read_excel("/Users/Runanyan/Desktop/6_Cocoa/0_Manuscript/LMF-Sal.xlsx")
LMF_Secondary <- read_excel("LMF-Sal.xlsx", sheet = "Secondary")
LMF_Serotype <- read_excel("LMF-Sal.xlsx", sheet = "6_serotypes")

View(LMF_Sal)

# Subset data
T100 <- subset(LMF_Sal,Temp == 100)
T110 <- subset(LMF_Sal,Temp == 110)
T115 <- subset(LMF_Sal,Temp == 115)
T120 <- subset(LMF_Sal,Temp == 120)
T130 <- subset(LMF_Sal,Temp == 130)
T140 <- subset(LMF_Sal,Temp == 140)

#Calculate the d values at different temperatures
# Fitting Log-linear regression, T=100
ll100 <- nls(logreduction~time/a, T100, start=list(a=10))
summary(ll100)
rsquare.100 <- cor(T100$logreduction, predict(ll100))^2

# Fitting Log-linear regression, T=110
ll110 <- nls(logreduction~time/a, T110, start=list(a=10))
summary(ll110)
rsquare.110 <- cor(T110$logreduction, predict(ll110))^2

# Fitting Log-linear regression, T=115
ll115 <- nls(logreduction~time/a, T115, start=list(a=10))
summary(ll115)
rsquare.115 <- cor(T115$logreduction, predict(ll115))^2

# Fitting Log-linear regression, T=120
ll120 <- nls(logreduction~time/a, T120, start=list(a=10))
summary(ll120)
rsquare.120 <- cor(T120$logreduction, predict(ll120))^2

# Fitting Log-linear regression, T=130
ll130 <- nls(logreduction~time/a, T130, start=list(a=10))
summary(ll130)
rsquare.130 <- cor(T130$logreduction, predict(ll130))^2

# Fitting Log-linear regression, T=140
ll140 <- nls(logreduction~time/a, T140, start=list(a=10))
summary(ll140)
rsquare.140 <- cor(T140$logreduction, predict(ll140))^2

# Non-linear regression, Weibull, T=100
wb100 <- nls(logreduction~(time/a)^b, T100, start = list(a=18, b=0.5))
summary(wb100)
rsquare.wb100 <- cor(T100$logreduction,predict(wb100))^2

# Non-linear regression, Weibull, T=110
wb110 <-nls(logreduction~(time/a)^b, T110, start = list(a=11.39, b=0.62))
summary(wb110)
rsquare.wb110 <- cor(T110$logreduction,predict(wb110))^2

# Non-linear regression, Weibull, T=115
wb115 <- nls(logreduction~(time/a)^b, T115, start = list(a=4.78, b=0.55))
summary(T115$logreduction, predict(wb115))
rsquare.wb115 <- cor(T115$logreduction,predict(wb115))^2

# Non-linear regression, Weibull, T=120
wb120 <- nls(logreduction~(time/a)^b, T120, start = list(a=5, b=0.8))
summary(wb120)
summary(T120$logreduction, predict(wb120))
rsquare.wb120 <- cor(T120$logreduction,predict(wb120))^2

# Non-linear regression, Weibull, T=130
wb130 <- nls(logreduction~(time/a)^b, T130, start = list(a=3.58, b=0.9))
summary(wb130)
rsquare.wb130 <- cor(T130$logreduction,predict(wb130))^2

# Non-linear regression, Weibull, T140
wb140 <- nls(logreduction~(time/a)^b, T140, start = list(a=1.55, b=0.87))
summary(wb140)
rsquare.wb140 <- cor(T140$logreduction,predict(wb140))^2

# Compare AIC and BIC of linear regression and weibull model

# T=100
ll100.aic <- AIC(ll100, k=2)
summary(ll100.aic)
wb100.aic <- AIC(wb100, k=2)
summary(wb100.aic )

ll100.bic <- BIC(ll100)
summary(ll100.bic)
wb100.bic <-BIC(wb100)
summary(wb100.bic)

# T=110
ll110.aic <- AIC(ll110, k=2)
summary(ll110.aic)
wb110.aic <- AIC(wb110, k=2)
summary(wb110.aic )

ll110.bic <- BIC(ll110)
summary(ll110.bic)
wb110.bic <-BIC(wb110)
summary(wb110.bic)
## Weibull regression is a better model when T=110

# T=115
ll115.aic <- AIC(ll115, k=2)
summary(ll115.aic)
wb115.aic <- AIC(wb115, k=2)
summary(wb100.aic )

ll115.bic <- BIC(ll115)
summary(ll115.bic)
wb115.bic <-BIC(wb115)
summary(wb115.bic)
## Weibull regression is a better model when T=115

# T=120
ll120.aic <- AIC(ll120, k=2)
summary(ll120.aic)
wb120.aic <- AIC(wb120, k=2)
summary(wb120.aic )

ll120.bic <- BIC(ll120)
summary(ll120.bic)
wb120.bic <-BIC(wb120)
summary(wb120.bic)

# T=130
ll130.aic <- AIC(ll130, k=2)
summary(ll130.aic)
wb130.aic <- AIC(wb130, k=2)
summary(wb130.aic )

ll130.bic <- BIC(ll130)
summary(ll130.bic)
wb130.bic <-BIC(wb130)
summary(wb130.bic)
## Weibull model is better when T=130

# T=140
ll140.aic <- AIC(ll140, k=2)
summary(ll140.aic)
wb140.aic <- AIC(wb140, k=2)
summary(wb140.aic )

ll140.bic <- BIC(ll140)
summary(ll140.bic)
wb140.bic <-BIC(wb140)
summary(wb140.bic)

# Draw figures for log-linear models
library(ggplot2)
# T=100
p.ll100 <- ggplot(T100,aes(x=time, y=-logreduction) )+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(A) T=100°C") +
  geom_smooth(method=lm, se=TRUE, formula = y~0+x, level=0.95, colour='black', 
              fullrange=TRUE, fill='lightskyblue3') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)")

# T=110

p.ll110 <- ggplot(T110,aes(x=time, y=-logreduction) )+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(B) T=110°C") +
  geom_smooth(method=lm, se=TRUE, formula = y~0+x, fullrange=TRUE, level=0.95, colour='black', fill='lightskyblue3') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=115
p.ll115 <- ggplot(T115,aes(x=time, y=-logreduction) )+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(C) T=115°C") +
  geom_smooth(method=lm, se=TRUE, formula = y~0+x, fullrange=TRUE, level=0.95, colour='black', fill='lightskyblue3') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=120

p.ll120 <- ggplot(T120,aes(x=time, y=-logreduction) )+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(D) T=120°C") +
  geom_smooth(method=lm, se=TRUE, formula = y~0+x, fullrange=TRUE, level=0.95, colour='black', fill='lightskyblue3') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=130

p.ll130 <- ggplot(T130,aes(x=time, y=-logreduction) )+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(E) T=130°C") +
  geom_smooth(method=lm, se=TRUE, formula = y~0+x, fullrange=TRUE, level=0.95, colour='black', fill='lightskyblue3') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=140

p.ll140 <- ggplot(T140,aes(x=time, y=-logreduction) )+
  theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(F) T=140°C") +
  geom_smooth(method=lm, se=TRUE, formula = y~0+x, fullrange=TRUE, level=0.95, colour='black', fill='lightskyblue3') +
  stat_summary(geom = "point", fun.y = mean, position = "dodge", width = 0.7) +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge") + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# Combine all plots using the defined multiplot function

# Define the multiplot function multiplot 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
  # Make the panel
  # ncol: Number of columns of plots
  # nrow: Number of rows needed, calculated from # of cols
  layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                   ncol = cols, nrow = ceiling(numPlots/cols))
}

if (numPlots==1) {
  print(plots[[1]])
  
  } else {
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    # Get the i,j matrix positions of the regions that contain this subplot
    matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
    
    print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                    layout.pos.col = matchidx$col))
  }
}
}

p.all.ll <- multiplot(p.ll100, p.ll120, p.ll110, p.ll130, p.ll115,p.ll140, cols=3)

# Plot weibull regression

# T=100

p.wb100 <- ggplot(T100,aes(x=time, y=-logreduction))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  geom_smooth(method=nls, data=T100,
              method.args = list(formula = y~-(x/a)^b, start = list(a=18.6, b=0.62)),
              se=FALSE, colour='black') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(A) T=100°C") +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=110
p.wb110 <- ggplot(T110,aes(x=time, y=-logreduction))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  geom_smooth(method=nls, data=T110,
              method.args = list(formula = y~-(x/a)^b, start = list(a=11, b=0.62)),
              se=FALSE, colour='black') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(B) T=110°C") +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=115
p.wb115 <- ggplot(T115,aes(x=time, y=-logreduction))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  geom_smooth(method=nls, data=T115,
              method.args = list(formula = y~-(x/a)^b, start = list(a=5, b=0.5)),
              se=FALSE, colour='black') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(C) T=115°C") +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)")

# T=120

p.wb120 <- ggplot(T120,aes(x=time, y=-logreduction))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  geom_smooth(method=nls, data=T120,
              method.args = list(formula = y~-(x/a)^b, start = list(a=10, b=1)),
              se=FALSE, colour='black') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(D) T=120°C") +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=130
p.wb130 <- ggplot(T130,aes(x=time, y=-logreduction))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  geom_smooth(method=nls, data=T130,
              method.args = list(formula = y~-(x/a)^b, start = list(a=3.5, b=0.9)),
              se=FALSE, colour='black') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(F) T=130°C") +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# T=140
p.wb140 <- ggplot(T140,aes(x=time, y=-logreduction))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  geom_smooth(method=nls, data=T140,
              method.args = list(formula = y~-(x/a)^b, start = list(a=1.5, b=0.87)),
              se=FALSE, colour='black') +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 12, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(F) T=140°C") +
  stat_summary(geom = "point", fun.y = mean, position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = mean_se, position = "dodge", width = 0.7) + 
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("Roasting time (min)") 

# Combine all weibull plots
p.all.wb <- multiplot(p.wb100, p.wb120, p.wb110, p.wb130, p.wb115,p.wb140, cols=3)

# Plot loglinear and weibull in the same plot
library("ggsci")
library("gridExtra")
# T=100
p.both100 <- ggplot(T100,aes(x=time, y=-logreduction) )+
  theme_bw() +
  geom_jitter() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(A) T=100°C") +
  geom_smooth(method=lm, se=FALSE, formula = y~0+x, 
              fullrange=TRUE, colour='#cd534c99',linetype="dashed") +
  geom_smooth(method=nls, data=T100,
              method.args = list(formula = y~-(x/a)^b, start = list(a=18.6, b=0.62)),
              se=FALSE, colour='#0073c299') +
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("")

# T=110
p.both110 <- ggplot(T110,aes(x=time, y=-logreduction) )+
  theme_bw() +
  geom_jitter() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(B) T=110°C") +
  geom_smooth(method=lm, se=FALSE, formula = y~0+x, 
              fullrange=TRUE, colour='#cd534c99',linetype="dashed") +
  geom_smooth(method=nls, data=T110,
              method.args = list(formula = y~-(x/a)^b, start = list(a=11, b=0.62)),
              se=FALSE, colour='#0073c299') +
  ylab("Salmonella reduction \n(Log10CFU/bean)") + xlab("")

# T=115
p.both115 <- ggplot(T115,aes(x=time, y=-logreduction) )+
  theme_bw() +
  geom_jitter() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(C) T=115°C") +
  geom_smooth(method=lm, se=FALSE, formula = y~0+x, 
              fullrange=TRUE, colour='#cd534c99',linetype="dashed") +
  geom_smooth(method=nls, data=T115,
              method.args = list(formula = y~-(x/a)^b, start = list(a=5, b=0.55)),
              se=FALSE, colour='#0073c299') +
  xlab("Roasting time (min)") +ylab("")

# T=120
p.both120 <- ggplot(T120,aes(x=time, y=-logreduction) )+
  theme_bw() +
  geom_jitter() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(D) T=120°C") +
  geom_smooth(method=lm, se=FALSE, formula = y~0+x, 
              fullrange=TRUE, colour='#cd534c99',linetype="dashed") +
  geom_smooth(method=nls, data=T120,
              method.args = list(formula = y~-(x/a)^b, start = list(a=10, b=0.99)),
              se=FALSE, colour='#0073c299') +
  xlab("Roasting time (min)") + ylab("")

# T=130
p.both130 <- ggplot(T130,aes(x=time, y=-logreduction) )+
  theme_bw() +
  geom_jitter() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(E) T=130°C") +
  geom_smooth(method=lm, se=FALSE, formula = y~0+x, 
              fullrange=TRUE, colour='#cd534c99',linetype="dashed") +
  geom_smooth(method=nls, data=T130,
              method.args = list(formula = y~-(x/a)^b, start = list(a=4, b=0.9)),
              se=FALSE, colour='#0073c299') +
  ylab("") + xlab("")

# T=140
p.both140 <- ggplot(T140,aes(x=time, y=-logreduction) )+
  theme_bw() +
  geom_jitter() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), panel.border = element_rect (colour = "black")) +
  theme(plot.margin = unit(c(1,1,1,1),"cm"))+
  theme(axis.text.x = element_text(size = 10, vjust = 0.5, hjust = 0.5, color = "black"))+
  theme(axis.text.y = element_text(size = 12, vjust =0.5, hjust =0.5, color = "black"))+
  theme(axis.title.x = element_text(size = 12, face = "bold", vjust = -2, hjust = 0.5,color = "black")) +
  theme(axis.title.y = element_text(size = 12,face = "bold", vjust = 4, hjust = 0.5,color = "black")) +
  theme(plot.title = element_text(size = 12,face = "bold"))+
  ggtitle("(F) T=140°C") +
  geom_smooth(method=lm, se=FALSE, formula = y~0+x, 
              fullrange=TRUE, colour='#cd534c99',linetype="dashed") +
  geom_smooth(method=nls, data=T140,
              method.args = list(formula = y~-(x/a)^b, start = list(a=1.5, b=0.87)),
              se=FALSE, colour='#0073c299') +
  ylab("") + xlab("")
  


# Combine all plots
p.both.all <- multiplot(p.both100, p.both110, p.both115, p.both120, p.both130,p.both140, cols=3)

# Secondary model- z value
# according to the paperhttps://meridian.allenpress.com/jfp/article/421053?searchresult=1&searchresult=1#10585681
# It is possible to calculate a z value from the average D, where the poser parameter=1 in the Weibull model.

Z.linear <- nls(LogD.ll~Temp/a+b, LMF_Secondary, start=list(a=-1, b=150))
summary(Z.linear)

Z.weibull <- nls(LogDelta~Temp/a+b, LMF_Secondary, start=list(a=-1, b=150))
summary(Z.weibull)

# Serotype ANOVA analysis
summary(aov(Reduction~Serotype, data=LMF_Serotype))

