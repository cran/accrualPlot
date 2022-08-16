## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=7.5,
  fig.height=3
)

## ----setup--------------------------------------------------------------------
library(accrualPlot)

## -----------------------------------------------------------------------------
data(accrualdemo)
head(accrualdemo)

## -----------------------------------------------------------------------------
df <- accrual_create_df(accrualdemo$date)
print(df, head = TRUE)

## -----------------------------------------------------------------------------
df2 <- accrual_create_df(accrualdemo$date, by = accrualdemo$site)
print(df2, head = TRUE)

## -----------------------------------------------------------------------------
df3 <- accrual_create_df(accrualdemo$date, start_date = as.Date("2020-07-08"))

## -----------------------------------------------------------------------------
start_date<-as.Date(c("2020-07-09","2020-07-09","2020-08-01"))
names(start_date)<-c("Site 1","Site 2","Site 3")
df4 <- accrual_create_df(accrualdemo$date, by = accrualdemo$site, start_date = start_date)

## ---- include = FALSE---------------------------------------------------------
oldpar <- par(no.readonly = TRUE)

## -----------------------------------------------------------------------------
par(mfrow = c(1, 3))
plot(df)
plot(df2)
plot(df4)

## -----------------------------------------------------------------------------
library(patchwork)
library(ggplot2)
p1 <- plot(df, engine = "ggplot")
p2 <- plot(df2, engine = "ggplot") + 
   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         axis.title.x = element_blank())
p3 <- plot(df4, engine = "ggplot") +
  labs(col = "Site") +
  theme_classic() +
  theme(legend.position = c(.35,.9),
     legend.key.height = unit(2, "mm"),
		 legend.text=element_text(size=8),
		 legend.title=element_blank(),
     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
		 axis.title.x = element_blank())
p1 + p2 + p3

## -----------------------------------------------------------------------------
par(mfrow = c(1, 3))
plot(df, which = "abs", unit = "week")
plot(df2, which = "abs", unit = "week", legend.list=list(x="topleft"), xlabsel=seq(1,20,by=2))
plot(df4, which = "abs", unit = "month",legend.list=list(x="topleft"))

## -----------------------------------------------------------------------------
p1 <- plot(df, which = "abs", unit = "week", engine = "ggplot")
p2 <- plot(df2, which = "abs", unit = "week", engine = "ggplot") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
         axis.title.x = element_blank())
p3 <- plot(df4, which = "abs", unit = "month", engine = "ggplot") +
  labs(fill = "Site") +
  theme_classic() +
  theme(legend.position = c(0.01,0.9),
     legend.justification = "left",
     legend.key.height = unit(2, "mm"),
     legend.key.width = unit(2, "mm"),
		 legend.title=element_blank(),
     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
		 axis.title.x = element_blank())
p1 + p2 + p3

## -----------------------------------------------------------------------------
plot(df2, which = "abs", unit = "week", engine = "ggplot") + facet_wrap(~site)

## -----------------------------------------------------------------------------
par(mfrow = c(1, 3))
plot(df, which = "predict", target = 300, cex_prediction=0.9)
plot(df2, which = "predict", target = 300, cex_prediction=0.9)
plot(df4, which = "predict", target = 300, cex_prediction=0.9,  center_legend="strip")

## ---- fig.height=6,out.width = '70%'------------------------------------------
plot(df4, which = "predict", target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300),
     show_center=FALSE)

## ---- fig.height=4------------------------------------------------------------
p1 <- plot(df, which = "predict", target = 300, engine = "ggplot2") +
  theme(plot.title.position = "plot")
p2 <- plot(df2, which = "predict", target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300),
  engine = "ggplot2") +
  labs(col = NULL) +
  theme_classic() +
  theme(legend.position = c(.025,.9),
        legend.justification = "left",
        legend.key.height = unit(2, "mm"),
        legend.key.width = unit(2, "mm"),
        legend.background = element_rect(fill = NA),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank())
p1 + p2

## ---- eval = FALSE------------------------------------------------------------
#  gg_accrual_plot_predict(df2, target=c("Site 1"=160,"Site 2"=100,"Site 3"=40,"Overall"=300))

## ---- fig.height=4, out.width = '90%'-----------------------------------------
par(mfrow = c(1, 2))
plot(df4, which = "predict", target = as.Date("2020-12-31"), cex_prediction=0.9,  center_legend="strip")
target<-rep(as.Date("2020-12-31"),4)
names(target)<-c("Site 1","Site 2","Site 3","Overall")
plot(df4, which = "predict", target=target,show_center=FALSE)

## -----------------------------------------------------------------------------
# accrual_table(df) 
summary(df, unit = "day") 
summary(df2, unit = "day") 
summary(df3, unit = "day") 
summary(df3, unit = "day", header = FALSE) 

## ---- include = FALSE---------------------------------------------------------
par(oldpar)

