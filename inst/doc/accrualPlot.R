## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(accrualPlot)

## -----------------------------------------------------------------------------
set.seed(1234)
x <- as.Date("2020-12-07") + sample(c(-20:20), 50, replace = TRUE)
site <- as.factor(paste0("Site",sample(1:3, 50, replace = TRUE)))

## -----------------------------------------------------------------------------
df <- accrual_create_df(x)
print(df, head = TRUE)

## -----------------------------------------------------------------------------
df2 <- accrual_create_df(x, by = site)
print(df2, head = TRUE)

## -----------------------------------------------------------------------------
df3 <- accrual_create_df(x, start_date = as.Date("2020-11-01"))

## -----------------------------------------------------------------------------
start_date<-as.Date(c("2020-11-10","2020-11-15","2020-11-18"))
names(start_date)<-c("Site1","Site2","Site3")
df4 <- accrual_create_df(x, by = site, start_date = start_date)

## ---- include = FALSE---------------------------------------------------------
oldpar <- par(no.readonly = TRUE)

## ---- fig.width=7-------------------------------------------------------------
par(mfrow = c(1, 3))
plot(df)
plot(df2)
plot(df4)

## ---- fig.width=7-------------------------------------------------------------
library(patchwork)
library(ggplot2)
p1 <- plot(df, engine = "ggplot")
p2 <- plot(df2, engine = "ggplot")
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

## ---- fig.width=7-------------------------------------------------------------
par(mfrow = c(1, 3))
plot(df, which = "abs", unit = "week")
plot(df2, which = "abs", unit = "week")
plot(df4, which = "abs", unit = "week")

## ---- fig.width=7-------------------------------------------------------------
p1 <- plot(df, which = "abs", unit = "week", engine = "ggplot")
p2 <- plot(df2, which = "abs", unit = "week", engine = "ggplot")
p3 <- plot(df4, which = "abs", unit = "week", engine = "ggplot") +
  labs(fill = "Site") +
  ylim(0,12) +
  theme_classic() +
  theme(legend.position = c(.6,0.9),
     legend.justification = "left",
     legend.key.height = unit(2, "mm"),
     legend.key.width = unit(2, "mm"),
		 legend.title=element_blank(),
     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
		 axis.title.x = element_blank())
p1 + p2 + p3

## ---- fig.width=7-------------------------------------------------------------
par(mfrow = c(1, 3))
plot(df, which = "predict", target = 75)
plot(df2, which = "predict", target = 75)
plot(df4, which = "predict", target = 75,  center_legend="strip")

## ---- fig.width=7, fig.height=4-----------------------------------------------
p1 <- plot(df, which = "predict", target = 75, engine = "ggplot2") +
  theme(plot.title.position = "plot")
p2 <- plot(df2, which = "predict", target = c(30, 25, 35, 90), engine = "ggplot2") +
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

## ---- include = FALSE---------------------------------------------------------
par(oldpar)

## -----------------------------------------------------------------------------
# accrual_table(df) 
summary(df, unit = "day") 
summary(df2, unit = "day") 
summary(df3, unit = "day") 
summary(df3, unit = "day", header = FALSE) 

