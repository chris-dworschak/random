
# An empty shell for creating an event heat map, CD 2020

rm(list=ls())
library(zoo)
set.seed(1)

#### USER: set your country and time period of interest: ####
country.of.interest <- "Andorra"
time.period.of.interest <- c("2018/01/01", "2020/09/01")



# providing full time period and country sample
plot.time <- seq(as.Date("1993/01/01"), as.Date("2020/09/01"), by="months")
country.vector <- c("Albania", "Andorra", "Azerbaijan")
df <- data.frame(time = rep(plot.time, length(country.vector)),
                 country = sort(rep(country.vector, length(plot.time))))

# restricting data frame to sample of interest
df1 <- df[df$country==country.of.interest & 
             df$time<=as.Date(time.period.of.interest[2]) & 
             df$time>=as.Date(time.period.of.interest[1]), ]

# providing data of categorical index components to be visualized
df.comp.cat <- data.frame(cat.comp.1 = rpois(nrow(df1), 1),
                          cat.comp.2 = rpois(nrow(df1), 3))

# providing data of binary index components to be visualized, standardizing with ref to cat comps
df.comp.bin <- data.frame(binary.comp.1 = ifelse( rbinom(nrow(df1), 1, 0.2) == 1, max(df.comp.cat), min(df.comp.cat) ),
                          binary.comp.2 = ifelse( rbinom(nrow(df1), 1, 0.3) == 1, max(df.comp.cat), min(df.comp.cat) ))

# combining data frames
df2 <- data.frame(df1, df.comp.cat, df.comp.bin)

x.axis.points <- c(1, round(length(df2$time)/4), round(length(df2$time)/2), round(length(df2$time)/1.3), length(df2$time))
x.axis.labels <- gsub("-\\d{2}$", "", df2$time[x.axis.points])
x <- as.matrix(df2[ ,3:ncol(df2)])

par(mar=c(13,10,13,2))
image(1:nrow(x),1:ncol(x), x, axes=F, ylab="", xlab="")
mtext(side=2, text="Structural & Events", line=8, cex = 1.1)
axis(1, x.axis.points, label=x.axis.labels)
axis(2, 1:ncol(x), colnames(x), las=1)
abline(h = 2.5, lwd = 4)
dev.off()

heatmap(t(x), Rowv = NA, Colv = NA, 
        main = paste0("Crisis components for ", country.of.interest, ", ", 
                      gsub("/\\d{2}$", "", time.period.of.interest[1]), " - ", 
                      gsub("/\\d{2}$", "", time.period.of.interest[2])), 
        labRow = colnames(x), labCol = x.axis, margins = c(31,7), scale = "none")


