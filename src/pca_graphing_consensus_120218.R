

#highlighting time points 
#https://stackoverflow.com/questions/4733182/how-to-highlight-time-ranges-on-a-plot
#https://stackoverflow.com/questions/32543176/highlight-areas-within-certain-x-range-in-ggplot2

#graphing multiple lines on sample graph
#https://rpubs.com/MarkusLoew/226759

setwd("C:/Users/soren/Google Drive/Biomedical MultipliciTs/1. Evidence Infrastructure/Prostate Cancer/Networks/Graphing Consensus")

#install.packages('ggplot2')
#install.packages('plotly')

library(ggplot2)
library(plotly)

obs <- read.csv("Measuring Consensus - Raw Data.csv", stringsAsFactors = FALSE)

rm(pp)
p <- ggplot(obs, aes(x = Year))
p <- p + geom_line(aes(y = LogLeichtMod, colour = "Scaled Modularity"))
p

rect <- data.frame(xmin=1987, xmax=1990, ymin=-Inf, ymax=Inf)
pp <- p + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              color="grey20",
              alpha=0.5,
              inherit.aes = FALSE)


## Determine highlighted regions
v <- rep(1, 37)
rects2 <- v[c(7:11, 16:19, 26:28)] 

## Get the start and end points for highlighted regions
inds <- diff(c(0, v))
start <- obs$Year[inds == 1]
end <- obs$Year[inds == -1]
if (length(start) > length(end)) end <- c(end, tail(obs$Year, 1))

## highlight region data
rects <- data.frame(start=start, end=end, group=seq_along(start))

p <- ggplot(obs, aes(x = Year)) +
  geom_line(aes(y = LogLeichtMod, colour = "Scaled Modularity")) +
  geom_rect(data=rects2, inherit.aes = FALSE,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey20", alpha=0.5)

ggplot(data=dat, aes(dates, value)) +
  theme_minimal() +
  geom_line(lty=2, color="steelblue", lwd=1.1) +
  geom_point() +
  geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(dat$value),
                                               ymax=max(dat$value), group=group), color="transparent", fill="orange", alpha=0.3)


# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = NetworkSize, colour = "Publications Per Year"))
p
# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*.^000001, name = "Publications Per Year")) 
p + coord_trans(y = "log10")

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "Scaled Modularity",
              x = "Year",
              colour = "Label")
p <- p + theme(legend.position = c(0.8, 0.9))
p
