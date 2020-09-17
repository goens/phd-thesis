#data source:https://github.com/karlrupp/microprocessor-trend-data 
library(tidyverse)
library(tikzDevice)
moore <- read_csv("data/moore.csv")


moore_transistors <- subset(moore,!is.na(transistors)& !is.na(frequency) & !is.na(cores) & !is.na(specint))
moore_transistors$panel <- 'Number of Transistors'

moore_specint <- subset(moore,!is.na(specint)& !is.na(frequency) & !is.na(cores) & !is.na(transistors))
moore_specint$panel <- "Single-core performance\n (SpecINT $\\times 10^3$)"


tikz("generated/moore.tex",width=7,height=4)
print(
ggplot(data=moore) +
  facet_grid(panel~., scale="free") + 
  geom_point(data = moore_transistors,mapping = aes(x=year,y=transistors,size = cores, color = frequency), stat='identity') +
  geom_point(data = moore_specint,mapping = aes(x=year,y=specint,size = cores, color = frequency), stat = 'identity') +
  scale_y_log10() +
  scale_radius(trans='log2') +
  scale_color_viridis_c(trans='log2') +
  labs(x="Year produced", y = element_blank(),color="Frequency\n (MHz)", size="Number\n of cores")
)
dev.off()
