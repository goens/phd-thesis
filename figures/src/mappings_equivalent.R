library(tidyverse)
library(tikzDevice)

#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
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



ct1 <- read_csv("data/ct1.stats", col_names = FALSE)
ct2 <- read_csv("data/ct2.stats", col_names = FALSE)
ct3 <- read_csv("data/ct3.stats", col_names = FALSE)
ct4 <- read_csv("data/ct4.stats", col_names = FALSE)

ct1m <- mutate(ct1,mapping=1)
ct2m <- mutate(ct2,mapping=2)
ct3m <- mutate(ct3,mapping=3)
ct4m <- mutate(ct4,mapping=4)

equiv <- full_join(ct1m, full_join(ct2m, full_join(ct3m,ct4m))) %>%
  transmute(mapping = factor(mapping),
            cpu_time = X1,
            wall_clock_time = X3,
            energy = X4 + X5 + X6 + X7)
ggplot(data = equiv) +
  geom_histogram(mapping = aes(x=wall_clock_time,fill=mapping)) + facet_wrap(~mapping)

mixed_palette <- c(brewer.pal(n=9,name="Greens")[c(4,5,6,7,8,9)])

ggplot(data = equiv) +
  geom_histogram(mapping = aes(x=energy,fill=mapping)) + facet_wrap(~mapping)
p1 <- ggplot(data = equiv) +
  geom_boxplot(mapping = aes(x=mapping,fill=mapping,y=wall_clock_time,group=mapping)) +
  scale_fill_manual(values =mixed_palette) +
  labs(x=element_blank(),y="Wall-clock time [s]") +
  theme(axis.text.x=element_blank(),legend.position = "bottom",text = element_text(size=14))  +
  guides(fill=guide_legend(nrow=1,title.position="top"))
p2 <- ggplot(data = equiv) +
  geom_boxplot(mapping = aes(x=mapping,fill=mapping,y=cpu_time,group=mapping)) + 
  scale_fill_manual(values =mixed_palette) +
  labs(x=element_blank(),y="CPU time [s]") +
  theme(axis.text.x=element_blank(),legend.position = "bottom",text = element_text(size=14))  +
  guides(fill=guide_legend(nrow=1,title.position="top"))
p3 <- ggplot(data = equiv) +
  geom_boxplot(mapping = aes(x=mapping,fill=mapping,y=energy,group=mapping)) + 
  scale_fill_manual(values =mixed_palette) +
  labs(x="mapping",y="Energy [J]") +
  theme(axis.text.x=element_blank(),legend.position = "bottom",text = element_text(size=14))  +
  guides(fill=guide_legend(nrow=1,title.position="top"))


tikz("generated/mappings_equivalent.tex",width=7,height=3,standAlone = FALSE)
print(multiplot(p1,p2,p3,cols=3))
dev.off()
summary(aov(energy~mapping,data = equiv))
summary(aov(energy~mapping,data = filter(equiv,mapping!=2 & mapping !=1)))
summary(aov(wall_clock_time~mapping,data = equiv))
summary(aov(cpu_time~mapping,data = equiv))

m1 <- filter(equiv,mapping==3)$energy
ks.test(m1[1:25],m1[26:51])
ggplot(data = equiv) +
  geom_boxplot(mapping = aes(x=mapping,fill=mapping,y=energy,group=mapping))
