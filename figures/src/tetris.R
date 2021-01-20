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



af_cfs_i1 <- mutate(read_csv("data/tetris/af_four_cfs.csv.i1", col_names = FALSE), instance=1)
af_cfs_i2 <- mutate(read_csv("data/tetris/af_four_cfs.csv.i2", col_names = FALSE), instance=2)
af_cfs_i3 <- mutate(read_csv("data/tetris/af_four_cfs.csv.i3", col_names = FALSE), instance=3)
af_cfs_i4 <- mutate(read_csv("data/tetris/af_four_cfs.csv.i4", col_names = FALSE), instance=4)

af_cfs <- full_join(af_cfs_i1,
          full_join(af_cfs_i2,          
          full_join(af_cfs_i3,af_cfs_i4))) %>%
  mutate(mapping = 'CFS')


af_wct_i1 <- mutate(read_csv("data/tetris/af_four_wct.csv.i1", col_names = FALSE), instance=1)
af_wct_i2 <- mutate(read_csv("data/tetris/af_four_wct.csv.i2", col_names = FALSE), instance=2)
af_wct_i3 <- mutate(read_csv("data/tetris/af_four_wct.csv.i3", col_names = FALSE), instance=3)
af_wct_i4 <- mutate(read_csv("data/tetris/af_four_wct.csv.i4", col_names = FALSE), instance=4)

af_wct <- full_join(af_wct_i1,
          full_join(af_wct_i2,          
          full_join(af_wct_i3,af_wct_i4))) %>%
  mutate(mapping = '$T_2$')


af_wsil_i1 <- mutate(read_csv("data/tetris/af_four_wsil.csv.i1", col_names = FALSE), instance=1)
af_wsil_i2 <- mutate(read_csv("data/tetris/af_four_wsil.csv.i2", col_names = FALSE), instance=2)
af_wsil_i3 <- mutate(read_csv("data/tetris/af_four_wsil.csv.i3", col_names = FALSE), instance=3)
af_wsil_i4 <- mutate(read_csv("data/tetris/af_four_wsil.csv.i4", col_names = FALSE), instance=4)

af_wsil <- full_join(af_wsil_i1,
          full_join(af_wsil_i2,          
          full_join(af_wsil_i3,af_wsil_i4))) %>%
  mutate(mapping = '$T_3$')

af_ct_i1 <- mutate(read_csv("data/tetris/af_four_ct.csv.i1", col_names = FALSE), instance=1)
af_ct_i2 <- mutate(read_csv("data/tetris/af_four_ct.csv.i2", col_names = FALSE), instance=2)
af_ct_i3 <- mutate(read_csv("data/tetris/af_four_ct.csv.i3", col_names = FALSE), instance=3)
af_ct_i4 <- mutate(read_csv("data/tetris/af_four_ct.csv.i4", col_names = FALSE), instance=4)

af_ct <- full_join(af_ct_i1,
          full_join(af_ct_i2,          
          full_join(af_ct_i3,af_ct_i4))) %>%
  mutate(mapping = '$T_1$')

all_data <- full_join(af_ct,full_join(af_wct,full_join(af_wsil,af_cfs))) %>%
  transmute(instance = factor(instance),
            mapping = mapping,
            wall_clock_time = X3 + X4,
            energy = X6 + X7 + X8 + X9,
            cpu_time = X5)

p1<- ggplot(data = all_data) +
  geom_boxplot(mapping = aes(x=mapping,y=wall_clock_time,fill=instance,group=paste(instance,mapping))) +
  labs(y="Wall-clock time [s]") +
  theme(legend.position = "right",text = element_text(size=14))  +
  scale_fill_brewer(palette="Blues")
p2<- ggplot(data = all_data) +
  geom_boxplot(mapping = aes(x=mapping,y=cpu_time,fill=instance,group=paste(instance,mapping))) +
  labs(y="CPU time [s]") +
  theme(legend.position = "right",text = element_text(size=14))  +
  scale_fill_brewer(palette="Blues")
p3 <- ggplot(data = all_data) +
  geom_boxplot(mapping = aes(x=mapping,y=energy,fill=instance,group=paste(instance,mapping))) +
  labs(y="Energy [J]") +
  theme(legend.position = "right",text = element_text(size=14))  +
  scale_fill_brewer(palette="Greens")

tikz("generated/tetris_experiment.tex",width=7,height=5,standAlone = FALSE)
print(multiplot(p1,p3,p2,cols=2))
dev.off()
