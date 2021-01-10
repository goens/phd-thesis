library(tidyverse)
library(readr)


norm_time <- function(time,mapper){
map = which('gbm' == mapper)
return(time[map][1])
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
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
meta <- read_csv("data/heuristics_vs_metaheuristics.csv")
heuristics <- read_csv("data/heuristics.csv")
results <- full_join(meta,heuristics)
results_relative <- group_by(results,tgff,platform) %>% #kpn.name,
  mutate(relative_time = estimated_total_time/norm_time(estimated_total_time,mapper),
         relative_best = best_mapping_time/norm_time(best_mapping_time,mapper)) %>%
  ungroup()

results_summary <- group_by(results_relative,mapper,platform) %>% #kpn.name,
  summarize(relative_time_gmean = gm_mean(relative_time),
            relative_best_gmean = gm_mean(relative_best)
            )                                   

filter(results_relative, mapper != "random_walk") %>%
ggplot() +
  geom_col(position='dodge2',mapping = aes(x=tgff,y=relative_best,fill=mapper))  +
  scale_y_log10() +
  facet_wrap(~platform)

p1 <- filter(results_summary, mapper != "random_walk") %>%
  ggplot() +
  geom_col(position='dodge2',mapping = aes(x=platform,y=relative_best_gmean,fill=mapper)) 

p2 <- filter(results_summary, mapper != "random_walk") %>%
  ggplot() +
  geom_col(position='dodge2',mapping = aes(x=platform,y=relative_time_gmean,fill=mapper)) 

multiplot(p1,p2)
