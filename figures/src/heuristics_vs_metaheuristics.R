library(tidyverse)
library(readr)
library(RColorBrewer)
library(tikzDevice)

norm_time <- function(time,mapper){
map = which('genetic' == mapper)
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
meta <- read_csv("data/metaheuristics.csv")
heuristics <- read_csv("data/heuristics.csv")
results <- full_join(meta,heuristics)
results_relative <- group_by(results,tgff,platform,mapper) %>% #kpn.name,
  summarize(time_err = sd(estimated_total_time),
         estimated_total_time = mean(estimated_total_time),
         best_err = sd(best_mapping_time),
         best_mapping_time = mean(best_mapping_time),
         ) %>%
  group_by(tgff,platform) %>%  
  mutate(relative_time = estimated_total_time/norm_time(estimated_total_time,mapper),
         relative_time_err = time_err/norm_time(estimated_total_time,mapper),
         relative_time_max = relative_time + relative_time_err,
         relative_time_min = relative_time - relative_time_err,
         relative_best = best_mapping_time/norm_time(best_mapping_time,mapper),
         relative_best_err = best_err/norm_time(best_mapping_time,mapper),
         relative_best_max = relative_best + relative_best_err,
         relative_best_min = relative_best - relative_best_err,
         ) %>%
  ungroup()

results_summary <- group_by(results_relative,mapper,platform) %>% #kpn.name,
  summarize(relative_time_gmean = gm_mean(relative_time,na.rm=F),
            max_time_gmean = gm_mean(relative_time_max,na.rm=F), 
            min_time_gmean = gm_mean(relative_time_min,na.rm=F), 
            relative_best_gmean = gm_mean(relative_best,na.rm=F),
            max_best_gmean = gm_mean(relative_best_max,na.rm=F),
            min_best_gmean = gm_mean(relative_best_min,na.rm=F),
            )                                   
results_summary$mapper <- fct_relevel(results_summary$mapper,'gbm', 'static_cfs', 'random_walk', 'simulated_annealing','tabu_search','genetic')

filter(results_relative, mapper != "random_walk") %>%
ggplot() +
  geom_col(position='dodge2',mapping = aes(x=tgff,y=relative_best,fill=mapper))  +
  geom_errorbar(position='dodge2', mapping = aes(x=tgff,ymin=relative_best-relative_time_err,ymax=relative_best+relative_time_err,color=mapper))+
  #scale_y_log10() +
  facet_wrap(~platform)
mixed_palette = c(brewer.pal(n=5,name='Blues')[c(3,4)],brewer.pal(n=6,name="Greens")[c(3,4,5,6)])

mapper_labels <- c('gbm' = 'GBM', 'static_cfs' = 'Static CFS', 'random_walk' = 'Random Walk', 'simulated_annealing' = 'Sim. Annealing', 'tabu_search' = 'Tabu Search', 'genetic' = 'Genetic')
p1 <- filter(results_summary, platform == "exynos") %>%
  ggplot() +
  geom_col(position='dodge2',mapping = aes(x=mapper,y=relative_best_gmean,fill=mapper)) +
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=min_best_gmean,ymax=max_best_gmean))  +
  scale_fill_manual(values =mixed_palette,guide=F) +
  scale_x_discrete(labels=mapper_labels) +
  theme(axis.text.x = element_text(angle=45,hjust=0.5,vjust=0.5),text=element_text(size=11))+
  labs(x=element_blank(),y="Rel. mapper results",title="Exynos")
p2 <- filter(results_summary, platform == "mppa_coolidge") %>%
  ggplot() +
  geom_col(position='dodge2',mapping = aes(x=mapper,y=relative_best_gmean,fill=mapper)) +
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=min_best_gmean,ymax=max_best_gmean)) +
  scale_fill_manual(values =mixed_palette,guide=F) +
  scale_x_discrete(labels=mapper_labels) +
  theme(axis.text.x = element_text(angle=45,hjust=0.5,vjust=0.5),text=element_text(size=11))+
  labs(x=element_blank(),y="Rel. mapper results",title="MPPA3 Coolidge")

p3 <- filter(results_summary, platform == "exynos") %>%
  ggplot() +
  geom_col(position='dodge2',mapping = aes(x=mapper,y=relative_time_gmean,fill=mapper)) +
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=min_time_gmean,ymax=max_time_gmean)) +
  scale_fill_manual(values =mixed_palette,guide=F) +
  scale_x_discrete(labels=mapper_labels) +
  theme(axis.text.x = element_text(angle=45,hjust=0.5,vjust=0.5),text=element_text(size=11))+
  labs(x=element_blank(),y="Rel. execution time",title="")
  
p4 <- filter(results_summary, platform == "mppa_coolidge") %>%
  ggplot() +
  geom_col(position='dodge2',mapping = aes(x=mapper,y=relative_time_gmean,fill=mapper)) +
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=min_time_gmean,ymax=max_time_gmean)) +
  scale_fill_manual(values =mixed_palette,guide=F) +
  theme(axis.text.x = element_text(angle=45,hjust=0.5,vjust=0.5),text=element_text(size=11))+
  scale_x_discrete(labels=mapper_labels) +
  labs(x=element_blank(),y="Rel. execution time",title="")

tikz("generated/heuristics_vs_metaheuristics.tex",width=8,height=5,standAlone = F)
print(multiplot(p1,p2,p3,p4,cols=2))
dev.off()
