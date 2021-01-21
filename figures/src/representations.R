library(tidyverse)
library(readr)
library(grid)
library(tikzDevice)
library(RColorBrewer)

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

slx_heuristics <- read.csv("data/heuristics-multiple-representations-slx.csv")
slx_meta <- read.csv("data/metaheuristics-multiple-representations-slx.csv")

e3s_heuristics <- read.csv("data/heuristics-multiple-representations-e3s.csv") %>%
  mutate(representation = NA)
e3s_meta <- read.csv("data/metaheuristics-multiple-representations-e3s.csv")

slx_data <- full_join(slx_heuristics,slx_meta) %>%
  mutate(benchmark = ifelse(trace == 'slx_default', 'CPN','E3S'))
e3s_data <- full_join(e3s_heuristics,e3s_meta) %>%
  mutate(benchmark = ifelse(trace == 'slx_default', 'CPN','E3S'))
full_data <- full_join(slx_data,e3s_data)

norm_time_rep <- function(time,mapper,representation){
map = which('simulated_annealing' == mapper)
sv = which('SimpleVector' == representation)
return(time[intersect(sv,map)[1]])
}
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x), na.rm=na.rm) / sum(!is.na((x))) )
}

relative_times <- group_by(full_data,platform,kpn,mapper,representation,benchmark) %>%
  summarize(best_time = mean(best_mapping_time),
            best_err = sd(best_mapping_time),
            total_time = mean(estimated_total_time),
            total_err = sd(estimated_total_time)
            ) %>%
  ungroup() %>%
  group_by(platform,kpn,benchmark) %>%
  mutate(relative_best = best_time/norm_time_rep(best_time,mapper,representation),
         relative_time = total_time/norm_time_rep(total_time,mapper,representation),
         relative_best_min = pmax(relative_best - best_err/norm_time_rep(best_time,mapper,representation), 0), 
         relative_best_max = relative_best + best_err/norm_time_rep(best_time,mapper,representation), 
         relative_total_min = pmax(relative_time - total_err/norm_time_rep(total_time,mapper,representation), 0), 
         relative_total_max = relative_time + total_err/norm_time_rep(total_time,mapper,representation), 
         ) %>%
  mutate(relative_best_min = ifelse(relative_best_min <= 0, NA, relative_best_min),
         relative_total_min = ifelse(relative_total_min <= 0, NA, relative_best_min)) %>%
  ungroup() 
relative_times$representation <- fct_relevel(relative_times$representation,'SimpleVector','Symmetries','MetricSpaceEmbedding','SymmetryEmbedding')


summarized <- group_by(relative_times,platform,mapper,representation,benchmark) %>%
  summarize(relative_best_gmean = gm_mean(relative_best),
            relative_best_gmean_min = gm_mean(relative_best_min), 
            relative_best_gmean_max = gm_mean(relative_best_max), 
            relative_total_gmean = gm_mean(relative_time), 
            relative_total_gmean_min = gm_mean(relative_total_min),  
            relative_total_gmean_max = gm_mean(relative_total_max), 
  )

summarized$mapper <- fct_relevel(summarized$mapper,'gbm', 'static_cfs', 'random_walk', 'simulated_annealing','tabu_search','genetic','gradient_descent')
mapper_labels <- c('gbm' = 'GBM', 'static_cfs' = 'Static CFS', 'random_walk' = 'Random Walk', 'simulated_annealing' = 'Sim. Annealing', 'tabu_search' = 'Tabu Search', 'genetic' = 'Genetic', 'gradient_descent' = "Grad. Descent")

summarized <- mutate(summarized,
       relative_best_gmean_min = ifelse(is.na(representation), NA, relative_best_gmean_min),
       relative_best_gmean_max = ifelse(is.na(representation), NA, relative_best_gmean_max),
       relative_total_gmean_min = ifelse(is.na(representation), NA, relative_total_gmean_min),
       relative_total_gmean_max = ifelse(is.na(representation), NA, relative_total_gmean_max),
) #remove errors for heuristics


p1 <- filter(summarized,platform == 'exynos') %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_best_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_best_gmean_min,ymax=relative_best_gmean_max))  +
  scale_fill_manual(values = brewer.pal(name="Greens",n=8)[c(4:8)],guide=F,na.value='grey50') +
  labs(x=element_blank(),y="Rel. mapper results (log)") +
  scale_x_discrete(labels=mapper_labels) +
  theme(text=element_text(size=12))+
  scale_y_log10() +
  facet_grid(rows=vars(benchmark),scales='free')

p2 <- filter(summarized,platform == 'exynos') %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_total_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_total_gmean_min,ymax=relative_total_gmean_max))  + 
  scale_fill_manual(values = brewer.pal(name="Greens",n=8)[c(4:8)],na.value='grey50') +
  labs(x=element_blank(),y="Rel. exploration time (log)") +
  scale_x_discrete(labels=mapper_labels) +
  theme(text=element_text(size=12))+
  scale_y_log10() +
  facet_grid(rows=vars(benchmark),scales='free')

p3 <- filter(summarized,platform == 'mppa_coolidge') %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_best_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_best_gmean_min,ymax=relative_best_gmean_max))  +
  scale_fill_manual(values = brewer.pal(name="Blues",n=8)[c(4:8)],guide=F,na.value='grey50') +
  labs(x=element_blank(),y="Rel. mapper results (log)") +
  scale_x_discrete(labels=mapper_labels) +
  theme(text=element_text(size=12))+
  scale_y_log10() +
  facet_grid(rows=vars(benchmark),scales='free')

p4 <- filter(summarized,platform == 'mppa_coolidge') %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_total_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_total_gmean_min,ymax=relative_total_gmean_max))  + 
  scale_fill_manual(values = brewer.pal(name="Blues",n=8)[c(4:8)],na.value='grey50') +
  labs(x=element_blank(),y="Rel. exploration time (log)") +
  scale_x_discrete(labels=mapper_labels) +
  theme(text=element_text(size=12))+
  scale_y_log10() +
  facet_grid(rows=vars(benchmark),scales='free')

tikz("generated/multiple_representations_exynos.tex",width = 8, height=6,standAlone = F)
print(multiplot(p1,p2,cols=1))
dev.off()

tikz("generated/multiple_representations_coolidge.tex",width = 8, height=6,standAlone = F)
print(multiplot(p3,p4,cols=1))
dev.off()

p5 <- filter(summarized,platform == 'mppa_coolidge' & (mapper == 'tabu_search' | mapper == 'simulated_annealing') & representation != 'Symmetries') %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_best_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_best_gmean_min,ymax=relative_best_gmean_max))  +
  scale_fill_manual(values = brewer.pal(name="Blues",n=8)[c(4:8)],guide=F,na.value='grey50') +
  labs(x=element_blank(),y="Rel. mapper results") +
  scale_x_discrete(labels=mapper_labels) +
  theme(text=element_text(size=12))+
  facet_wrap(~benchmark)
p6 <- filter(summarized,platform == 'mppa_coolidge' & (mapper == 'tabu_search' | mapper == 'simulated_annealing') & representation != "Symmetries") %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_total_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_total_gmean_min,ymax=relative_total_gmean_max))  + 
  scale_fill_manual(values = brewer.pal(name="Blues",n=8)[c(4:8)],na.value='grey50') +
  labs(x=element_blank(),y="Rel. exploration time (log)") +
  scale_x_discrete(labels=mapper_labels) +
  theme(text=element_text(size=12))+
  scale_y_log10() +
  facet_wrap(~benchmark)

tikz("generated/geometric_heuristics_coolidge.tex",width = 8, height=4,standAlone = F)
print(multiplot(p5,p6,cols=2))
dev.off()


