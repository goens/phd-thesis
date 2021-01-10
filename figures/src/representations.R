library(tidyverse)
library(readr)
library(grid)
library(tikzDevice)
multiple_reps <- filter(read_csv("data/slx-multiple-representations.csv"),!is.na(best_mapping_time))

norm_time_simple <- function(time,mapper){
map = which('genetic' == mapper)
return(time[map][1])
}
relative_times <- group_by(multiple_reps,platform,kpn,mapper,representation) %>%
  summarize(best_time = mean(best_mapping_time),
            best_err = sd(best_mapping_time),
            total_time = mean(estimated_total_time),
            total_err = sd(estimated_total_time)
            ) %>%
  group_by(platform) %>%
  mutate(relative_best = best_time/norm_time_simple(best_time,mapper),
         relative_time = total_time/norm_time_simple(total_time,mapper),
         relative_best_min = relative_best - best_err/norm_time_simple(best_time,mapper), 
         relative_best_max = relative_best + best_err/norm_time_simple(best_time,mapper), 
         relative_total_min = relative_time - total_err/norm_time_simple(total_time,mapper), 
         relative_total_max = relative_time + total_err/norm_time_simple(total_time,mapper), 
         ) %>%
  ungroup() 
relative_times$representation <- fct_relevel(relative_times$representation,'SimpleVector','Symmetries','MetricSpaceEmbedding','SymmetryEmbedding')


summarized <- group_by(relative_times,platform,mapper,representation) %>%
  summarize(relative_best_gmean = gm_mean(relative_best),
            relative_best_gmean_min = gm_mean(relative_best_min), 
            relative_best_gmean_max = gm_mean(relative_best_max), 
            relative_total_gmean = gm_mean(relative_time), 
            relative_total_gmean_min = gm_mean(relative_total_min),  
            relative_total_gmean_max = gm_mean(relative_total_max), 
  )

filter(summarized,platform == 'mppa_coolidge' | platform == 'exynos') %>%
  ggplot() +
  geom_col(position="dodge2",mapping = aes(x=mapper,y=relative_best_gmean,fill=representation)) + 
  geom_errorbar(position='dodge2',mapping = aes(x=mapper,ymin=relative_best_gmean_min,ymax=relative_best_gmean_max))  +
  facet_wrap(~platform)

####################################
 Symmetries
####################################

norm_time <- function(time,canonical,mapper){
can = which(FALSE == canonical)
map = which('genetic' == mapper)
return(time[intersect(can,map)[1]])
}

noncanonical_time <- function(time,canonical){
can = which(FALSE == canonical)
return(time[can[1]])
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

genetic <- filter(read_csv("data/changed_operations.csv"),!is.na(best_mapping_time))
clean_names <- mutate(genetic,platform=ifelse(platform == 'designer_odroid', 'Exynos',ifelse(platform == 'multi_cluster','Simple cluster',ifelse(platform == "mppa_coolidge","MPPA3 Coolidge","HAEC"))))
times_normed_per_app <- group_by(clean_names,mapper,platform,kpn.name,mapper.random_seed,tgff.file) %>%
  mutate(relative_best = best_mapping_time/noncanonical_time(best_mapping_time,representation.canonical_operations),
         relative_time = time_simulating/noncanonical_time(time_simulating,representation.canonical_operations)) %>%
  ungroup() 
times_normed <-  group_by(times_normed_per_app,representation.canonical_operations,mapper,platform) %>%
  mutate(total_time = time_representation+time_simulating+representation_init_time) %>%
  ungroup() %>% group_by(representation.canonical_operations,mapper,platform) %>%
  summarize(time = median(total_time), time_err = sd(total_time),
            best_mapping = median(best_mapping_time), best_err = sd(best_mapping_time),
            relative_time = gm_mean(relative_time),relative_best = gm_mean(relative_best)) %>%
  ungroup() %>% group_by(platform) %>%
  mutate(normed_time = time/norm_time(time,representation.canonical_operations,mapper),
         normed_err = time_err/norm_time(time,representation.canonical_operations,mapper),
         normed_best_mapping = best_mapping/norm_time(best_mapping,representation.canonical_operations,mapper),
         normed_best_err = best_err/norm_time(best_mapping,representation.canonical_operations,mapper),
         mapper = mapper) %>%
         ungroup() %>%
  group_by(platform,representation.canonical_operations,mapper) %>% summarise(normed_time = median(normed_time),
                                   normed_err = median(normed_err),
                                   normed_best_mapping = median(normed_best_mapping),
                                   normed_best_err = median(normed_best_err),
                                   relative_best = gm_mean(relative_best),
                                   relative_time = gm_mean(relative_time))
  
times_split <- pivot_longer(times_normed,c(`normed_time`,`normed_best_mapping`),names_to='time_type',values_to='time_value')  %>%
  mutate( err = ifelse(time_type == 'normed_time',normed_err,normed_best_err)) %>%
  select(-ends_with("_err"))

times_split$platform <- factor(times_split$platform,levels=c("Exynos", "Simple cluster","MPPA3 Coolidge", "HAEC"))
times.labs <- c("Best mapping","Exploration time")
names(times.labs) <-c("normed_best_mapping","normed_time")
mapper.labs <- c("genetic" = "genetic", "random_walk"="random walk",
                 "simulated_annealing"="simulated annealing", "tabu_search"="tabu search")

tikz("figures/changed_operations.tex",standAlone = FALSE, width = 7, height = 4)
print(ggplot(data=times_split) +
      geom_col(alpha=0.8,position = "dodge2", mapping = aes(x=mapper,y=time_value,fill=representation.canonical_operations)) +
      #geom_errorbar(position= "dodge2", mapping = aes(x=mapper,y=time_value,group=representation.canonical_operations,ymax=time_value+err,ymin=time_value-err)) + 
      labs(x='Mapping algorithm', y='Time (normed,log)') + 
      scale_fill_brewer(palette="Set1", name=element_blank(), labels=c("Standard", "Changed operations") ) + 
      scale_color_brewer(palette="Set1", name=element_blank(), labels=c("Standard", "Changed operations") ) + 
      scale_x_discrete(labels=mapper.labs) +
      scale_y_log10() +
      facet_grid(time_type~platform,labeller =labeller(time_type=times.labs)) +
      theme(axis.text.x = element_text(angle = 45,  hjust=1),legend.position="top")  
)
dev.off()

summary <- filter(times_normed,platform == 'MPPA3 Coolidge') %>%
  group_by(representation.canonical_operations,mapper) %>% summarize(best_tot = sum(normed_best_mapping),time_tot = sum(normed_time))
filter(summary,representation.canonical_operations)$time_tot/filter(summary,!representation.canonical_operations)$time_tot
filter(summary,!representation.canonical_operations)$best_tot/filter(summary,representation.canonical_operations)$best_tot

times_per_file <- filter(clean_names,platform != '') %>%
  group_by(representation.canonical_operations,mapper,tgff.file,kpn.name,platform) %>%
  summarize(time = median(time_simulating + time_representation + representation_init_time),
            best_mapping = median(best_mapping_time), processes = median(processes_in_task)) %>%
  ungroup() %>% 
  mutate(num_cores_in_plat = ifelse(platform == 'MPPA3 Coolidge', 85,ifelse(platform == "HAEC", 64, ifelse(platform == "Simple cluster", 4,8)))) %>%
  group_by(tgff.file,kpn.name,platform) %>%
  mutate(normed_time = time/norm_time(time,representation.canonical_operations,mapper),
         normed_best_mapping = best_mapping/norm_time(best_mapping,representation.canonical_operations,mapper),
         mapper = mapper,
         mapping_space_size = num_cores_in_plat**processes
         ) %>%
         ungroup() 

tikz("figures/genetic_changed_operations.tex",standAlone = FALSE, width = 7, height = 2.5)
ggplot(filter(times_per_file,mapper == 'genetic' & platform == "MPPA3 Coolidge")) + 
  geom_boxplot(alpha=0.5,position = "identity",mapping = aes(x=processes,y=normed_best_mapping,color=representation.canonical_operations,group=paste(representation.canonical_operations,processes))) +
  #geom_col(position = "dodge2", mapping = aes(x=tgff.file,y=normed_best_mapping,fill=representation.canonical_operations,group=paste(representation.canonical_operations,processes))) +
  scale_y_log10() +
  theme(legend.position="top")  +
  scale_color_brewer(palette="Set1", name=element_blank(), labels=c("Standard", "Changed operations") ) + 
  labs(x= "No. of Tasks", y = "Best mapping (rel., log)") +
  theme(text = element_text(size = 18)) 
dev.off()  
  

ggplot(data=filter(times_normed,representation.canonical_operations==TRUE)) + geom_col(position = 'dodge2', mapping = aes(x=platform,y=relative_best,fill=mapper))


tikz("figures/coolidge_changed_operations.tex",standAlone = FALSE, width = 7, height = 6)
ggplot(data=filter(times_normed_per_app,representation.canonical_operations==TRUE & platform == "MPPA3 Coolidge")) +
  geom_boxplot(position = 'dodge2', mapping = aes(x=processes_in_task,fill=mapper,y=relative_best,group = processes_in_task)) +
  scale_y_log10() +
  scale_fill_brewer(palette="Set1", name=element_blank(), labels=mapper.labs ) + 
  labs(x= "Number of Tasks", y = "Best mapping (rel., log)") +
  facet_wrap(~mapper, labeller = as_labeller(mapper.labs) ) +
  theme(legend.position="none", text = element_text(size = 16)) 
dev.off()










