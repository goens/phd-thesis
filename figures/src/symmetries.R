library(tidyverse)
library(readr)
library(grid)
library(tikzDevice)
changed_raw <- filter(read_csv("data/symmetries_changed_operations.csv"),!is.na(best_mapping_time))
cache_raw <- filter(read_csv("data/symmetries_cache.csv"),!is.na(time_simulating))

norm_time <- function(time,canonical,mapper){
can = which(FALSE == canonical)
map = which('random_walk' == mapper)
return(time[intersect(can,map)[1]])
}

noncanonical_time <- function(time,canonical){
can = which(FALSE == canonical)
return(time[can[1]])
}

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

changed_w_simvec <- full_join(
  mutate(filter(cache_raw,representation == 'SimpleVector'),representation.canonical_operations = F),
  filter(changed_raw,representation.canonical_operations == T))

clean_names <- mutate(changed_w_simvec,platform=ifelse(platform == 'designer_odroid', 'Odroid XU4',ifelse(platform == 'multi_cluster','Simple cluster',ifelse(platform == "mppa_coolidge","MPPA3 Coolidge","HAEC"))))
changed_times_normed_per_app <- group_by(clean_names,mapper,platform,kpn.name,mapper.random_seed,tgff.file) %>%
  mutate(relative_best = best_mapping_time/noncanonical_time(best_mapping_time,representation.canonical_operations),
         relative_time = time_simulating/noncanonical_time(time_simulating,representation.canonical_operations)) %>%
  ungroup() 
changed_times_normed <-  group_by(changed_times_normed_per_app,representation.canonical_operations,mapper,platform) %>%
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
         ungroup()  %>%
  group_by(platform,representation.canonical_operations,mapper) %>% summarise(normed_time = median(normed_time),
                                   normed_err = median(normed_err),
                                   normed_best_mapping = median(normed_best_mapping),
                                   normed_best_err = median(normed_best_err),
                                   relative_best = gm_mean(relative_best),
                                   relative_time = gm_mean(relative_time))
  
changed_times_split <- pivot_longer(changed_times_normed,c(`normed_time`,`normed_best_mapping`),names_to='time_type',values_to='time_value')  %>%
  mutate( err = ifelse(time_type == 'normed_time',normed_err,normed_best_err)) %>%
  select(-ends_with("_err"))

changed_times_split$platform <- factor(changed_times_split$platform,levels=c("Odroid XU4", "Simple cluster","MPPA3 Coolidge", "HAEC"))
times.labs <- c("Best mapping","Exploration time")
names(times.labs) <-c("normed_best_mapping","normed_time")
mapper.labs <- c("genetic" = "genetic", "random_walk"="random walk",
                 "simulated_annealing"="simulated annealing", "tabu_search"="tabu search")

changed_summary <- filter(changed_times_normed,platform == 'MPPA3 Coolidge') %>%
  group_by(representation.canonical_operations,mapper) %>% summarize(best_tot = sum(normed_best_mapping),time_tot = sum(normed_time))
filter(changed_summary,representation.canonical_operations)$time_tot /filter(changed_summary,!representation.canonical_operations)$time_tot
filter(changed_summary,!representation.canonical_operations)$best_tot/filter(changed_summary,representation.canonical_operations)$best_tot

changed_times_per_file <- filter(clean_names,platform != '') %>%
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

multiple_scenarios <- mutate(cache_raw,scenario = paste(tgff.file,kpn.name,platform))

geom_mean <- function(a){prod(a)^(1/length(a))}
norm_time <- function(time,representation,time_type){
svs = which('SimpleVector' == representation)
simu = which('time_simulating' == time_type)
return(time[intersect(svs,simu)[1]])
}
norm_overhead <- function(time,representation,time_type){
svs = which('SimpleVector' == representation)
rept = which('time_representation' == time_type)
return(time[intersect(svs,rept)[1]])
}

  
cache_tidy_times <- mutate(multiple_scenarios,time_representation = time_representation + representation_init_time) %>%
  mutate(time_simulating = ifelse(representation == 'SimpleVector',time_simulating+time_representation,time_simulating)) %>%
  select(mapper,time_representation,time_simulating,best_mapping_time,representation,platform,tgff.file,scenario,mapper.random_seed) %>%
  pivot_longer(c(`time_representation`,`time_simulating`),names_to='time_type',values_to='time')
cache_times_overheads <- group_by(cache_tidy_times,mapper,scenario,mapper.random_seed) %>%
  mutate( time =  ifelse(time_type == 'time_representation',pmax(time - norm_overhead(time,representation,time_type),0),time)) %>% ungroup()
cache_times_mean_pre <- group_by(cache_times_overheads,representation,time_type,mapper,platform,scenario) %>%
  summarize(time_error=sd(time), time = mean(time),best_mapping=mean(best_mapping_time)) %>% ungroup()
cache_times_mean <- group_by(cache_times_mean_pre,representation,time_type,mapper,platform) %>%
  summarize(time_error=mean(time_error), time = mean(time),best_mapping=mean(best_mapping)) %>% ungroup()
cache_times_normed <- group_by(cache_times_mean,mapper,platform) %>%
  mutate(normed_time = time/norm_time(time,representation,time_type),
         normed_error = time_error/norm_time(time,representation,time_type)) %>% 
  ungroup() %>% mutate(mapper = factor(mapper)) %>%
  mutate(representation = ifelse(representation == 'SimpleVector','Regular',representation))  

cache_times_normed$platform <- factor(cache_times_normed$platform,levels=c("designer_odroid", "multi_cluster","mppa_coolidge", "haec"))
  
cache_times_normed_totals <- group_by(cache_times_normed,mapper,platform,representation) %>%
  mutate(total_normed_time = sum(normed_time), total_normed_error = sum(normed_error)) %>% ungroup() %>%
  mutate(representation = ifelse(representation == 'SimpleVector','Regular',representation))  

mapper.labs <- c("gen.", "rand. w.","sim. ann.", "tabu s.")
names(mapper.labs) <-c("genetic", "random_walk","simulated_annealing", "tabu_search") 
platform.labs <- c("Odroid XU4", "HAEC","MPPA3 Coolidge", "Simple cluster")
names(platform.labs) <- factor(c("designer_odroid", "haec","mppa_coolidge", "multi_cluster"))
tikz("figures/symmetries_genetic_cache.tex",standAlone = FALSE, width = 7, height = 2.5)
print(
  ggplot(data=filter(cache_times_normed,mapper=='genetic')) +
    geom_col(mapping = aes(x=representation,y=normed_time,fill=time_type)) +
    #geom_errorbar(data = times_normed_totals,
    #              mapping = aes(x=representation,y=total_normed_time,
    #                            min=total_normed_time - total_normed_error,
    #                            ymax=total_normed_time + total_normed_error)) +
    labs(x='Cache Type', y='Time (normed)') + 
    scale_fill_brewer(palette="Set1", name=element_blank(), labels=c('runtime overhead','simulation time')) + 
    facet_grid(cols=vars(platform),labeller = labeller(mapper = mapper.labs, platform = platform.labs))  +
    theme(legend.position = "top")  
)
dev.off()

tikz("generated/symmetries_cache.tex",standAlone = FALSE, width = 7, height = 4)
print(
  ggplot(data=cache_times_normed) +
    geom_col(mapping = aes(x=representation,y=normed_time,fill=time_type)) +
    #geom_errorbar(data = times_normed_totals,
    #              mapping = aes(x=representation,y=total_normed_time,
    #                            min=total_normed_time - total_normed_error,
    #                            ymax=total_normed_time + total_normed_error)) +
    labs(x='Cache Type', y='Relative exploration time') + 
    scale_fill_brewer(palette="Set1", name=element_blank(), labels=c('runtime overhead','simulation time')) + 
    facet_grid(cols=vars(platform),rows=vars(mapper),labeller = labeller(mapper = mapper.labs, platform = platform.labs))  +
    theme(legend.position = "top")  
)
dev.off()
filter(cache_times_normed_totals,platform == 'mppa_coolidge') %>%
  group_by(representation,mapper) %>% summarize(time_tot = sum(normed_time), factor = 1/time_tot)

tikz("generated/symmetries_changed_operations.tex",standAlone = FALSE, width = 7, height = 5.5)
blues_darker <- brewer.pal(5,"Blues")[c(3,5)]
print(ggplot(data=changed_times_split) +
      geom_col(alpha=0.8,position = "dodge2", mapping = aes(x=mapper,y=time_value,fill=representation.canonical_operations)) +
      #geom_errorbar(position= "dodge2", mapping = aes(x=mapper,y=time_value,group=representation.canonical_operations,ymax=time_value+err,ymin=time_value-err)) + 
      labs(x='Mapping algorithm', y='Time (normed,log)') + 
      scale_fill_manual(values=blues_darker, name=element_blank(), labels=c("Standard", "Changed operations") ) + 
      scale_color_manual(values=blues_darker, name=element_blank(), labels=c("Standard", "Changed operations") ) + 
      scale_x_discrete(labels=mapper.labs) +
      scale_y_log10() +
      facet_grid(time_type~platform,labeller =labeller(time_type=times.labs)) +
      theme(axis.text.x = element_text(angle = 45,  hjust=1),legend.position="top",text=element_text(size=16))  
)
dev.off()

greens_darker <- brewer.pal(7,"Greens")[c(3,1,5,7)]
mapper.labs <- c("genetic", "random walk","s. annealing", "tabu search")
names(mapper.labs) <-c("genetic", "random_walk","simulated_annealing", "tabu_search") 
tikz("generated/symmetries_coolidge_changed_operations.tex",standAlone = FALSE, width = 7, height = 2.5)
ggplot(data=filter(changed_times_normed_per_app,representation.canonical_operations==TRUE & platform == "MPPA3 Coolidge")) +
  geom_boxplot(position = 'dodge2', mapping = aes(x=processes_in_task,fill=mapper,y=relative_best,group = processes_in_task)) +
  scale_y_log10() +
  scale_fill_manual(values=greens_darker, name=element_blank(), labels=mapper.labs ) + 
  labs(x= "Number of Tasks", y = "Best mapping (rel., log)") +
  facet_wrap(~mapper, labeller = as_labeller(mapper.labs),ncol=4 ) +
  theme(legend.position="none", text = element_text(size = 16)) 
dev.off()

#HCA Chapter
tikz("/tmp/symmetries.tex",standAlone = FALSE, width = 7, height = 6)
print(
  filter(changed_times_split,platform == 'Odroid XU4' | platform == "MPPA3 Coolidge") %>%
ggplot() +
      geom_col(alpha=0.8,position = "dodge2", mapping = aes(x=mapper,y=time_value,fill=representation.canonical_operations)) +
      #geom_errorbar(position= "dodge2", mapping = aes(x=mapper,y=time_value,group=representation.canonical_operations,ymax=time_value+err,ymin=time_value-err)) + 
      labs(x='Mapping algorithm', y='Time (normed,log)') + 
      scale_fill_manual(values=blues_darker, name=element_blank(), labels=c("Standard", "Symmetries") ) + 
      scale_color_manual(values=blues_darker, name=element_blank(), labels=c("Standard", "Symmetries") ) + 
      scale_x_discrete(labels=mapper.labs) +
      scale_y_log10() +
      facet_grid(platform~time_type,labeller =labeller(time_type=times.labs)) +
      theme(axis.text.x = element_text(angle = 45,  hjust=1),legend.position="right",text=element_text(size=20))  
)
dev.off()
