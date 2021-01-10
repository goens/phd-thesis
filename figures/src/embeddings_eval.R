library(readr)
library(tidyverse)
library(tikzDevice)
library(tsne)
library(scales)
library(rsq)

rankQuantileCut <- function(a,n){
    a_ranks <- rank(a, ties.method = "first")
    return(cut(a_ranks, quantile(a_ranks, probs=0:n/n),ordered_result =TRUE,labels=FALSE))
}

euclidean_distance <- function(v1,v2){
    diff <- v1 - v2
    sq <- diff ** 2
    dist <- sqrt(sum(sq))
    return(dist)
}

parse_mappings <- function(data){
    n <- length(read.table(text=data[[1]]))
    res <- matrix(, ncol = n, nrow = 0)
    for (x in data){
        l <- gsub('\\[','',x)
        l <- gsub('\\]','',l)
        l <- gsub(',','',l)
        parsed_line <- read.table(text=l)
        res <- rbind(res,parsed_line)
    }
    return(res)
}

rel_distance <- function(m1, m2){
  euclidean_distance(parse_mappings(m1),parse_mappings(m2))
}

vectorized_rel_distance <- Vectorize(rel_distance,vectorize.args=c('m1','m2'))

rel_runtime <- function(r1, r2){
  return(max(r1/r2,r2/r1))
}

vectorized_rel_runtime <- Vectorize(rel_runtime,vectorize.args=c('r1','r2'))

if( !file.exists("data/metrics_rel_distances.csv")){
mappings <- read_csv("data/randoms_multiple_metrics.csv")
pruned <- select(mappings,mapping,representation,representation.target_distortion,representation.extra_dimensions,runtime) 
pruned$representation <- fct_relevel(pruned$representation, "SimpleVector", "MetricSpaceEmbedding", "SymmetryEmbedding")
left <- rename(pruned,m1 = mapping,r1 = runtime)
right <- rename(pruned,m2 = mapping,r2 = runtime)
tuples <- full_join(left,right)
subset_mappings <- group_by(tuples,representation.target_distortion,representation.extra_dimensions,representation) %>%
  sample_n(size = 1000) %>%
  ungroup()
product <- group_by(subset_mappings,representation.target_distortion,representation.extra_dimensions,representation) %>%
  transmute(rel_distance = vectorized_rel_distance(m1,m2),
            rel_runtime = vectorized_rel_runtime(r1,r2))
} else{
 product <- read.csv("data/metrics_rel_distances.csv") 
}

write.csv(product,"data/metrics_rel_distances.csv")

increased <- mutate(product, rel_distance = ifelse(representation=="SimpleVector",rel_distance,rel_distance*1))

increased_scenarios <- increased  %>%
  mutate(scenario=paste(representation,ifelse(is.na(representation.extra_dimensions),'',ifelse(representation.extra_dimensions,'ED','No-ED')),sep='\n')) %>%
  distinct()
increased_scenarios$scenario <- fct_relevel(increased_scenarios$scenario,'MetricSpaceEmbedding\nNo-ED', 'SymmetryEmbedding\nNo-ED', 'MetricSpaceEmbedding\nED', 'SymmetryEmbedding\nED','SimpleVector\n')

tikz(file="generated/metric_spaces_comparison.tex",standAlone = F,width=12,height=4)
print(
  sample_n(increased_scenarios,200) %>% #to make it easier on latex
  ggplot(mapping = aes(x=rel_distance,y=rel_runtime,color=factor(representation.target_distortion))) +
  geom_point() +
  facet_wrap(~scenario,scale='free') +
  scale_color_brewer(palette = "Greens",na.value="grey50",name="(target)\ndistortion") +
  labs(x="relative distance",y="relative runtime")  +
  theme(text=element_text(size=18),
        legend.position = c(0.8,0.2)) 
  
)
dev.off()
only_max <- increased_scenarios %>%
  group_by(representation,
           representation.target_distortion,
           representation.extra_dimensions) %>%
  arrange(rel_distance) %>%
  transmute(rel_distance = rel_distance,
            max_rel_runtime = accumulate(rel_runtime,max),scenario=scenario) %>%
  distinct() %>%
  ungroup() %>%
  group_by(representation,
           scenario,
           representation.target_distortion,
           representation.extra_dimensions,
           rel_distance) %>%
  summarize(max_rel_runtime = max(max_rel_runtime)) %>%
  mutate(representation.target_distortion = factor(representation.target_distortion))

tikz(file="generated/metric_spaces_comparison_max.tex",standAlone = F,width=12,height=4)
print(
sample_n(only_max,50) %>%
ggplot(mapping = aes(x=rel_distance,y=max_rel_runtime,color=representation.target_distortion)) +
  scale_color_brewer(palette = "Greens",na.value="grey50",name="(target)\ndistortion") +
  geom_smooth(formula=y~x,method = 'lm') +
  geom_point() +
  labs(x="relative distance",y="relative runtime") +
  facet_wrap(~scenario,scale="free") + 
  theme(text=element_text(size=18),
        legend.position = c(0.8,0.2)) 
)
dev.off()

metrics_max <- group_by(only_max,representation.target_distortion,representation.extra_dimensions,representation) %>%
  summarize(
    rsq_val = rsq(lm(rel_distance~max_rel_runtime)),
    rho = cor.test(rel_distance,max_rel_runtime, method="spearman")$statistic,
    rho = cor.test(rel_distance,max_rel_runtime, method="spearman")$p.value,
  ) %>% mutate(scenario=paste(representation,ifelse(is.na(representation.extra_dimensions),'',ifelse(representation.extra_dimensions,'ED','No-ED')),sep='\n'))
metrics_max$scenario <- fct_relevel(metrics_max$scenario,'MetricSpaceEmbedding\nNo-ED', 'SymmetryEmbedding\nNo-ED', 'MetricSpaceEmbedding\nED', 'SymmetryEmbedding\nED','SimpleVector\n')

tikz("generated/metrics_regression_rsq.tex",width=8,height=3.5,standAlone = F)
print(
ggplot(data = metrics_max) +
  geom_col(position = position_dodge(preserve='single'),
           color = 'black',
           mapping = aes(x=scenario,
                         y=rsq_val,fill=representation.target_distortion)) +
  scale_fill_brewer(palette = "Greens",na.value="grey50",name = "(target)\ndistortion") +
  theme(axis.text.x = element_text(angle=25,hjust=0.7,vjust=0.4),
        text=element_text(size=18),
        axis.ticks.x = element_blank()) +
  labs(x=element_blank(),y="Linear Regression $R^2$") 
 ) 
dev.off()
