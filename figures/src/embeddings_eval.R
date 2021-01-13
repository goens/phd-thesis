library(readr)
library(tidyverse)
library(tikzDevice)
library(RColorBrewer)
library(tsne)
library(scales)
library(rsq)
library(ggrastr)
library(ggforce)

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


vectorized_rel_runtime <- Vectorize(rel_runtime,vectorize.args=c('r1','r2'))

if( !file.exists("data/metrics_rel_distances.csv")){
mappings_exynos <- read_csv("data/randoms-metrics-exynos.csv")
mappings_coolidge <- read_csv("data/randoms-metrics-coolidge.csv")
mappings_simplevector <- read_csv("data/random-metrics-simplevector.csv")
mappings <- full_join(mappings_exynos,full_join(mappings_coolidge,mappings_simplevector))
pruned <- select(mappings,platform,mapping,representation,representation.target_distortion,representation.extra_dimensions,runtime) 
pruned$representation <- fct_relevel(pruned$representation, "SimpleVector", "MetricSpaceEmbedding", "SymmetryEmbedding")
left <- rename(pruned,m1 = mapping,r1 = runtime)
right <- rename(pruned,m2 = mapping,r2 = runtime)
tuples <- full_join(left,right)
subset_mappings <- group_by(tuples,platform,representation.target_distortion,representation.extra_dimensions,representation) %>%
  sample_n(size = 1000) %>%
  ungroup()
product <- group_by(subset_mappings,platform,representation.target_distortion,representation.extra_dimensions,representation) %>%
  transmute(rel_distance = vectorized_rel_distance(m1,m2),
            rel_runtime = vectorized_rel_runtime(r1,r2))
write.csv(product,"data/metrics_rel_distances.csv")
} else{
 product <- read.csv("data/metrics_rel_distances.csv") 
}
product$representation <- fct_relevel(product$representation, "SimpleVector", "MetricSpaceEmbedding", "SymmetryEmbedding")


increased <- mutate(product, rel_distance = ifelse(representation=="SimpleVector",rel_distance,rel_distance*1))

increased_scenarios <- increased  %>%
  mutate(scenario=paste(representation,ifelse(is.na(representation.extra_dimensions),'',ifelse(representation.extra_dimensions,'ED','No-ED')),sep='\n')) %>%
  distinct()
increased_scenarios$scenario <- fct_relevel(increased_scenarios$scenario,'MetricSpaceEmbedding\nNo-ED', 'SymmetryEmbedding\nNo-ED', 'MetricSpaceEmbedding\nED', 'SymmetryEmbedding\nED','SimpleVector\n')

tikz(file="generated/metric_spaces_comparison_exynos.tex",standAlone = F,width=12,height=4)
print(
  filter(increased_scenarios,platform == 'exynos') %>%
  #sample_n(min(200,length(rel_runtime))) %>% #to make it easier on latex
  ggplot(mapping = aes(x=rel_distance,y=rel_runtime,color=factor(representation.target_distortion))) +
  geom_point_rast() +
  facet_wrap(~scenario,scale='free') +
  scale_color_brewer(palette = "Greens",na.value="grey50",name="(target)\ndistortion") +
  labs(x="relative distance",y="relative runtime")  +
  theme(text=element_text(size=18),
        legend.position = c(0.8,0.2)) 
  
)
dev.off()
tikz(file="generated/metric_spaces_comparison_coolidge.tex",standAlone = F,width=12,height=4)
print(
  filter(increased_scenarios,platform == 'mppa_coolidge') %>%
  #sample_n(min(200,length(rel_runtime))) %>% #to make it easier on latex
  ggplot(mapping = aes(x=rel_distance,y=rel_runtime,color=factor(representation.target_distortion))) +
  geom_point_rast() +
  facet_wrap(~scenario,scale='free') +
  scale_color_brewer(palette = "Blues",na.value="grey50",name="(target)\ndistortion") +
  labs(x="relative distance",y="relative runtime")  +
  theme(text=element_text(size=18),
        legend.position = c(0.8,0.2)) 
  
)
dev.off()
only_max <- increased_scenarios %>%
  group_by(representation,
           platform,
           representation.target_distortion,
           representation.extra_dimensions) %>%
  arrange(rel_distance) %>%
  transmute(rel_distance = rel_distance,
            max_rel_runtime = accumulate(rel_runtime,max),scenario=scenario) %>%
  distinct() %>%
  ungroup() %>%
  group_by(representation,
           scenario,
           platform,
           representation.target_distortion,
           representation.extra_dimensions,
           rel_distance) %>%
  summarize(max_rel_runtime = max(max_rel_runtime)) %>%
  mutate(representation.target_distortion = factor(representation.target_distortion))

tikz(file="generated/metric_spaces_comparison_max_exynos.tex",standAlone = F,width=12,height=4)
print(
  filter(only_max,platform == 'exynos') %>%
#sample_n(min(50,length(rel_runtime))) %>% #to make it easier on latex
ggplot(mapping = aes(x=rel_distance,y=max_rel_runtime,color=representation.target_distortion)) +
  scale_color_brewer(palette = "Greens",na.value="grey50",name="(target)\ndistortion") +
  geom_smooth(formula=y~x,method = 'lm') +
  geom_point_rast() +
  labs(x="relative distance",y="relative runtime") +
  facet_wrap(platform~scenario,scale="free") + 
  theme(text=element_text(size=18),
        legend.position = c(0.8,0.2)) 
)
dev.off()
tikz(file="generated/metric_spaces_comparison_max_coolidge.tex",standAlone = F,width=12,height=4)
print(
filter(only_max,platform == 'mppa_coolidge') %>%
#sample_n(min(50,length(rel_runtime))) %>% #to make it easier on latex
ggplot(mapping = aes(x=rel_distance,y=max_rel_runtime,color=representation.target_distortion)) +
  scale_color_brewer(palette = "Blues",na.value="grey50",name="(target)\ndistortion") +
  geom_smooth(formula=y~x,method = 'lm') +
  geom_point_rast() +
  labs(x="relative distance",y="relative runtime") +
  facet_wrap(~scenario,scale="free") + 
  theme(text=element_text(size=18),
        legend.position = c(0.8,0.2)) 
)
dev.off()

metrics_max <- group_by(only_max,representation.target_distortion,representation.extra_dimensions,representation,platform) %>%
  summarize(
    rsq_val = rsq(lm(rel_distance~max_rel_runtime)),
    rho = cor.test(rel_distance,max_rel_runtime, method="spearman")$statistic,
    rho = cor.test(rel_distance,max_rel_runtime, method="spearman")$p.value,
  ) %>% mutate(scenario=paste(representation,ifelse(is.na(representation.extra_dimensions),'',ifelse(representation.extra_dimensions,'ED','No-ED')),sep='\n'))

metrics_max$scenario <- fct_relevel(metrics_max$scenario,'MetricSpaceEmbedding\nNo-ED', 'SymmetryEmbedding\nNo-ED', 'MetricSpaceEmbedding\nED', 'SymmetryEmbedding\nED','SimpleVector\n')
embedding_labels <- c('MetricSpaceEmbedding\nNo-ED' = 'M.S.Emb.\nNo-ED', 'SymmetryEmbedding\nNo-ED' = 'Sym.+Emb.\nNo-ED', 'MetricSpaceEmbedding\nED'='M.S.Emb.\nED', 'SymmetryEmbedding\nED'='Sym.+Emb.\nED','SimpleVector\n'='Simp. Vec.\n')
platform_labels <- c('exynos' = "Odroid XU4", 'mppa_coolidge' = "MPPA3 Coolidge")

p1 <-  filter(metrics_max,platform == 'exynos') %>%
ggplot() +
  geom_col(position = position_dodge(preserve='single'),
           color = 'black',
           mapping = aes(x=scenario,
                         y=rsq_val,fill=representation.target_distortion)) +
  scale_fill_brewer(palette = "Greens",na.value="grey50",name = "(target)\ndistortion") +
  theme(axis.text.x = element_text(angle=90,hjust=0.7,vjust=0.4),
        text=element_text(size=18),
        legend.position="top",
        axis.ticks.x = element_blank()) +
  scale_x_discrete(labels=embedding_labels) +
  labs(x=element_blank(),y="Linear Regression $R^2$")  +
  facet_wrap(~platform,labeller = labeller(platform=platform_labels))
p2 <-  filter(metrics_max,platform == 'mppa_coolidge') %>%
ggplot() +
  geom_col(position = position_dodge(preserve='single'),
           color = 'black',
           mapping = aes(x=scenario,
                         y=rsq_val,fill=representation.target_distortion)) +
  scale_fill_brewer(palette = "Blues",na.value="grey50",name = "(target)\ndistortion") +
  theme(axis.text.x = element_text(angle=90,hjust=0.7,vjust=0.4),
        text=element_text(size=18),
        legend.position="top",
        axis.ticks.x = element_blank()) +
  scale_x_discrete(labels=embedding_labels) +
  labs(x=element_blank(),y=" ")  +
  facet_wrap(~platform,labeller = labeller(platform=platform_labels))

tikz("generated/metrics_regression_rsq.tex",width=8,height=4,standAlone = F)
print(multiplot(p1,p2,cols=2) )  
dev.off()


thresholds <- c(38.5,45.3,70.4)
jlt_projection <- read.csv("data/randoms-metrics-exynos.projection.csv")
projection_w_thresholds <- mutate(jlt_projection,
                                  t1 = runtime <= thresholds[1],
                                  t2 = runtime <= thresholds[2],
                                  t3 = runtime <= thresholds[3],
                                  num_thresholds = factor(t1+t2+t3))

mse <- filter(projection_w_thresholds,representation == 'MetricSpaceEmbedding',
       representation.target_distortion == '1.001', representation.extra_dimensions == 'True'
       )  %>% mutate( t1 = ifelse(t1,'feasible','infeasible'),
                      t2 = ifelse(t2,'feasible','infeasible'), 
                      t3 = ifelse(t3,'feasible','infeasible'), 
                      )
mse$t1 <- fct_relevel(mse$t1,'infeasible', 'feasible')  
mse$t2 <- fct_relevel(mse$t2,'infeasible', 'feasible')  
mse$t3 <- fct_relevel(mse$t3,'infeasible', 'feasible')  
empty <- tibble()
p1 <- ggplot(mse) +
  geom_point_rast(mapping = aes(x=x,y=y,color=t1),size=1)  +
  geom_ellipse(data=empty, aes(x0=1.95,y0=3.9,a=0.9,b=0.45,angle=10))  +
  scale_color_brewer(palette="Set1") +
  theme(text=element_text(size=18),legend.position='bottom') +
  labs(x="1. Dimension (projection)", y="2. Dimension (projection)",title="Low threshold", color=element_blank())
p2 <- ggplot(mse) +
  geom_point_rast(mapping = aes(x=x,y=y,color=t2),size=1)  +
  geom_ellipse(data=empty,aes(x0=2.3,y0=4.2,a=1.25,b=0.52,angle=10))  +
  scale_color_brewer(palette="Set1") +
  theme(text=element_text(size=18),legend.position='bottom') +
  labs(x="1. Dimension (projection)", y="2. Dimension (projection)",title="Med. threshold", color=element_blank())
p3 <- ggplot(mse) +
  geom_point_rast(mapping = aes(x=x,y=y,color=t3),size=1)  +
  geom_ellipse(data=empty,aes(x0=3.0,y0=4.1,a=1.99,b=0.61,angle=0))  +
  scale_color_brewer(palette="Set1") +
  theme(text=element_text(size=18),legend.position='bottom') +
  labs(x="1. Dimension (projection)", y="2. Dimension (projection)", title="High threshold", color=element_blank())
tikz('generated/design_center_mapping_spaces.tex',width=10,height=4,standAlone=F)
print(multiplot(p1,p2,p3,cols=3))
dev.off()
