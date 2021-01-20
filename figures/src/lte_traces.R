library(readr)
library(tidyverse)
library(tikzDevice)
library(ggrastr)
#devtools::install_github("AckerDWM/gg3D")
#library(gg3D)

if( !file.exists("data/lte_traces_summarized.csv")){
trace <- read_csv("data/slicedtrace30BS.txt.parsed.csv",
                  col_types = cols(CRNTI = col_integer(), 
                  base_station_id = col_integer(), 
                  criticality_type = col_integer(), 
                  is_new = col_logical(), layers = col_integer(), 
                  modulation_scheme = col_integer(), 
                  rb = col_integer(), subframe = col_integer()))
#trace$CRNTI <- as.factor(trace$CRNTI)
by_base_station <- group_by(trace,subframe,base_station_id) %>% 
  summarize(number_of_ues = length(unique(CRNTI)), rb_per_ue = max(rb), rb = sum(rb))
traces_summarized <- group_by(trace,subframe) %>%
  summarize(number_of_ues = length(unique(CRNTI)), rb_per_ue = max(rb), rb = sum(rb))
#param_combinations_traces <- distinct(traces_summarized[,c("number_of_ues","rb","layers")])
param_combinations_traces <- group_by(traces_summarized,number_of_ues,rb) %>%
  summarise(count = n())
param_combinations_traces_per_base_station <- group_by(by_base_station,number_of_ues,rb,rb_per_ue) %>%
  summarise(count = n()) 
write.csv(param_combinations_traces_per_base_station,"data/lte_traces_summarized.csv")
} else{
 param_combinations_traces_per_base_station <- read.csv("data/lte_traces_summarized.csv") 
}

sweep <- read_delim("data/lte_analyzes.output.csv", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
sweep_filtered <- group_by(sweep,ue,rb) %>%
  filter(layer == max(layer)) %>%
  filter(rxAnt == max(rxAnt)) %>%
  filter(carrier == min(carrier)) %>%
  ungroup() %>%
  mutate(arbitrary = eqlz/max(eqlz) + IDFT/max(IDFT))

#ue: ChanEqlz,IDFT,Decode,ChanEst,ChanInv
#rb: demod,eqlz,inv,demod,matrixInv,est

estimate_resources <- function(ue_in,rb_in){
  possibles <- filter(sweep_filtered,ue_in <= ue,rb_in <= rb)
  if (length(possibles$ue) == 0){
    return(ceiling(max(sweep_filtered$arbitrary)));
  }
  ue_sweep <- min(possibles$ue)
  possibles <- filter(possibles,ue == ue_sweep) 
  if (length(possibles$rb) == 0){
    return(ceiling(max(sweep_filtered$arbitrary)));
  }
  rb_sweep <- min(possibles$rb) 
  return(pull(filter(possibles,rb==rb_sweep),arbitrary))
}

design_points <- mutate(param_combinations_traces_per_base_station,arbitrary=Vectorize(estimate_resources)(number_of_ues,rb_per_ue))

limit <- 1.65
gen_classify_points_fun <- function(limit){
  ues <-  design_points$number_of_ues
  rbs <- design_points$rb_per_ue
  forbidden <- filter(design_points,arbitrary >= limit)
  ue_limit <- min(forbidden$number_of_ues)
  ue <- max(ues[ues < ue_limit])
  rb_limit <- min(forbidden$rb_per_ue)
  rb <- max(rbs[rbs < rb_limit])
  print(c(ue,rb))
  classify_points_fun <- function(ue_in,rb_in,arbitrary){
    if(arbitrary > limit){
      return("Impossible")
    }
    else{
      if(ue_in > ue){
        return("Dynamic")
      }
      if(rb_in > rb){
        return("Dynamic")
      }
      return("Static")
    }
  }
  return(classify_points_fun) 
}
classify_points <- Vectorize(gen_classify_points_fun(limit))
design_points <- mutate(design_points,type = classify_points(number_of_ues,rb_per_ue,arbitrary))
design_points$type <- factor(design_points$type, levels=c("Dynamic","Static","Impossible"))


("data/lte_traces_summarized.csv")
tikz("generated/lte_statistics.tex", standAlone = FALSE,width=7,height=4)
#print(
  ggplot(data = arrange(design_points,type)) + 
  geom_jitter_rast( mapping = aes(x=number_of_ues,y=rb,size=count,color=type),alpha=0.7,width=0.33,height=0) +
  scale_color_manual(values = c("Static" = "darkgreen","Dynamic" = "skyblue2", "Impossible" = "red")) + 
  scale_radius(range = c(0.5,5),trans="log10") + #breaks=c(1,10,100,1000,10000,100000)) 
  labs(x="Number of UEs",y="(Total) Number of Resource Blocks") +
  guides(color=guide_legend(title=NULL,order=1),size=guide_legend(order=2,title="Instance count")) + 
  theme(text=element_text(size=14)) #+ theme(legend.position=c(0.17,0.8))
#)
dev.off()
print(summarize( group_by(design_points,type), count = n()))

#theta <- 45
#phi <- 15
#ggplot(data = design_points, aes(x=number_of_ues,y=rb,z=arbitrary,size=count)) + 
#  axes_3D(theta=theta, phi=phi) +
#  stat_3D(theta=theta, phi=phi) + 
#  labs_3D(theta=theta, phi=phi, labs=c('ue','rb','layers'))

#number of actors micf_x   : #Layers * #Antenna (#Antenna is base station parameter, so it would be independent of UE workload)
#number of actors combwc_x : 12                 (This may independent of UEs, but actor execution time is linearly related to #PRBs)
#number of actors antcomb_x : #Layers x #Symbols (Again #Symbols is standard defined and independent of UE workload)
#number of actors demap_x : 24 (This may be independent of UEs, but actor execution time is linearly related to #PRBs and modulation scheme).
#
#micf_x     :- is Matched filter, IFFT, Windowing and FFT (Ignore the acronym for now)
#combwc_x   :- Combiner weight Calculation
#antcomb_x  :- Antenna Combiner
#demap_x    :- Soft Symbol Demap   

