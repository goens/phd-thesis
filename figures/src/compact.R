library(tidyverse)
library(tikzDevice)
library(RColorBrewer)
compact.mappings <- read.csv("data/compact.csv")
grouped <- group_by(compact.mappings,applications,app,type,fire.rate)
means <- summarize(grouped,delay.avg=mean(delay.avg..cycles.),delay.var=mean(delay.var..cycles.),throughput=mean(throughput..flits.cycle.),overlap=mean(overlap))
print.scenario <- function(x){
    return(ifelse(x %in% c("['1'  '2'  '3'  '4'  '5'  '6'  '7'  '8'  '9']", "['0']"), "isolated", "joint"))
}
append.freqstr <- function(strings){
    paste("Injection rate: ", strings)
}
#tikz('figs/rms.tex',standAlone = FALSE, width = 14,height = 5)
#ggplot(data=ungroup(filter(means,fire.rate != 0.01)),mapping = aes(x=type,y=delay.var,color=type))+
#    geom_boxplot(outlier.shape = NA) + #hide outlires because added by jitter
#    #geom_jitter(width=0.14) +
#    facet_wrap(~fire.rate, labeller = labeller(fire.rate = append.freqstr))+
#    labs(y = "RMS network delay / cycles") +
#    scale_y_log10()
#dev.off()

tikz('generated/compact_latency.tex',standAlone = FALSE, width = 10,height = 5)
ggplot(data=ungroup(filter(means,fire.rate != 0.01)),mapping = aes(x=type,y=delay.avg))+
    geom_boxplot(fill=rgb(0.1,0.6,0.6),alpha=0.9) + #hide outlires because added by jitter
    #geom_jitter(width=0.14) +
    facet_wrap(~fire.rate, labeller = labeller(fire.rate = append.freqstr))+
    theme(text=element_text(size=22),legend.title=element_blank(),axis.text.x = element_text(angle=45,vjust=0.4)) + 
    labs(y = "Avg. network delay / cycles", x = element_blank())
dev.off()

by.scenario <- means %>%
    mutate(case = print.scenario(x = applications)) %>%
    group_by(app,type,fire.rate,case) %>%
    mutate(case.type = paste(case,type))
                                        #  summarize(delay.avg = sum(delay.avg))
#ggplot(data = filter(by.scenario, fire.rate != -1.01)) +
#    geom_col(size = .8, mapping = aes(x=mapply(toString,app),y=delay.avg,fill=case),position=position_dodge(),width=0.8) +
#    facet_wrap(type~fire.rate)

by.scenario.allapps.full <- compact.mappings %>%
    mutate(case = print.scenario(x = applications)) %>%
    group_by(type,fire.rate,case)
by.scenario.allapps <-  summarize(by.scenario.allapps.full, delay.avg = mean(delay.avg..cycles.))


#ggplot(data = filter(ungroup(by.scenario.allapps.full), fire.rate != 0.01 & app == 0)) +   labs(y = "Average network delay / cycles") +
#    geom_boxplot(mapping = aes(x=type,y=delay.avg..cycles.,color=case),position=position_dodge()) +
#    facet_wrap(~fire.rate, labeller = labeller(fire.rate = append.freqstr)) + theme_light(base_size=22) #+  scale_y_log10() 

mixed_palette <- c(brewer.pal(n=5,name='Blues')[c(4)],brewer.pal(n=9,name="Greens")[c(5)])
tikz("generated/compact_cases.tex",standAlone = FALSE, width = 8,height = 4)
print(ggplot(data = filter(ungroup(by.scenario.allapps.full), app == 0)) +
          labs(y = "Average network delay / cycles", x="Mapping type") +
          geom_boxplot(mapping = aes(x=type,y=delay.avg..cycles.,color=case),position=position_dodge()) +
          facet_wrap(~fire.rate, labeller = labeller(fire.rate = append.freqstr)) +
          scale_color_manual(values = mixed_palette) +
          theme(text = element_text(size=18),legend.position="top") +
          scale_y_log10() )
dev.off()
    #print(filter(ungroup(by.scenario.allapps.full), fire.rate == rate & app == 0))
#ggplot(data = filter(ungroup(by.scenario.allapps.full), fire.rate == 0.001 & app != 0)) +   labs(y = "RMS network delay / cycles") +
#    geom_boxplot(mapping = aes(x=type,y=sqrt(delay.var..cycles.),color=case),position=position_dodge(),notch=TRUE) +
#    facet_wrap(~fire.rate, labeller = labeller(fire.rate = append.freqstr)) + theme_light(base_size=22) +  scale_y_log10() 
#
