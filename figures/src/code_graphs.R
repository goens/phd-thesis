library(readr)
library(tidyverse)
library(tikzDevice)
#install.packages("ggforce")
library(ggforce)
library(scales)
library("RColorBrewer")
library(ggpubr)

#https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#https://stackoverflow.com/questions/20123147/add-line-break-to-axis-labels-and-ticks-in-ggplot
addline_format <- function(x,...){
    gsub('\\s','\n',x)
}

########################
#   Read everything    #
########################
devmap_grouped <- read_csv("data/devmap_grouped_baseline.csv")
devmap_grouped <- mutate(devmap_grouped,Model=ifelse(Model == "GNN-LLVM", "GNN-CDFG",Model))
devmap_random <- read_csv("data/devmap_random_baseline.csv")
devmap_random <- mutate(devmap_random,Model=ifelse(Model == "GNN-LLVM", "GNN-CDFG",Model))
differential_grouped <- read_csv("data/devmap_grouped.csv")
differential_random <- read_csv("data/devmap_random.csv")


########################
#      Grouped         #
########################
#Add baseline models to differential data 
cols <- c("Model","Platform","Benchmark","Benchmark Suite","Oracle Mapping","Predicted Mapping","Correct?","Speedup","Fold","num_trainable_parameters")
grouped_differential <- rbind(devmap_grouped[cols],differential_grouped[cols])
differential_with_accuracy <- group_by(grouped_differential,Model) %>%
    summarize(Accuracy=sum(`Correct?`)/length(`Correct?`),Speedup=gm_mean(Speedup)) %>%
    ungroup()
grouped_with_accuracy <- mutate(devmap_grouped,Model=ifelse(Model == "GNN-LLVM", "GNN-CDFG",Model)) %>%
   group_by(Model,Platform,`Benchmark Suite`) %>%
   summarize(Accuracy=sum(`Correct?`)/length(`Correct?`),Speedup=gm_mean(Speedup)) %>%
   ungroup()

grouped_models <-c("Random mapping", "Static mapping",  "Grewe et al.", "DeepTune", "inst2vec", "GNN CFG", "GNN CDFG", "GNN CDFG + CALL", "GNN CDFG + CALL + MEM", "GNN AST","GNN AST + DF") 
differential_with_accuracy$`Model` <- factor(differential_with_accuracy$`Model`,levels = grouped_models)

#add gmeans
grouped_gmeans <- group_by(grouped_with_accuracy,Model,Platform) %>%
    summarize(mean_accuracy = mean(Accuracy), Gmean_speedup = gm_mean(Speedup)) %>%
    ungroup() %>%
    transmute(Model=Model,Platform=Platform,`Benchmark Suite`="Mean",Accuracy=mean_accuracy,Speedup=Gmean_speedup)
grouped_with_gmeans <- rbind(grouped_with_accuracy,grouped_gmeans)

#sort
grouped_with_gmeans$`Benchmark Suite` <- factor(grouped_with_gmeans$`Benchmark Suite`,levels = c("AMD SDK","NPB","NVIDIA SDK","Parboil","Polybench","Rodinia","SHOC","Mean"))
grouped_with_gmeans$`Model` <- factor(grouped_with_gmeans$`Model`,levels = c("Random mapping","Static mapping","Grewe et al.", "DeepTune", "inst2vec", "GNN-CDFG", "GNN-AST"))

########################
#      Random          #
########################

random_with_accuracy <- group_by(devmap_random,Model,Platform) %>%
  summarize(Accuracy=sum(`Correct?`)/length(`Correct?`),Speedup=gm_mean(Speedup)) %>%
  ungroup()

#Add baseline models to differential data 
cols <- c("Model","Platform","Benchmark","Benchmark Suite","Oracle Mapping","Predicted Mapping","Correct?","Speedup","Fold","num_trainable_parameters")
rd_differential <- rbind(devmap_random[cols],differential_random[cols])
differential_with_accuracy_rd <- group_by(rd_differential,Model) %>%
    summarize(Accuracy=sum(`Correct?`)/length(`Correct?`),Speedup=gm_mean(Speedup)) %>%
    ungroup()
differential_with_accuracy_rd$`Model` <- factor(differential_with_accuracy_rd$`Model`,levels = c("Random mapping", "Static mapping", "Grewe et al.", "DeepTune", "inst2vec", "GNN CFG", "GNN CDFG", "GNN CDFG + CALL", "GNN CDFG + CALL + MEM", "GNN AST","GNN AST + DF"))


#sort
random_with_accuracy$`Model` <- factor(random_with_accuracy$`Model`,levels = c("Random mapping","Static mapping","Grewe et al.", "DeepTune", "inst2vec", "GNN-CDFG", "GNN-AST"))

########################
#  Make plots          #
########################

#tikz("./devmap-grouped-accuracy.tex",standAlone = FALSE,height=3.5,width=6)
#print(ggplot(data = grouped_with_gmeans) +
#      geom_col(width=0.7,position='dodge',mapping = aes(x=`Benchmark Suite`,y=Accuracy,fill=Model)) +
#      facet_wrap(~Platform) +
#      labs(fill='Method',color='') +
#      scale_fill_brewer(palette='Set1') +
#      scale_color_brewer(palette='Set1') +
##      theme_dark(base_size = 18) +
#      theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1)))#, legend.position = c(0.9,0.85))
#dev.off()
latex_percent = scales::label_percent(suffix = '\\%')
mixed_palette <- c(brewer.pal(n=5,name='Blues')[c(3,4,5)],brewer.pal(n=9,name="Greens")[c(4,5,6,7,8,9)])
df_rd_plt <- ggplot(data = filter(differential_with_accuracy_rd, Model != "Static mapping" & Model != "Random mapping") )+
    geom_col(width=0.7,position=position_dodge(width=TRUE),mapping = aes(x=`Model`,y=Accuracy,fill=Model)) +
    geom_text(size = 4, position = position_dodge(width=TRUE),mapping = aes(x=`Model`,y=Accuracy+0.035,colour=Model,label=round(Accuracy,digits=2))) +
    theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),text=element_text(size=20)) +
    scale_color_manual(values = mixed_palette) +
    scale_fill_manual(values = mixed_palette) +
    #scale_fill_manual(limits = grouped_models[c(3,4,5,6,7,8,9,10,11)],values = c(brewer.pal(n = 12, name = "Set3"),muted("blue"))[c(3,4,5,10,11,12,6,13,7)]) +
    #scale_color_manual(values = c(brewer.pal(n = 12, name = "Set3"),muted("blue"))[c(3,4,5,10,11,12,6,13,7)]) +
    scale_y_continuous(labels = latex_percent, limits=c(0,1)) +
    guides(color=FALSE) +
    labs(title="Random split", x = "", y = "Accuracy [\\%]",color='')
df_gp_plt <- ggplot(data = filter(differential_with_accuracy, Model != "Static mapping" & Model != "Random mapping") )+
    geom_col(width=0.7,position=position_dodge(width=TRUE),mapping = aes(x=`Model`,y=Accuracy,fill=Model)) +
    geom_text(size = 4, position = position_dodge(width=TRUE),mapping = aes(x=`Model`,y=Accuracy+0.03,colour=Model,label=round(Accuracy,digits=2))) +
    theme(axis.text.x = element_blank(),axis.ticks.x=element_blank(),text=element_text(size=20)) +
    guides(color=FALSE) +
    scale_color_manual(values = mixed_palette) +
    scale_fill_manual(values = mixed_palette) +
    #scale_fill_manual(values = c(brewer.pal(n = 12, name = "Set3"),muted("blue"))[c(3,4,5,10,11,12,6,13,7)]) +
    #scale_color_manual(values = c(brewer.pal(n = 12, name = "Set3"),muted("blue"))[c(3,4,5,10,11,12,6,13,7)]) +
    scale_y_continuous(labels = latex_percent, limits=c(0,1)) +
    labs(title="Grouped split", x = "",y = "Accuracy [\\%]",color='')
tikz("generated/graph_representations_code.tex",standAlone = FALSE,height=4,width=12)
print(ggarrange(df_rd_plt,df_gp_plt, nrow=1,common.legend=TRUE, legend='right'))
dev.off()

