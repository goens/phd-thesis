library(scales)
library(tidyverse)
library(tikzDevice)
asinh_trans <- function(){
    trans_new(name = 'asinh', transform = function(x) asinh(x), 
              inverse = function(x) sinh(x))
}
pca_gh <- read.csv("data/clgen_pca_github.csv")
pca_synth <- read.csv("data/clgen_pca_synth.csv")
pca_benchmarks <- read.csv("data/clgen_pca_benchmarks.csv")
pca <- full_join(full_join(mutate(pca_synth,dataset='CLGen (Synth.)'),
                 mutate(pca_gh,dataset='Github')),
                 mutate(pca_benchmarks,dataset='Benchmarks')) %>%
  mutate(dataset = fct_relevel(dataset,'Benchmarks','Github','CLGen (Synth.)'))



mixed_palette <- c(brewer.pal(n=7,name='Blues')[c(3,5)],
                   brewer.pal(n=9,name="Greens")[c(3)],
                   brewer.pal(n=7,name='Blues')[c(7)],
                   brewer.pal(n=9,name="Greens")[c(5)])
tikz('generated/clgen_pca.tex',standAlone = FALSE, width = 7,height = 2.5)
ggplot( data = pca) +
  geom_density(alpha = 0.8, mapping = aes(x=principal_component_1,fill = dataset, y=..ndensity..)) +
  scale_x_continuous(trans = 'asinh') +
  scale_fill_manual(values=mixed_palette) +
  labs(x = "Principal Component 1 (asinh)", y= "Rel. freq. (smoothed)") +
  theme(legend.position = "top",legend.title = element_blank(), text=element_text(size=14)) 
dev.off()

accuracy_exp1 <- read.csv("data/clgen_accuracy1.csv")
accuracy_exp2 <- read.csv("data/clgen_accuracy2.csv")
exp_1 <- pivot_longer(accuracy_exp1,
                      cols=c('Bench....GH','Bench....Synth.','Benchmarks'),
                      names_to='setup',values_to='accuracy')
exp_2 <- pivot_longer(accuracy_exp2,
                      cols=c('GitHub','Synthesized'),
                      names_to='setup',values_to='accuracy')
accuracies <- full_join(exp_2,exp_1) %>%
  mutate(setup = fct_relevel(setup,'Benchmarks','GitHub','Synthesized','Bench....GH','Bench....Synth.'))
  
setups <- c(
  Bench....GH = 'Bench. + Github',
  Bench....Synth. = 'Bench. + CLGen',
  Benchmarks = 'Benchmarks',
  GitHub = 'Github',
  Synthesized = 'CLgen'
)

latex_percent = scales::label_percent(suffix = '\\%')
tikz('generated/clgen_accuracy.tex',standAlone = FALSE, width = 8,height = 2)
ggplot(data = accuracies) +
  geom_boxplot(mapping = aes(fill=setup,group=setup,y=accuracy)) +
  scale_y_continuous(labels = latex_percent) +
  scale_fill_manual(values=mixed_palette,labels=setups) +
  theme(text=element_text(size=22),legend.position = "right",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank())  +
  labs(y="Accuracy [\\%]")

dev.off()
