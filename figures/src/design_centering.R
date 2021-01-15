library(readr)
library(tidyverse)
library(tikzDevice)
library(RColorBrewer)

d1 <- read.csv("data/design-centering-exynos-audio_filter.csv")
d2 <- read.csv("data/design-centering-mppa_coolidge-audio_filter.csv")
d3 <- read.csv("data/design-centering-exynos-hog.csv")
d4 <- read.csv("data/design-centering-mppa_coolidge-hog.csv")
d5 <- read.csv("data/design-centering-exynos-speaker_recognition.csv")
d6 <- read.csv("data/design-centering-mppa_coolidge-speaker_recognition.csv")
#d6 <- d5
design_centering <- full_join(d1,
                    full_join(d2,
                    full_join(d3,
                    full_join(d4,
                    full_join(d5,d6)))))
                              
hml_fct <- factor(c('high','med','low'),levels=c('low','med','high'))
hml_val <- c(75,50,25)
thresholds <- group_by(design_centering,platform,kpn) %>%
  mutate(`threshold level` = hml_fct[match(threshold,sort(unique(threshold)))],
         `pert. type` = design_centering.perturbation.perturbation_type,
         threshold_pct = hml_val[match(threshold,sort(unique(threshold)))]
         )  %>%
  ungroup()
thresholds$representation <- fct_relevel(thresholds$representation,'SimpleVector','MetricSpaceEmbedding')
apps <- c("audio_filter" = "audio filter", "hog" = "HOG", 'speaker_recognition' = "speaker recognition")
emptylevels <- c("low" = "", "med" = "", 'high' = "")

mixed_palette <- c(brewer.pal(n=5,name='Blues')[c(4)],brewer.pal(n=9,name="Greens")[c(5)])
tikz("generated/design_centering_exynos.tex",width=8,height=4,standAlone=F)
print(
filter(thresholds,platform == 'exynos', `pert. type` == 'representation') %>%
ggplot() +
  geom_boxplot(mapping = aes(x=type,y=passed,fill=representation)) +
  geom_hline(mapping = aes(yintercept=threshold_pct),linetype=2) +
  facet_wrap(kpn~`threshold level`,labeller = labeller(`threshold level`=emptylevels, kpn=apps,.multi_line = F)) +
  scale_fill_manual(values=mixed_palette) +
  scale_x_discrete(labels=c('center'='center','rand'='other')) +
  labs(y="Perturbations passed [\\%]",x="Point type")  +
  theme(text = element_text(size=15),legend.position = 'top',legend.title = element_blank())
)
dev.off()

tikz("generated/design_centering_coolidge.tex",width=8,height=4,standAlone=F)
print(
filter(thresholds,platform == 'mppa_coolidge',`pert. type` == 'representation') %>%
ggplot() +
  geom_boxplot(mapping = aes(x=type,y=passed,fill=representation)) +
  geom_hline(mapping = aes(yintercept=threshold_pct),linetype=2) +
  facet_wrap(kpn~`threshold level`,labeller = labeller(kpn=apps,`threshold level`=emptylevels,.multi_line=F)) +
  scale_fill_manual(values=mixed_palette) +
  scale_x_discrete(labels=c('center'='center','rand'='other')) +
  labs(y="Perturbations passed [\\%]",x="Point type")  +
  theme(text = element_text(size=15),legend.position = 'top',legend.title = element_blank())
)
dev.off()
