#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
#library(ggradar)
library(tidyverse)
library(scales)
library(tikzDevice)
#install.packages("ggforce")
library(ggforce)
#install.packages("ggiraphExtra")
library(ggiraphExtra)

tikzOptions <- c("\\usepackage{tikz}","\\usepackage[active,tightpage]{preview}","\\usepackage{amsmath}","\\PreviewEnvironment{pgfpicture}","\\setlength\\PreviewBorder{0pt}")

analyzes_output <- read_delim("data/lte_analyzes.output.csv", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)
is_pareto_cond <- function(x_in,y_in,condition){
    x <- x_in[condition]
    y <- y_in[condition]
    d = data.frame(x,y)
    D = d[order(d$x,d$y,decreasing=FALSE),]
    front = D[which(!duplicated(cummin(D$y))),]
    #print("front length:")
    #print(length(front))
    res = ifelse(paste(x_in,y_in) %in% paste(front$x,front$y),'Pareto','Non-Pareto')
    return(res)
}

tikz("generated/5g_radar.tex", standAlone = FALSE,width=9,height=5)
filter(analyzes_output, (ue ==1 |  ue == 6 | ue ==4  | ue == 10 | ue == 12 | ue == 16) &  rxAnt ==1 & layer==1 & (carrier==1 | carrier==4)) %>% 
  mutate(rb = paste("No. RB: ",rb)) %>%
 as_tibble() %>% 
 transform(group = paste("Users: ", ue, ", Carrier:", carrier)) %>%
 #mutate_each(funs(rescale), -c(group,rb)) %>%
 #tail(16) %>%
 select(group,rb,FFT,IDFT,eqlz,demod,ChanInv) %>%
 ggRadar(mapping = aes(color=group,facet=rb),
         axis.label.size = 6,
         legend.position="right",
         values.radar = "",
         axis.labels = c("FFT", 'IDFT', 'Equalization', 'Demodulation', 'Chan. Inv.'),)  +
  theme(legend.title = element_blank(), text = element_text(size=20),axis.ticks.y = element_blank())


dev.off()
 
