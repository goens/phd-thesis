library(tidyverse)
#install.packages("rjson")
library(rjson)
library(RColorBrewer)
library(tikzDevice)
parse_raw <- function(raw_data){
  result <- as.data.frame(raw_data[1])
    for (i in 1:length(raw_data)){
        tmp <- as.data.frame(raw_data[i])
        result <- full_join(result,tmp)
    } 
  return(as.tibble(result))
}


#f03 <- parse_raw(fromJSON(file="data/yauhau/ghc8.json"))
#f06 <- parse_raw(fromJSON(file="data/yauhau/haskell-func-HaxlDo.json"))
#f07 <- parse_raw(fromJSON(file="data/yauhau/haskell.json"))
#f11 <- parse_raw(fromJSON(file="data/yauhau/muse-func-monad.json"))
#f12 <- parse_raw(fromJSON(file="data/yauhau/muse-monad.json"))
#f13 <- parse_raw(fromJSON(file="data/yauhau/yauhau-applicative.json"))
#f14 <- parse_raw(fromJSON(file="data/yauhau/yauhau-func-app.json"))

f15 <- parse_raw(fromJSON(file="data/yauhau/yauhau-func-monad.json"))
f10 <- parse_raw(fromJSON(file="data/yauhau/muse-func-app.json"))
f05 <- parse_raw(fromJSON(file="data/yauhau/haskell-func-HaxlDoApp.json"))

yauhau_functions <-
    rename(f05,rounds_performed = rounds_made) %>%
    mutate(Framework = 'Haxl') %>%
    select(rounds_performed,if_percentage,Framework) %>%
    full_join(
        mutate(f15,Framework = 'Yauhau') %>%
           select(rounds_performed,if_percentage,Framework) 
    ) %>%
    full_join(
        mutate(f10,Framework = 'Muse')  %>%
           select(rounds_performed,if_percentage,Framework) 
    ) 

f01 <- parse_raw(fromJSON(file="data/yauhau/concurrency-no-rewrite.json"))
f02 <- parse_raw(fromJSON(file="data/yauhau/concurrency-rewrite.json"))
f08 <- parse_raw(fromJSON(file="data/yauhau/haskell-timed.json"))

yauhau_concurrency <-
    mutate(f01,Framework = 'Yauhau\n(conc. I/O)') %>%
    select(time,levels,Framework) %>%
    full_join(
        mutate(f02,Framework = 'Yauhau') %>%
    select(time,levels,Framework) 
    ) %>%
    full_join(
        mutate(f08,Framework = 'Haxl')  %>%
    select(time,levels,Framework) 
    ) 


#seq: yauhau-monad -> read_requests
f04 <- parse_raw(fromJSON(file="data/yauhau/haskell-applicative.json"))
f16 <- parse_raw(fromJSON(file="data/yauhau/yauhau-monad.json"))
f09 <- parse_raw(fromJSON(file="data/yauhau/muse-applicative.json"))

yauhau_baseline <-
    mutate(f16,Framework = 'Yauhau') %>%
    select(rounds_performed,levels,Framework) %>%
    full_join(
        mutate(f16,Framework = 'Seq.') %>%
        select(read_requests,levels,Framework) %>%
        rename(rounds_performed = read_requests) 
    ) %>%
    full_join(
        mutate(f04,Framework = 'Haxl') %>%
        rename(rounds_performed = rounds_made) %>%
    select(rounds_performed,levels,Framework) 
    ) %>%
    full_join(
        mutate(f09,Framework = 'Muse')  %>%
    select(rounds_performed,levels,Framework) 
    ) 

greens <- brewer.pal(8,"Greens")[c(2,5,8)]
blues <- brewer.pal(8,"Blues")[c(2,5,8)]
mixed <- c(greens[2:3],blues[2:3])

tikz("generated/yauhau_functions.tex",standAlone=F,width=8,height=4)
    print(
mutate(yauhau_functions, group=paste(`if_percentage`,Framework)) %>%
        ggplot(mapping = aes(x=if_percentage, y=rounds_performed, fill=Framework,group=group)) + 
            geom_boxplot(position='identity',alpha=0.7) + 
            scale_fill_manual(values=greens) + 
            labs(x="prob. of function/algorithm calls", y="No. of I/O calls") + 
            theme(text = element_text(size = 20)) + 
            guides(fill = guide_legend(title = element_blank()))  
)
dev.off()

tikz("generated/yauhau_concurrency.tex",standAlone=F,width=8,height=4)
print(
    mutate(yauhau_concurrency, group=paste(levels,Framework)) %>%
        ggplot(mapping = aes(x=levels, y=time, fill=Framework,group=group)) + 
            geom_boxplot(position='identity',alpha=0.7) + 
            scale_fill_manual(values=blues) + 
            labs(x="No. of levels", y="Service latency [ms]") + 
            theme(text = element_text(size = 20)) + 
            guides(fill = guide_legend(title = element_blank()))  
)
dev.off()


tikz("generated/yauhau_baseline.tex",standAlone=F,width=8,height=4)
    print(
    mutate(yauhau_baseline, group=paste(levels,Framework)) %>%
        ggplot() + 
            #geom_smooth(mapping = aes(x=levels, y=rounds_performed,color=Framework),method='lm') +
            geom_boxplot(mapping = aes(x=levels, y=rounds_performed, fill=Framework,group=group),position='identity',alpha=0.7) + 
            scale_fill_manual(values=mixed) + 
            scale_color_manual(values=mixed) + 
            labs(x="No. of levels", y="No. of I/O calls") + 
            theme(text = element_text(size = 20)) + 
            guides(fill = guide_legend(title = element_blank()))  
    )
dev.off()
