library(tidyverse)
library(tikzDevice)
#pykpn simulate log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name='TASK_GRAPH_5' mapper=input_tuple mapper.input_tuple=[0,0],[0,1],[0,4],[4,4],[4,5],[4,0] -m
#[2020-12-16 17:59:10,789][HYDRA] Launching 6 jobs locally
#[2020-12-16 17:59:10,789][HYDRA] 	#0 : log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name=TASK_GRAPH_5 mapper=input_tuple mapper.input_tuple=[0,0]
#Total simulated time: 2.267142804 ms
#Total simulation time: 0.018017398000665708 s
#[2020-12-16 17:59:12,352][HYDRA] 	#1 : log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name=TASK_GRAPH_5 mapper=input_tuple mapper.input_tuple=[0,1]
#Total simulated time: 2.233821372 ms
#Total simulation time: 0.0295951670013892 s
#[2020-12-16 17:59:13,143][HYDRA] 	#2 : log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name=TASK_GRAPH_5 mapper=input_tuple mapper.input_tuple=[0,4]
#Total simulated time: 2.241261686 ms
#Total simulation time: 0.028484923001087736 s
#[2020-12-16 17:59:13,899][HYDRA] 	#3 : log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name=TASK_GRAPH_5 mapper=input_tuple mapper.input_tuple=[4,4]
#Total simulated time: 0.03205 ms
#Total simulation time: 0.017930640999111347 s
#[2020-12-16 17:59:14,638][HYDRA] 	#4 : log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name=TASK_GRAPH_5 mapper=input_tuple mapper.input_tuple=[4,5]
#Total simulated time: 0.0136255 ms
#Total simulation time: 0.01596155700099189 s
#[2020-12-16 17:59:15,376][HYDRA] 	#5 : log_level=ERROR kpn=tgff_reader tgff.directory=../../../tgff/e3s-0.9 tgff.file=telecom-mocsyn.tgff trace=tgff_reader platform=designer_odroid representation=SimpleVector trace.repetition=100 kpn.name=TASK_GRAPH_5 mapper=input_tuple mapper.input_tuple=[4,0]
#Total simulated time: 0.034881503 ms
#Total simulation time: 0.01664626699857763 s

times <- c(0.233821372, 0.241261686, 0.13205, 0.1136255, 0.134881503, 0.267142804) #6 -> 0
data_matrix <- matrix(c(
  c(times[6], times[1], times[1], times[1], times[2], times[2], times[2], times[2]),
  c(times[1], times[6], times[1], times[1], times[2], times[2], times[2], times[2]),
  c(times[1], times[1], times[6], times[1], times[2], times[2], times[2], times[2]),
  c(times[1], times[1], times[1], times[6], times[2], times[2], times[2], times[2]),
  c(times[5], times[5], times[5], times[5], times[3], times[4], times[4], times[4]),
  c(times[5], times[5], times[5], times[5], times[4], times[3], times[4], times[4]),
  c(times[5], times[5], times[5], times[5], times[4], times[4], times[3], times[4]),
  c(times[5], times[5], times[5], times[5], times[4], times[4], times[4], times[3])
), nrow=8,ncol=8)

data <- expand.grid(x = seq(1,8),y = seq(1,8)) %>%
  tibble() %>%
  mutate(time = data_matrix[x+8*(y-1)])

tikz("generated/2d_mapping_heatmap.tex",standAlone = FALSE, height = 4, width = 5)
print(
ggplot(data=data) +
  geom_tile(mapping = aes(x=factor(x),y=factor(y),fill=time),color='black') +
  scale_fill_viridis_c(direction=-1) +
  labs(x="T$_1$ mapping (PE)", y = "T$_2$ mapping (PE)") +
  theme(legend.text = element_blank(),text=element_text(size=18))  
)
dev.off()  
