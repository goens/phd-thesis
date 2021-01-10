pykpn generate_mapping \
      kpn=tgff_reader  \
      trace=tgff_reader \
      platform=exynos,mppa_coolidge \
      tgff={file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_2},{file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_3},{file:consumer-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:consumer-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_2},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_3},{file:office-automation-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_2},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_3},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_4},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_5},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_6},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_7},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_8}\
      mapper=static_cfs,gbm \
      log_level=ERROR \
      -m
pykpn parse_multirun parsers=[best_mapping_time,estimated_total_time] output_filename=heuristics output_format=csv

pykpn generate_mapping \
      kpn=tgff_reader  \
      trace=tgff_reader \
      platform=exynos,mppa_coolidge \
      tgff={file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_2},{file:auto-indust-mocsyn.tgff,graph_name:TASK_GRAPH_3},{file:consumer-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:consumer-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_2},{file:networking-mocsyn.tgff,graph_name:TASK_GRAPH_3},{file:office-automation-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_0},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_1},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_2},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_3},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_4},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_5},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_6},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_7},{file:telecom-mocsyn.tgff,graph_name:TASK_GRAPH_8}\
      mapper=genetic,tabu_search,simulated_annealing,random_walk \
      mapper.random_seed=`seq -s, 1 10` \
      log_level=ERROR \
      -m
pykpn parse_multirun parsers=[best_mapping_time,estimated_total_time] output_filename=metaheuristics output_format=csv
#rm -fr multirun
