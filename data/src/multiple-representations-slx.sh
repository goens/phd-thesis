rm -fr multirun-heuristics-multiple-representations-slx
pykpn generate_mapping \
      kpn=audio_filter,hog,speaker_recognition  \
      trace=slx_default \
      platform=exynos,mppa_coolidge \
      platform.processor_0='ARM_CORTEX_A7' \
      platform.processor_1='ARM_CORTEX_A15' \
      mapper=static_cfs,gbm \
      hydra.sweep.dir=multirun-heuristics-multiple-representations-slx \
      log_level=ERROR \
      -m
pykpn parse_multirun parsers=[best_mapping_time,estimated_total_time] path=multirun-heuristics-multiple-representations-slx output_format=csv

rm -fr multirun-metaheuristics-multiple-representations-slx
pykpn generate_mapping \
      kpn=audio_filter,hog,speaker_recognition  \
      trace=slx_default \
      platform=exynos,mppa_coolidge \
      platform.processor_0='ARM_CORTEX_A7' \
      platform.processor_1='ARM_CORTEX_A15' \
      mapper=genetic,tabu_search,simulated_annealing,random_walk,gradient_descent \
      representation=SimpleVector,MetricSpaceEmbedding,Symmetries,SymmetryEmbedding \
      mapper.random_seed=`seq -s, 1 10` \
      hydra.sweep.dir=multirun-metaheuristics-multiple-representations-slx \
      log_level=ERROR \
      -m
pykpn parse_multirun parsers=[best_mapping_time,estimated_total_time] path=multirun-metaheuristics-multiple-representations-slx output_format=csv
#rm -fr multirun
