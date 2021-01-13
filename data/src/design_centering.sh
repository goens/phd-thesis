#!/bin/bash
#  kpn                 platform           q1      q2      q3
#  <chr>               <chr>           <dbl>   <dbl>   <dbl>
#1 audio_filter        exynos           38.5    45.3    70.4
#2 audio_filter        mppa_coolidge  5930.   7909.   8305. 
#3 hog                 exynos          884.   1070.   1284. 
#4 hog                 mppa_coolidge 12153.  13343.  14925. 
#5 speaker_recognition exynos           34.9    40.0    60.3
#6 speaker_recognition mppa_coolidge 50227.  52987.  60014. 

declare -A THRESHOLDS
THRESHOLDS["exynosaudio_filter"]=38.5ms,45.3ms,70.4ms
THRESHOLDS["exynoshog"]=884ms,1070ms,1284ms
THRESHOLDS["exynosspeaker_recognition"]=34.9ms,40.0ms,60.3ms
THRESHOLDS["mppa_coolidgeaudio_filter"]=5930ms,7909ms,8305ms
THRESHOLDS["mppa_coolidgehog"]=12153ms,13343ms,14925ms
THRESHOLDS["mppa_coolidgespeaker_recognition"]=50227ms,52987ms,60014ms

PLATFORMS="mppa_coolidge" #exynos"
APPS="audio_filter hog speaker_recognition"
for app in $APPS; do
    for plat in $PLATFORMS; do
        OUT_DIR="multirun/dc-${plat}-${app}"
        rm -fr $OUT_DIR
        pykpn find_design_center \
                 kpn=$app  \
                 trace=slx_default \
                 platform=$plat \
                 platform.processor_0.type="ARM_CORTEX_A7" \
                 platform.processor_1.type="ARM_CORTEX_A15" \
                 platform.name="exynos" \
                 hydra.sweep.dir=$OUT_DIR \
                 random_seed=`seq -s, 1 10` \
                 threshold=${THRESHOLDS["${plat}${app}"]} \
                 design_centering.perturbation.perturbation_type=classic,representation \
                 representation=SimpleVector,MetricSpaceEmbedding \
                 log_level=ERROR \
                 -m
        pykpn parse_multirun parsers=[dc_json] path=$OUT_DIR output_format=csv
    done;
done;
