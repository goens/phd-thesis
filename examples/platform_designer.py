pd = PlatformDesigner(self)
pd.setSchedulingPolicy('FIFO', 1000)
pd.newElement("Odroid-XU4")
# cluster 0 with l2 cache
pd.addPeClusterForProcessor("cluster_a7", processor_0,
                            num_little)
# Add L1/L2 caches
pd.addCacheForPEs("cluster_a7", 1, 0, 8.0, float('inf'),
                  frequencyDomain=1400000000.0, name='L1_A7')
pd.addCommunicationResource("L2_A7", ["cluster_a7"], 250, 250,
                            float('inf'), float('inf'),
                            frequencyDomain=1400000000.0)
# cluster 1, with l2 cache
pd.addPeClusterForProcessor("cluster_a15", processor_1, num_big)
# Add L1/L2 caches
pd.addCacheForPEs("cluster_a15", 1, 4, 8.0, 8.0,
                  frequencyDomain=2000000000.0, name='L1_A15')
pd.addCommunicationResource("L2_A15", ["cluster_a15"], 250, 250,
                            float('inf'), float('inf'),
                            frequencyDomain=2000000000.0)
# RAM connecting all clusters
pd.addCommunicationResource("DRAM", ["cluster_a7", "cluster_a15"],
                            120, 120, 8.0, 8.0,
                            frequencyDomain=933000000.0)
pd.finishElement()
