# experimental-results
Collection of our hypergraph partitioning experiments

The KaHyPar runs were run with a soft time limit of 8 hours, and a slightly longer hard time limit. In case the solution projection finishes barely past 8 hours, I still wanted to know the solution. Therefore it's necessary to additionally filter your dataframe for totalPartitionTime <= 28800 when generating plots.

CPU stats:
Intel Xeon Gold 6230, 2.1 GHz, 2x20 cores, 96 GB RAM, 27.5MB L3 Cache
All runs are single-threaded
