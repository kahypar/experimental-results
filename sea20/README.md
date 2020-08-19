# Hypergraph Partitioning Experimental Results for our SEA 20 paper (https://drops.dagstuhl.de/opus/volltexte/2020/12085/pdf/LIPIcs-SEA-2020-11.pdf)

Only connectivity metric

KaHyPar is only used in direct k-way mode.

Timelimit: 8 hours

Machine used:
Old BWUniCluster
Intel Xeon E5-2670, Sandy Bridge, 2.6 GHz, 2x8 cores, 64 GB RAM, 20MB L3 Cache, 8x256KB L2 Cache.
All runs are 1 core out of the 16 cores of a node.
g++ 9.2	

Disclaimers:

1) A few runs are missing due to some weird filesystem issues on the cluster, but we should have at least 8 out of the 10 seeds completed for each instance, and it's only instances that timed out anyways.

2) The column order isn't the same in every file. While pandas deals with that gracefully, your toolchain may not. 
