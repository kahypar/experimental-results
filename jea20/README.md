## Experimental Results of Publication "High-Quality Hypergraph Partitioning"

This repository contains the experimental results used in the publication *High-Quality Hypergraph Partitioning* (currently under review, arxiv link will be provided once available). In each folder, we provide the results of each tested partitioner for a specific experiment in a seperate csv file. The csv files contain the following data/columns:

- **algorithm**: name of the partitioner
- **graph**: name of the (hyper)graph
- **timeout**: *yes* if the partitioner exceeded the given the timelimit for the instance and *no* otherwise
- **seed**: random seed
- **k**: number of blocks
- **epsilon**: imbalance parameter
- **imbalance**: imbalance produced by the partitioner for the instance (instance is considered as imbalanced, if *imbalance* > *epsilon*)
- **totalPartitionTime**: time required to partition the instance
- **objective**: objective function used to partition the instance (either *km1* or *cut*)
- **km1**: connectivity metric
- **cut**: cut metric
- **failed**: *yes* if the partitioner exited with non-zero exit status and *no* otherwise

The benchmark sets are publicly available at https://doi.org/10.5445/IR/1000098881. Note that we did not use benchmark set D from this resource. Therefore, benchmark set E, F and G corresponds to the benchmark set D, E and F in our publication.

The csv files have the following naming convention: `<objective>_<partitioner-name>_<optional-suffix>.csv`. `<objective>` denotes the objective function used in the experiment, `<partitioner-name>` the name of the partitioner and `<optional-suffix>` is used to denote special algorithm configurations or referencing to a specific benchmark set.

In the following, we give a brief overview of the data contained in the different folders and to which experiment it corresponds to in our publication. For a detailed description of the experimental environment and methodology, we reference the reader to our publication. Note, that the folders also contain additionally data that are not presented in our publication (results for the cut metric and the recursive bipartitioning version of KaHyPar).

- **component_effectiveness_tests**: Evaluates the effectivness of the different algorithmic components of KaHyPar (corresponds to Section 4.2). Suffix `-s` denotes KaHyPar without pin sparsification, `-s-cac` additionally disables the community-aware coarsening technique and `-s-cac-f` additionally disables our flow-based refinement.
- **km1**: Results for different hypergraph partitioners on our entire benchmark set (set A) optimizing the connectivity metric (corresponds to Section 4.3).
- **cut**: Results for different hypergraph partitioners on our entire benchmark set (set A) optimizing the cut metric.
- **repeated_executions**: Contains results for our evolutionary algorithm and for several partitioners that we run several times until they exceed a timelimit of eight hours for each instance (corresponds to Section 4.3.5).
- **edge_partitioning**: Results for different hypergraph partitioners for our case study *edge partitioning* (corresponds to Section 4.4)
- **graph_partitioning**: Results for different (hyper)graph partitioners for our case study *graph partitioning* (corresponds to Section 4.5). The experiments are executed on two different benchmark sets. The suffix `_websocial` denotes results for benchmark set E and `_dimacs` denotes the results for benchmark set F.