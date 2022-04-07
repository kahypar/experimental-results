#!/usr/bin/env bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

rm -rf $SCRIPT_DIR/img/
mkdir $SCRIPT_DIR/img/

Rscript $SCRIPT_DIR/component_effectiveness_tests/reproducibility_plots.R $SCRIPT_DIR
Rscript $SCRIPT_DIR/km1/reproducibility_plots.R $SCRIPT_DIR
Rscript $SCRIPT_DIR/repeated_executions/reproducibility_plots.R $SCRIPT_DIR
Rscript $SCRIPT_DIR/edge_partitioning/reproducibility_plots.R $SCRIPT_DIR
Rscript $SCRIPT_DIR/graph_partitioning/reproducibility_plots.R $SCRIPT_DIR
