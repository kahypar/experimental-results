git clone --recursive git@github.com:kahypar/kahypar.git
cd kahypar 
git checkout 876b7768a92614747e6427e596b89506349c7561
cd external_tools/WHFC
git checkout 1a787e9f7dfd29dca3037beb9959fcec8756bce0
cd ../../ && mkdir release && cd release
cmake .. -DCMAKE_BUILD_TYPE=Release
make KaHyPar
cd ../../
curl -o benchmark_set.tar https://algo2.iti.kit.edu/schlag/sea2017/benchmark_set.tar
tar -xf benchmark_set.tar
