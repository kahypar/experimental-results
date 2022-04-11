## Reproduce the Experiments

Reproduction is done in two separate steps due to time constraints.

1) Build plots from existing data. This only works on the login node because the other machines don't have R installed.

```
./generate_plots.sh
```

This should take about 2-3 minutes. The plots are located in the /img/ folder, with their names matching those in the paper.

2) Run code for random sample and check against existing data

First log on a free machine. The login node displays a table of free machines (user set as -).

```
ssh i10pc132
```

Then run the setup (downloads the instanced and code, and compiles it), and subsequently run a subset of the instances.

```
exclusive ./setup.sh
exclusive python3 reproduce.py 
```

The exclusive command tells other users not to use the same machine.
Downloading the instances can take some time.
The reproduce script takes a parameter --time in seconds, which is the amount of time the sampled runs took, which is then an indicator for how long your run is going to take.
The default is 2 hours.

Unfortunately, even with correct compiler versions we couldn't exactly reproduce all instances.
Therefore the reproduce script prints a list of all mismatches, and for a some small deltas, how many instances were off by that delta.
