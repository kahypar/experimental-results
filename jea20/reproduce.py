import pandas as pd
import random
import argparse
import scipy.stats
import numpy as np

from threading import Timer
import signal
import shutil
import subprocess
import os

def run_algo(graph, k, eps,seed):
	config = "kahypar/config/km1_kKaHyPar_sea20.ini"
	benchmark_dir = "benchmark_set/"
	kahypar_k = "kahypar/release/kahypar/application/KaHyPar"
	kahypar_k_proc = subprocess.Popen([kahypar_k,
						"-h" + benchmark_dir + graph,
						"-k" + str(k),
						"-e" + str(eps),
						"--seed=" + str(seed),
						"-okm1" ,
						"-mdirect",
						"-p" + config,
						"--sp-process=true"],
	stdout=subprocess.PIPE, universal_newlines=True, preexec_fn=os.setsid)
	out, err = kahypar_k_proc.communicate()

	time = 2147483647
	cut = 2147483647
	km1 = 2147483647
	imbalance = 1.0

	if kahypar_k_proc.returncode == 0:
		# Extract metrics out of KaHyPar-CA output
		for line in out.split('\n'):
			s = str(line).strip()
			if "RESULT" in s:
				km1 = int(s.split(" km1=")[1].split(" ")[0])
				cut = int(s.split(" cut=")[1].split(" ")[0])
				time = float(s.split(" totalPartitionTime=")[1].split(" ")[0])
				imbalance = float(s.split(" imbalance=")[1].split(" ")[0])
	return km1, cut, time, imbalance

def sample_until_timelimit(df, time_limit):
	rows = []
	time_sum = 0
	fails = 0
	while fails < 200:
		x = df.sample(n=1).iloc[0] # sample one row which creates new dataframe, then take the first row
		if x.totalPartitionTime + time_sum <= time_limit and x.totalPartitionTime <= 200:	# accept as long as we stay below time limit, and dont take too long running ones
			rows.append(x)
			time_sum += x.totalPartitionTime
		else:
			fails += 1
	print(time_sum)
	return rows

def run_instances(rows):
	output = []
	for i, r in enumerate(rows):
		graph,k,eps,seed = r.graph,r.k,r.epsilon,r.seed
		print(i,"/",len(rows),sep='', end=' ')
		print("running", graph, k, eps, seed)
		km1, cut, time, imbalance = run_algo(graph, k, eps, seed)

		if km1 == 0:
			if r.km1 == 0:
				km1_ratio = 1
			else:
				km1_ratio = 0
		else:
			km1_ratio = r.km1 / km1

		output.append((graph,k,eps,seed,km1_ratio, r.totalPartitionTime/time))
		
	df = pd.DataFrame(data=output, columns=["graph","k","eps","seed","km1_ratio","time_ratio"])
	return df

def analyze(df):
	print("list of all mismatches")
	print(df[df.km1_ratio != 1.0])

	print("running time ratios: record / fresh")
	print(df["time_ratio"].describe(percentiles=[0.05, 0.25, 0.5, 0.75, 0.95]))
	
	for deviation in [0.03, 0.01, 0.001]:
		print("There were", len(df[(df.km1_ratio > 1+deviation) | (df.km1_ratio < 1-deviation)]), "km1 mismatches with more deviation than", deviation)

if __name__ == '__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument("--csv", type=str, default="km1/km1_kKaHyPar.csv", help="Path to data csv. default km1/km1_kKaHyPar.csv")
	parser.add_argument("--time", type=int, default=7200, help="time to run in seconds. default 2 hours")
	
	args = parser.parse_args()
	df = pd.read_csv(args.csv)

	rows = sample_until_timelimit(df, args.time)
	out_df = run_instances(rows)
	out_df.to_csv('out.csv', index=False)
	analyze(out_df)
