import pandas as pd
import random
import argparse
import scipy.stats
import numpy as np

def run_algo(graph, k, eps,seed):
	km1 = 0
	cut = 0
	time = 2
	imbalance = 0
	return km1, cut, time, imbalance

def sample_until_timelimit(df, time_limit):
	rows = []
	time_sum = 0
	fails = 0
	while fails < 20:
		x = df.sample(n=1).iloc[0] # sample one row which creates new dataframe, then take the first row
		if x.totalPartitionTime + time_sum <= time_limit:
			rows.append(x)
		else:
			fails += 1
	return rows

def run_instances(rows):
	time_ratios = []
	mismatch_instances = []
	for i, r in enumerate(rows):
		graph,k,eps,seed = r.graph,r.k,r.epsilon,r.seed
		print(i,"/",len(rows),sep='', end=' ')
		print("running", graph, k, eps, seed)
		km1, cut, time, imbalance = run_algo(graph, k, eps, seed)
		time_ratios.append(r.totalPartitionTime / time)
		if km1 != r.km1 or cut != r.cut or imbalance != r.imbalance:
			print("mismatch on", graph, k, eps, seed)
			mismatch_instances.append((graph,k,eps,seed))
	if len(mismatch_instances) == 0:
		print("All data reproduced! Yay.")
	else:
		print("There were", len(mismatch_instances), "mismatches.")
		print(mismatch_instances)

	time_ratios = sorted(time_ratios)
	print("gmean time ratio", scipy.stats.gmean(time_ratios))
	for q in [0.0, 0.05, 0.25, 0.5, 0.75, 0.95, 1.0]:
		print("quantile", q, "time ratio:", np.quantile(time_ratios, q))

if __name__ == '__main__':
	parser = argparse.ArgumentParser()
	parser.add_argument("--csv", type=str, default="km1/km1_kKaHyPar.csv", help="Path to data csv. default km1/km1_kKaHyPar.csv")
	parser.add_argument("--time", type=int, default=7200, help="time to run in seconds. default 2 hours")
	
	args = parser.parse_args()
	df = pd.read_csv(args.csv)

	rows = sample_until_timelimit(df, args.time)
	run_instances(rows)

