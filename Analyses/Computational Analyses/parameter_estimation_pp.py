"""
script to estimate the model parameters of one participant
"""
import sys
from csv import DictWriter
import pandas as pd
import numpy as np
from parameter_estimation import estimate_1env
from banditsl10 import animallist
from limitspace import limit_search_space



#specific variables for this data file
nr_blocks = 10#30 
nr_trials = 21
nr_participants = 50 #97#660 #98 #88
size = 457
#name of the file to read the data from
data_name = "Exp1data_second10r"#"data2" #"Exp1data_clean"
date =  "150726"
localize = True
freqbias = True
limit = "no" #"no" #"all" #"ind"

"""
read in the data
"""
data = pd.read_csv(data_name + '.csv', delimiter=',') #delimiter = ',')
#select one participant to estimate in this script
val = sys.argv[1:]
assert len(val) == 1
p_index = int(val[0])

participant = data.subjectID.unique()[p_index]
print("For participant {}".format(participant))
data_p = data.query('subjectID == "{}"'.format(participant))

"""
get the data ready for parameter estimation
"""
data_p = data_p.query('type == "sell"')
data_p = data_p[data_p["reward"].isna() == False]

data_p["initial_opened"] = [animallist.index(data_p["animal_opened"].values[i]) for i in range(len(data_p))]

#select the search space in which the model will search and generalize
if limit == "all":
    #get all unique responses given by all participants in this experiment
    responses = np.unique(data[data["reward"].isna() == False][["animal_opened", "animal"]].values)
elif limit == "ind":
    #get all unique responses given by this subject
    responses = np.unique(data_p[['animal_opened', 'animal']].values)
else:
    #we do not limit the search space
    responses = animallist

subanimallist, subd_matrix, subst_freqs = limit_search_space(responses)
size = len(subanimallist)
#change the indices of the choice options based on the new, smaller lits of animals
data_p["initial_opened"] = [subanimallist.index(data_p["animal_opened"].values[i]) for i in range(len(data_p))]
data_p["selected_choice"] = [subanimallist.index(data_p["animal"].values[i]) for i in range(len(data_p))]


"""
estimate the model parameters of this participant
"""

est = estimate_1env(size, nr_trials, nr_blocks, data_p, localize, freqbias, subanimallist, subd_matrix, subst_freqs)

"""
save the output
"""

results      = {"Participant": data_p["subjectID"].values[0], "l_fit": est[0][0], "beta": est[0][1], 
	     	     "tau": est[0][2], "phi": est[0][3], "NLL": est[1]}

#open CSV file in append mode
with open("est_" + date + ".csv", 'a') as f_object:
    field_names = ["Participant", "l_fit", "beta", "tau", "phi", "NLL"]
    dictwriter_object = DictWriter(f_object, fieldnames=field_names)
    dictwriter_object.writerow(results)
    f_object.close()


del data_p