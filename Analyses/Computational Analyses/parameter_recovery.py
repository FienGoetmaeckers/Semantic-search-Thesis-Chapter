# -*- coding: utf-8 -*-
"""
Created on Tue Jul 15 15:20:45 2025

@author: fgoetmae
"""

# -*- coding: utf-8 -*-
"""
parameter recovery code
"""
from csv import DictWriter
import pandas as pd
import math
import numpy as np
import random
from parameter_estimation1 import estimate_1env
from solving_models import GP, softmax, localizer, freqbiaser
from banditsl10 import bandits, animallist, d_matrix, st_freqs

#we use a uniform distribution to generate data from
bounds_beta = (0.0, 1.0)
bounds_tau = (0.01, 0.2)
bounds_lfit = (0.01, 20)
bounds_phi = (0, 1.0)

#experiment settings to simulate
nr_trials = 21
nr_blocks = 10
l = 10
size = 457
localize = True
freqbias = True

"""
step 1: generate data with given parameters
to do this, we follow exactly the same steps 
as in the Learnsolver function of the solving_models script
but simultaneously, we write out data in the format of behavioural data
"""
#we need to make a dataframe similar to the behavioural data
data = pd.DataFrame(columns=["block_nr", "trial_nr", "initial_opened", "selected_choice", "reward", "average_reward"])
    
#sample model parameters for the agent
beta = random.uniform(bounds_beta[0], bounds_beta[1])
tau = random.uniform(bounds_tau[0], bounds_tau[1])
l_fit = random.uniform(bounds_lfit[0], bounds_lfit[1])
phi = random.uniform(bounds_phi[0], bounds_phi[1])

for block_nr in range(0, nr_blocks):
    bandit = bandits[block_nr]  
    hidden = np.array([True]*size) 
    observed_bandit = [[] for i in range(size)]
        
    '''
    step 1: one random cells is revealed at the begin of the experiment
    '''
    tile_number = random.randint(0, size-1)
    tile = animallist[tile_number]
    reward = random.normalvariate(bandit[tile_number], 1)
	    
    #save that this cell has been opened before and save the history in observed_bandit
    hidden[tile_number] = False #save that this cell has been opened
    observed_bandit[tile_number].append(reward)
    
    #save for dataframe
    initial_opened = tile
    initial_opened_number = tile_number
    rewardlist = [reward] #to calculate the average accumulated reward per trial
       
        
    for trial_nr in range(0, nr_trials):
        '''
        per choice to make, 
        the first step is to learn from the prior rewards
        and make predictions about the cells of the grid
        this is done via Gaussian Process regression
        '''
        m, s = GP(observed_bandit, size, l_fit, hidden)
         
        '''
        to translate the expectations to a probability to select a cell,
        we use UCB and a softmax rule
        '''
        UCB = [m[i] + beta * s[i] for i in range(0, size)]
        if localize:
            prior_choice = tile
            UCB = localizer(UCB, prior_choice, d_matrix, animallist)
        if freqbias:
            UCB = freqbiaser(phi, UCB, st_freqs)
        P = softmax(UCB, tau)
        
        #the agent will choose from the tiles, for which the probabilites are given by P 
        tile_number = random.choices(np.arange(0, size), weights=P)[0]
        tile = animallist[tile_number]
        reward = random.normalvariate(bandit[tile_number], 1)
         
        #save that this cell has been opened before and save the history in observed_bandit
        hidden[tile_number] = False #save that this cell has been opened
        observed_bandit[tile_number].append(reward)
        
        rewardlist.append(reward)
           
        result_trial = pd.DataFrame({"block_nr": block_nr, "trial_nr": trial_nr, "initial_opened": initial_opened_number, "initial_opened_value": initial_opened, "selected_choice": tile_number, "selected_choice_value": tile, "reward": reward, "average_reward": np.mean(rewardlist)},index=[0]) 
        data = pd.concat([data, result_trial], ignore_index=True)
 
print("done with simulating data")    
"""
step 2: parameter estimation
"""

est = estimate_1env(size, nr_trials, nr_blocks, data, localize, freqbias, animallist, d_matrix, st_freqs)
       
"""
step 3: save generating and estimated parameter combo
"""
results = {"l_fit_g": l_fit, "l_fit_e": est[0][0], "beta_g": beta, "beta_e": est[0][1], "tau_g": tau, "tau_e": est[0][2], "phi_g": phi, "phi_e": est[0][3], "optimal NLL": est[1]}

file_name = 'estimated_parameters_{}_{}_l{}phibl.csv'.format(nr_trials, nr_blocks, l)
field_names=["l_fit_g", "l_fit_e", "beta_g", "beta_e", "tau_g", "tau_e", "phi_g", "phi_e", "optimal NLL"]
#open CSV file in append mode
with open(file_name, 'a') as f_object:
    dictwriter_object = DictWriter(f_object, fieldnames=field_names)
    dictwriter_object.writerow(results)
    f_object.close()
              