# -*- coding: utf-8 -*-
"""
GP-UCB model
"""
from functools import lru_cache
import random
import math
import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import WhiteKernel, Kernel
from banditsl10 import animallist, d_matrix, st_freqs


def Learnsolver(bandit, size, nr_trials, max_r, min_r, l_fit, beta, tau, localize=False):
    """
    agent solves a given bandit
    while learning using the GP-UCB model

    Parameters
    bandit :    The bandit, which is a list with the rewards.
    size :      size of the bandit.
    nr_trials : size of the search horizon, number of tiles that can be chosen.
    twoD:       True for bivariate bandits, False for univariate
    beta :      strenght of directed exploration
                0 for PureExploit.
    tau :       strenght of random exploration, temperature.
    

    Returns
    -------
    total_r: float, average accumulated reward at the end of the round.

    """
    total_r = 0
    hidden = np.array([True]*size)  
    observed_bandit = [[] for i in range(size)] #a list with for every cell the observed history
        
    '''
    step 1: one random cells is revealed at the begin of the experiment
    '''
    tile_number = random.randint(0, (size-1))
    tile = animallist[tile_number]
    reward = random.normalvariate(bandit[tile_number], 1)
	    
    #save that this cell has been opened before and save the history in observed_bandit
    hidden[tile_number] = False #save that this cell has been opened
    observed_bandit[tile_number].append(reward)
    

    for trial_nr in range(0, nr_trials):
        '''
        per choice to make, 
        the first step is to learn from the prior rewards
        and make predictions about the cells of the grid
        this is done via Gaussian Process regression
        '''
        m, s = GP(observed_bandit, size, l_fit, hidden, d_matrix = d_matrix)
        
        '''
        to translate the expectations to a probability to select a cell,
        we use UCB and a softmax rule
        '''
        UCB = [m[i] + beta * s[i] for i in range(0, size)]
        '''
        if localize:
            prior_choice = [(tile_number-(tile_number%W-1))//W, tile_number%W]
            UCB = localizer(UCB, prior_choice, W, L)
        '''
        P = softmax(UCB, tau)
            
        #the agent will choose from the tiles, for which the probabilites are given by P
        tile_number = random.choices(np.arange(0, size), weights=P)[0]
        reward = random.normalvariate(bandit[tile_number], 1)
        total_r += reward
         
        #save that this cell has been opened before and save the history in observed_bandit
        hidden[tile_number] = False #save that this cell has been opened
        observed_bandit[tile_number].append(reward)
    
    return total_r


def GP(observed_bandit, size, l_fit, hidden, noise=True, epsilon = 0.0001, d_matrix = d_matrix):
    '''
    This function starts from the observations,
    for which the coordinates are saved in xlist
    and the observed rewards in rewardlist
    since the GP assumes that not observed states have as default a value of 0,
    we rescale the reward to have a mean 0 and vary between max bounds of -0.5, 0.5
    
    Parameters
    ----------
    observed_bandit : the observations
    size : lenght of the bandit.
    l_fit: the generalization strength with which the participant smooths out the observed rewards
    hidden : list of which cells are hidden. True if hidden, False if the reward is known.
    noise: True if the participant assumes noise in their observations
    epsilon: the assumed noise, 
            measured as the variance of the reward around the mean
            std is 1 up 100, 
            var is 0.01**2 = 0.0001
    
    Returns
    -------
    Returns the mean function m(x) and the uncertainty function s(x) per tile.
    '''
    
    xlist = [] #list with the coordinates of the data points (list of doubles)
    rewardlist = []
    for i in range(0, len(hidden)):
        if hidden[i] == False: #then we have an observation for this cell
            for observation in observed_bandit[i]:
                    xlist.append([i])
                    rewardlist.append(observation)
                    
    cells =[[i] for i in range(0, size)]    
    
    kernel = semantic_kernel(k_matrix(l_fit, tuple(map(tuple, d_matrix))))
    if noise:
        kernel += WhiteKernel(epsilon, "fixed")
        
    gp = GaussianProcessRegressor(kernel=kernel)
    gp.fit(xlist, [(reward-40)/90 for reward in rewardlist])
    mlist, sigmalist = gp.predict(cells, return_std=True)
             
    return mlist, sigmalist

@lru_cache(maxsize=1)
def k_matrix(l, d_matrix_tuple):
    """
    Function to generate a correlation matrix
    Needed to generate mulitvariate normal correlated rewards

    **WARNING!** This function uses an lru_cache based on the provided 
    parameter l and can return a reference to the same numpy array, 
    so be careful to never modify the returned array as it will affect 
    future k_matrix return values when called with the same l. 
    
    Parameters
    ----------
    animallist :    list of strings
                    all animal options.
    d_matrix:        2D array, with pairwaise euclidian distance
    l : float
        the strength of the spatial correlations (given by the kRBF)
        between the rewards.

    Returns
    -------
    2D numpy array with correlations between pairwise animals
    one correlation for every animal-animal combination

    """
    
    d = np.array(d_matrix_tuple)
    d.flags.writeable = False
    K = kRBF(d, l)
    K.flags.writeable = False
    return K


def kRBF(d, l):
    """
    the Radial Basis Function kernel
    this kernel translates the spatial distances
    to correlations with smoothness lam
    Parameters
    ----------
    v1 : 2D numpy array of ints
        the coordinates of one cell.
    v2 : 2D numpy array of ints
        the coordinates of one cell.
    l : float
        the strength of the spatial correlations (given by the kRBF)
        between the rewards.

    Returns
    -------
    the correlation between two cells,
    based on their euclidian distance
    and the smoothness

    """
    
    return np.exp(-d**2/(2*l**2))

class semantic_kernel(Kernel):
    """
    create a class to replace the RBF class used in euclidian space
    this class needs a length scale declaration in init,
    calleable to evaluate the kernel
    diag to returns the diagnoal more efficiently
    if it is stationary; yes because we don't want the length scale to be optimized
    """
    def __init__(self, cov_matrix):
        #assign the d_matrix as your covariance matrix
        #for this, you just need to give the d_matrix as argument to this class
        self.cov_matrix = np.asarray(cov_matrix) #make sure it is stored as a Numpy array
        
    def __call__(self, X, Y=None, eval_gradient=False):
        #to compute kernel between two inputs X and Y
        #first, some safety steps to get the inputs in the correct format
        X = np.asarray(X, dtype=int).ravel()
        if Y is None:
            Y = X
        else:
            Y = np.asarray(Y, dtype=int).ravel()
        
        #the covariance value, K is returned between the two points
        #by finding the correct 2D index in K
        K = self.cov_matrix[np.ix_(X, Y)] 
        
        return K


    def diag(self, X):
        #faster way to get a diagonal element
        X = np.asarray(X, dtype=int).ravel()
        return self.cov_matrix[X, X]
    
    def is_stationary(self):
        #kernel is stationary, since it only depends on
        #distance between inputs, not on the values of the observations
        return True

def softmax(UCB, tau):
    """
    translates the UCB (the inflated expectation) to probabilities to sample tiles

    Parameters
    ----------
    UCB : the inflated expectations per tile.
    tau : softmax temperature, the level of undirected (random) exploration.

    Returns
    -------
    the probability to sample a tile.

    """
    
    UCB = [ucb - max(UCB) for ucb in UCB] #rescaling to avoid overflows
    exp_list = np.array([np.exp(ucb/tau) for ucb in UCB])
    Z = sum(exp_list)
    returnlijst = [value/Z for value in exp_list]
    return returnlijst


def localizer(UCB, prior_choice, d_matrix, animallist):
    i_prior_choice = animallist.index(prior_choice)
    IMD = [1/d_matrix[i_prior_choice][j] if d_matrix[i_prior_choice][j] >= 1 else 1 for j in range(len(animallist))]
    UCBloc = [IMD[i]*UCB[i] for i in range(len(animallist))]
    
    return UCBloc

def freqbiaser(phi, UCB, st_freqs):
    return [UCB[i] + phi*st_freqs[i] for i in range(len(UCB))]
    