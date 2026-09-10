# -*- coding: utf-8 -*-
"""
script to estimate the model parameters of one (behavioural/computational) participant
"""
import math
import numpy as np
from sklearn.gaussian_process import GaussianProcessRegressor
from sklearn.gaussian_process.kernels import RBF, WhiteKernel
import scipy
from scipy.optimize import minimize
from solving_models import semantic_kernel, k_matrix, localizer, freqbiaser
from banditsl10 import animallist, d_matrix, st_freqs


"""
Prior with which we want to estimate the model parameters:
"""
#prior values of model parameters, used to start the search and to bias the optimization
x0 = [10, 0.5, 0.01, 0.2]
#std of the priors on the model parameters, used to bias the optimization
xstd = [8, 0.2, 0.2, 0.2] #actually is this the variance, just wrong name but correct values and use
#covariance matrix of the priors on the model parameters
xcov = np.diag(xstd)

bounds = np.exp(np.array([(-5, 6), (-5, 3), (-5, 3), (-5,6)]))

def estimate_1env(size, nr_trials, nr_blocks, data, localize = False, freqbias = False, animallist = animallist, d_matrix = d_matrix, st_freqs = st_freqs):
    """
    estimates the model parameters of the data
    for an experiment with one environment
    using leave-two-out method where two rounds are left out
    for cross validation
    
    1 estimation is made, using the entire dataset
    one value of l, beta and tau
            
    then use the median of the estimations as the final result
    and the quartile deviation as a measurement of variance
    
    Parameters
    ----------
    nr_trials : int
        nr of trials per round.
    nr_blocks : int
        nr of blocks per experiment.
    data : dataframe
        all the participant's data.

    Returns
    -------
    the estimated parameters and the NLL belonging to that estimation.

    """
    
    #make usefull lists of the data
    datalast = data.query('trial_nr == {}'.format(nr_trials-1))  #this is a dataframe that only contains the data of the last trial of every grid
    #lists with info per grid
    initial_opened = [value for value in datalast.initial_opened]
    
    l_fit_est_list = [0]*int(nr_blocks)#/3)
    beta_est_list = [0]*int(nr_blocks)#/3)
    tau_est_list = [0]*int(nr_blocks)#/3)
    phi_est_list = [0]*int(nr_blocks)#/3)
    NLL_list = [0]*int(nr_blocks)#/3)
    
    for out_index in range(0, nr_blocks):#, 3):
        #data_partial = data.query('block_nr != {} and block_nr != {} and block_nr != {}'.format(out_index, out_index+1, out_index+2))
        data_partial = data.query('block_nr != {}'.format(out_index))

        args = (nr_trials, size, initial_opened[:out_index] + initial_opened[out_index+1:],#3:],
                [value for value in data_partial.selected_choice],
                [value for value in data_partial.reward],
                [value for value in data_partial.average_reward],
                localize, freqbias, animallist, d_matrix, st_freqs)
        
        #print(out_index)
        #(l_fit_est, beta_est, tau_est) = (2, 0.3, 0.02)
        res = minimize(fun=wrapper, x0 = np.log(x0), args=args, method='SLSQP', bounds=np.log(np.array(bounds)))
        (l_fit_est, beta_est, tau_est, phi_est) = np.exp(res.x)
        
        #l_fit_est_list[int(out_index/3)] = l_fit_est
        #beta_est_list[int(out_index/3)] = beta_est
        #tau_est_list[int(out_index/3)] = tau_est
        #phi_est_list[int(out_index/3)] = phi_est
        l_fit_est_list[int(out_index)] = l_fit_est
        beta_est_list[int(out_index)] = beta_est
        tau_est_list[int(out_index)] = tau_est
        phi_est_list[int(out_index)] = phi_est

        
        #cross validation
        #data_partial = data.query('block_nr == {} or block_nr == {} or block_nr == {}'.format(out_index, out_index+1, out_index+2))
        data_partial = data.query('block_nr == {}'.format(out_index))
        cross_val = NLL(nr_trials, size, l_fit_est, beta_est, tau_est, phi_est,
                        initial_opened[out_index],#:out_index+2], 
                        [value for value in data_partial.selected_choice],
                        [value for value in data_partial.reward], 
                        [value for value in data_partial.average_reward],
                        localize, freqbias, animallist, d_matrix, st_freqs)        
        #NLL_list[int(out_index/3)] = cross_val
        NLL_list[int(out_index)] = cross_val

                
    print("for the entire experiment we estimated a median of:")    
    print(r'l_fit = %.3f +- %.3f, beta = %.3f +- %.3f, tau = %.3f +- %.3f, phi = %.3f +- %.3f'
          % (np.median(l_fit_est_list), (np.percentile(l_fit_est_list, 75) - np.percentile(l_fit_est_list, 25))/2, 
             np.median(beta_est_list), (np.percentile(beta_est_list, 75) - np.percentile(beta_est_list, 25))/2, 
                  np.median(tau_est_list), (np.percentile(tau_est_list, 75) - np.percentile(tau_est_list, 25))/2,
              np.median(phi_est_list), (np.percentile(phi_est_list, 75) - np.percentile(phi_est_list, 25))/2 ))
    print("NLL = %.3f\n" % np.sum(NLL_list))
    print("\n")
    resx = (np.median(l_fit_est_list), np.median(beta_est_list), np.median(tau_est_list), np.median(phi_est_list))
    resfun = np.sum(NLL_list)
    
        
    return (resx, resfun)


def wrapper(par, *args):
    """
    this function is just a wrapper function that brings the NLL function into the correct format
    to be called by the optimization function
    
    used for the 1 l, 1 beta and 1 tau per data set
    
    Parameters
    ----------
    par : array of floats
        the array of the model parameters that need to be optimized
    *args : tuple of ...
        all the other arguments needed for NLL

    Returns
    -------
    returns the function NLL
    """
    l_fit = np.exp(par[0])
    if (l_fit == 0):
        l_fit = 10e-8
    
    beta = np.exp(par[1])
    if (beta == 0):
        beta = 10e-8
    
    tau = np.exp(par[2])
    if (tau == 0):
        tau = 10e-8    

    (nr_trials, size, initial_opened, selected_choice, reward, average_reward, localize, freqbias, animallist, d_matrix, st_freqs) = args
    
    if freqbias:
        phi = np.exp(par[-1])
        if (phi == 0):
            phi = 10e-8
    else: #set a default value for phi if we don't use the frequency bias
        phi = 10e-8
    
    '''
    The probability that the data is generated 
    by a model with the given model parameters
    is 
    prob = L * bias
    so 
    NLprob = NLL + NLbias = NLL - Lbias
    '''
    term1 = NLL(nr_trials, size, l_fit, beta, tau, phi, initial_opened, selected_choice, reward, average_reward, localize, freqbias, animallist, d_matrix, st_freqs)
    if freqbias:
        term2 = NLbias((l_fit, beta, tau, phi), x0, xcov)
    else:
        term2 = NLbias((l_fit, beta, tau), x0, xcov)
    NLprob = term1 + term2
    return NLprob


'''
functions calculating the probability of each choice given a parameter set
'''
def NLL(nr_trials, size, l_fit, beta, tau, phi, initial_opened, selected_choice, reward, average_reward, localize, freqbias, animallist, d_matrix, st_freqs):
    """
    NLL (par, observations)
    This function calculates the negative log likelihood of a set of observations (all the choices) given a parameter set

    Parameters
    ----------
    l_fit : float
        model parameter defining the generalization strenght
    beta : float
        model parameter defining the exploration bonus
    tau : float
        model parameter defining the softmax temperature
    
    initial_opened : list of ints
        per grid, the first opened cell
    selected_choice : list of ints
        all chosen cells
    reward : list of floats
        the observed rewards per chosen cell
    average_reward : list of floats
        the average reward per choice, needed to calculate the reward of the initially opened cell

    Returns
    -------
    returns a float, this will be given to the optimizer

    """
    LL = 0
    if type(initial_opened) == int:
        b = 1
    else:
        b = len(initial_opened)
    for round_nr in range(0, b):
        if type(initial_opened) == int:
            opened_cells = [initial_opened]
        else:
            opened_cells = [initial_opened[round_nr]]
        first_observation = 2*average_reward[round_nr*nr_trials] - reward[round_nr*nr_trials]
        observations = [first_observation]
        
        if type(initial_opened) == int:
            prior_choice = animallist[initial_opened]
        else:
            prior_choice = animallist[initial_opened[round_nr]]
        for trial_nr in range(0, nr_trials):
            choice = selected_choice[round_nr*nr_trials + trial_nr]
            LL += log_probability(size, choice, opened_cells, observations, l_fit, beta, tau, phi, localize, prior_choice, freqbias, animallist, d_matrix, st_freqs)
            
            #update the observations before moving on to the next 
            opened_cells.append(choice)
            observations.append(reward[round_nr*nr_trials + trial_nr])
            prior_choice = animallist[int(choice)]
    return -LL


'''
functions needed to calculate the NLL
'''
#more stable if we use the log_softmax function of scipy instead of calculing P our selves and then logging it
def log_probability(size, choice, opened_cells, observations, l_fit, beta, tau, phi, localize, prior_choice, freqbias, animallist, d_matrix, st_freqs):
    m, s = GP(observations, size, l_fit, opened_cells, d_matrix = d_matrix)
    UCB = [m[i] + beta * s[i] for i in range(0, size)]
    if localize:
        UCB = localizer(UCB, prior_choice, d_matrix, animallist)    
    if freqbias: 
        UCB = freqbiaser(phi, UCB, st_freqs)
    UCBtau = [value/tau for value in UCB]
    log_P = scipy.special.log_softmax(UCBtau)
    
    return log_P[int(choice)]
    

def GP(observations, size, l_fit, opened_cells, noise=True, epsilon = 0.0001, d_matrix = d_matrix):   
    '''
    This function starts from the observations
    since the GP assumes that not observed states have as default a value of 0,
    we rescale the reward to have a mean 0 and vary between max bounds of -0.5, 0.5
    
    Parameters
    ----------
    observations : the observations
    W : width of the bandit.
    L : lenght of the bandit.
    l_fit: the generalization strength with which the participant smooths out the observed rewards
    opened_cells : list of which cells are opened.
    noise: True if the participant assumes noise in their observations
    epsilon: the assumed noise, 
            measured as the variance of the reward around the mean
            std is 1 up 100, 
            var is 0.01**2 = 0.0001
             
    
    Returns
    -------
    Returns the mean function m(x) and the uncertainty function s(x) per tile.
    '''
    
    cells =[[i] for i in range(0, size)]    
    
    kernel = semantic_kernel(k_matrix(l_fit, tuple(map(tuple, d_matrix))))
    if noise:
        kernel += WhiteKernel(epsilon, "fixed")
        
    gp = GaussianProcessRegressor(kernel=kernel)
    gp.fit([[i] for i in opened_cells], [(reward-40)/90 for reward in observations]) 
    mlist, sigmalist = gp.predict(cells, return_std=True)
    
    return mlist, sigmalist



def NLbias(par, x0, xcov):
    """
    function to calculate the negative log of the bias term
    this term comes from a multivariate normal distribution
    (the prior of the model parameters)

    Parameters
    ----------
    par: (l_fit, beta, tau)
    x0 : mean of the prior
    xcov : covariance matrix of the prior

    Returns
    -------
    return the NL of the bias term

    """
    NLbias = 0
    for i in range(len(par)):
        NLbias += (par[i]-x0[i])**2/xcov[i][i]
   
    return NLbias/2
    #return 0 #temporary: use no extra bias!