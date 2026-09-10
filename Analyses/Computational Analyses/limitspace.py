# -*- coding: utf-8 -*-
"""
create a bandit based on only those animals that have been answered in the given dataset
"""

import pandas as pd
from banditsl10 import animallist, d_matrix, st_freqs


def limit_search_space(responses):
    #find indexes of all responses
    index_responses = [animallist.index(response) for response in responses]
    
    #create subset of animallist with only those indices
    subanimallist = [animallist[index] for index in range(len(animallist)) if index in index_responses]
    
    #create submatrix for distances with those indices
    subd_matrix = [[d_matrix[index_1][index] for index in range(len(animallist)) if index in index_responses] for index_1 in range(len(animallist)) if index_1 in index_responses]
    
    subst_freqs = [st_freqs[index] for index in range(len(animallist)) if index in index_responses]
    
    return subanimallist, subd_matrix, subst_freqs