import keras
import numpy as np
from w_object_class import w_object

class ensemble_w_object:
    def __init__(self, pho_ensemble):
        self.w_dict = {key: np.eye(1, len(pho_ensemble.ensemble[0].pho_w_dict.keys()), k = idx) 
                       for idx, key in enumerate(pho_ensemble.ensemble[0].pho_w_dict.keys())}
        self.w = np.eye(len(pho_ensemble.ensemble[0].pho_w_dict.keys()))