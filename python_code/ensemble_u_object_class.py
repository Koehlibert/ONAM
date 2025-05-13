import numpy as np
from u_object_class import u_object

class ensemble_u_object(u_object):
    def __init__(self, pho_ensemble, data = None):
        if data is None:
            data = pho_ensemble.fit_data

        member_predictions = [member.predict_separately(pho_ensemble.fit_data) 
                              for member in pho_ensemble.ensemble]
        self.u_dict = {key: np.array([member_prediction[key] 
                                      for member_prediction in member_predictions]).mean(axis = 0) 
                       for key in pho_ensemble.ensemble[0].pho_w_dict.keys()}
        self.u_indices = {key: idx for idx, key in enumerate(self.u_dict)}
        self.u = np.array([self.u_dict[key] for key in self.u_dict]).transpose()