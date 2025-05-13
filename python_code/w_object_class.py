import keras
import numpy as np

class w_object:
    def __init__(self, pho_model, u_object):
        w_dict_sep = {model_key: pho_model.submodels.model_dict[model_key].get_w()
                      for model_key in pho_model.submodels.model_dict.keys()}
        w_dict_sep = {w_key: w_value[0].reshape((-1,1)) if len(w_value) == 1 else np.concatenate([w_value[0], w_value[1].reshape((-1,1))])
                      for w_key, w_value in w_dict_sep.items()}
        self.w = np.concatenate(list(w_dict_sep.values()), axis = 0)
        self.w = np.append(self.w, 0)
        self.w_dict = {sub_w_key: np.put((vec := np.zeros(self.w.shape[0])), u_object.u_indices[sub_w_key], sub_w_value) or vec 
                       for sub_w_key, sub_w_value in w_dict_sep.items()} | {"intercept": np.zeros(self.w.shape[0])}
        