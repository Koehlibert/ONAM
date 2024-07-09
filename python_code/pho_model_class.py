import orthogonalize
import numpy as np
import sympy
from copy import deepcopy
from model_container_class import model_container
from u_object_class import u_object
from w_object_class import w_object
from data_container_class import data_container
class pho_model:
    def __init__(self, model_info):
        inputs_dict = orthogonalize.create_inputs(model_info)
        self.model_info = model_info
        self.submodels = model_container(model_info, inputs_dict)
        self.whole_model = orthogonalize.compile_model(inputs_dict, self.submodels)
        
    def fit(self, x, y, epochs, **kwargs):
        if "callbacks" in kwargs: 
            callbacks = kwargs.get("callbacks")
        else:
            callbacks = None
        self.whole_model.fit(x, y, epochs = epochs, callbacks = callbacks)
        
    def pho(self, data_container):
        tmp_u_object = u_object(self, data_container)
        self.w_object = w_object(self, tmp_u_object)
        new_w_dict = deepcopy(self.w_object.w_dict)
        model_orders = {key: model.order
                         for key, model in self.submodels.model_dict.items()} | {"intercept": 1}
        unique_sorted_orders = -np.sort(-np.unique(list(model_orders.values())))
        for o_idx in unique_sorted_orders[1:]:
            l_o_keys = [key for key in model_orders.keys() if model_orders[key] <= o_idx]
            h_o_keys = [key for key in model_orders.keys() if model_orders[key] > o_idx]
            rel_indices = np.concatenate([tmp_u_object.u_indices[key] for key in l_o_keys])
            tmp_u = np.copy(tmp_u_object.u)
            mask = np.ones(tmp_u.shape[1], dtype = bool)
            mask.put(rel_indices, 0)
            tmp_u[:, mask] = 0
            tmp_u[:, -1] = 1
            h = np.matmul(tmp_u.transpose(), tmp_u)
            outputs = {h_key: np.matmul(tmp_u_object.u, w.reshape((-1,1))) 
                       for h_key in h_o_keys for w in [new_w_dict[h_key]]}
            z = {h_key: np.linalg.lstsq(h, np.matmul(tmp_u.transpose(), output))[0].flatten()
                 for h_key, output in outputs.items()}
            for h_key in h_o_keys:
                new_w_dict[h_key] -= z[h_key]
            for l_key in l_o_keys:
                w_update = np.zeros(tmp_u.shape[1])
                for h_key in h_o_keys:
                    w_update += z[h_key]
                new_w_dict[l_key][tmp_u_object.u_indices[l_key]] += w_update[tmp_u_object.u_indices[l_key]]
        all_o_means = {key : np.mean(np.matmul(tmp_u_object.u, w)) for key, w in new_w_dict.items()}
        for w_key, w in new_w_dict.items():
            if(w_key != "intercept"):
                w[-1] -= all_o_means[w_key]
            else:
                w[-1] += np.sum(value for key, value in all_o_means.items() if key != "intercept")
        self.pho_w_dict = new_w_dict
        pho_w_matrix = np.concatenate(list(new_w_dict.values()), axis = 0).reshape(tmp_u_object.u.shape[1], len(self.w_object.w_dict.values()), order = "F")
        self.pho_w_big = np.sum(pho_w_matrix, 1)
    
    def predict_pho(self, data):
        if(type(data) != data_container):
            data = data_container(data, self.model_info)
        tmp_u_object = u_object(self, data)
        return(np.matmul(tmp_u_object.u, self.pho_w_big))
    
    def predict_separately(self, data):
        if(type(data) != data_container):
            data = data_container(data, self.model_info)
        tmp_u_object = u_object(self, data)
        separate_preds = {effect: np.matmul(tmp_u_object.u, w_effect) for effect, w_effect in self.pho_w_dict.items()}
        return(separate_preds)
    # def predict_effect(self, data, effect):
        