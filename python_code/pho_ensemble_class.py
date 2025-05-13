from model_info_object_class import model_info_object
from pho_model_class import pho_model
from data_container_class import data_container
from ensemble_u_object_class import ensemble_u_object
from ensemble_w_object_class import ensemble_w_object
from copy import deepcopy
import numpy as np
class pho_ensemble:
    def __init__(self, model_formula, dict_of_deep_models):
        self.model_formula = model_formula
        self.dict_of_deep_models = dict_of_deep_models
        self.model_info = model_info_object(self.model_formula, 
                                            self.dict_of_deep_models)
        self.ensemble_built = False
    
    def create_models(self, n_ensemble = 20):
        self.ensemble = [pho_model(self.model_info) 
                         for i in range(n_ensemble)]
        self.n_ensemble = n_ensemble
        self.ensemble_built = True
    
    def fit(self, data, epochs = 500, n_ensemble = 20, **kwargs):
        if(not self.ensemble_built):
            self.create_models(n_ensemble)
            self.ensemble_built = True
        self.fit_data = data_container(data, self.model_info)
        self.Y = data.loc[:, self.model_info.outcome]
        for ensemble_member in self.ensemble:
            history = ensemble_member.fit(x = self.fit_data.model_data, 
                                          y = self.Y, 
                                          epochs = epochs, **kwargs)
            ensemble_member.pho(self.fit_data)
        self.final_pho()
        
    def final_pho(self):
        u_ensemble_object = ensemble_u_object(self)
        model_orders = {key: model.order 
                        for key, model in self.ensemble[0].submodels.model_dict.items()} | {"intercept": 1}
        n_models = len(model_orders)
        w_ensemble_object = ensemble_w_object(self)
        self.final_pho_w_dict = deepcopy(w_ensemble_object.w_dict)
        unique_sorted_orders = -np.sort(-np.unique(list(model_orders.values())))
        for o_idx in unique_sorted_orders[1:]:
            l_o_keys = [key for key in model_orders.keys() if model_orders[key] <= o_idx]
            h_o_keys = [key for key in model_orders.keys() if model_orders[key] > o_idx]
            rel_indices = np.array([u_ensemble_object.u_indices[key] for key in l_o_keys])
            tmp_u = np.copy(u_ensemble_object.u)
            mask = np.ones(tmp_u.shape[1], dtype = bool)
            mask.put(rel_indices, 0)
            tmp_u[:, mask] = 0
            tmp_u[:, -1] = 1
            h = np.matmul(tmp_u.transpose(), tmp_u)
            outputs = {h_key: np.matmul(u_ensemble_object.u, w.reshape((-1,1))) 
                        for h_key in h_o_keys for w in [self.final_pho_w_dict[h_key]]}
            z = {h_key: np.linalg.lstsq(h, np.matmul(tmp_u.transpose(), output))[0].flatten()
                  for h_key, output in outputs.items()}
            # z = {h_key: np.put(vec := np.zeros(tmp_u.shape[1]), pivot, np.matmul(tmp_inverse, np.matmul(tmp_u_reduced.transpose(), output))) or vec 
            #      for h_key, output in outputs.items()}
            for h_key in h_o_keys:
                self.final_pho_w_dict[h_key] -= z[h_key]
            for l_key in l_o_keys:
                w_update = np.zeros(tmp_u.shape[1])
                for h_key in h_o_keys:
                    w_update += z[h_key]
                self.final_pho_w_dict[l_key][u_ensemble_object.u_indices[l_key]] += w_update[u_ensemble_object.u_indices[l_key]]
        final_pho_w_dict_matrix = np.concatenate(list(self.final_pho_w_dict.values()), axis = 0).reshape(u_ensemble_object.u.shape[1], n_models, order = "F")
        self.final_pho_w = np.sum(final_pho_w_dict_matrix, 1)
        
    def predict(self, data = None):
        if data is None:
            data = self.fit_data
        tmp_u_ensemble_object = ensemble_u_object(self, data)
        final_pred = np.matmul(tmp_u_ensemble_object.u, self.final_pho_w)
        return(final_pred)
    
    def predict_separate(self, data = None):
        if data is None:
            data = self.fit_data
        tmp_u_ensemble_object = ensemble_u_object(self, data)
        separate_predictions = {key: np.matmul(tmp_u_ensemble_object.u, w) 
                                for key, w in self.final_pho_w_dict.items()}
        return(separate_predictions)