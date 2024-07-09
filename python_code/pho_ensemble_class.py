from model_info_object_class import model_info_object
from pho_model_class import pho_model
from data_container_class import data_container
import model_setup
import keras
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
        # self.final_pho()
        
    # def final_pho(self):
    #     member_predictions = [member.predict_separately(self.fit_data) 
    #                           for member in self.ensemble]
    #     mean_predictions = {key: np.array([member_predictions[key]])}
    #     model_orders = {key: model.order
    #                       for key, model in self.ensemble[0].submodels.model_dict.items()} | {"intercept": 1}
    #     n_models = len(model_orders)
        