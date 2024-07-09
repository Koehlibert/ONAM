import numpy as np
class u_object:
    # def __init__(self, u, u_list, u_dims, u_indices):
    #     self.u = u
    #     self.u_list = u_list
    #     self.u_dims = u_dims
    #     self.u_indices = u_indices
    def __init__(self, pho_model, data_container = None):
        self.u_dims = {model_key: model.output_shape_u_dims for model_key, model in pho_model.submodels.model_dict.items()} | {"intercept": 1}
        tmp_lower = np.cumsum([0] + list(self.u_dims.values())[:-1])
        tmp_upper = np.cumsum(list(self.u_dims.values()))
        self.u_shape = np.max(tmp_upper)
        self.u_indices = {u_key: np.arange(tmp_lower[i], tmp_upper[i]) for i, u_key in enumerate(self.u_dims.keys())}
        if data_container is not None:
            self.get_u(pho_model, data_container)
        
    def get_u(self, pho_model, data_container):
        self.u_dict = {feature_set: pho_model.submodels.model_dict[feature_set].get_u(data_container.data_dict[feature_set]) 
                        for feature_set in data_container.data_dict.keys()}
        self.u_dict = {u_key: u_value.values.reshape(-1,1) if len(u_value.shape) == 1 else u_value for u_key, u_value in self.u_dict.items()}
        self.u_dict = {u_key: np.c_[u_value, np.ones(u_value.shape[0])] if pho_model.submodels.model_dict[u_key].output.node.layer.get_config()["use_bias"] else u_value
                       for u_key, u_value in self.u_dict.items()} | {"intercept": np.ones(data_container.n).reshape(-1,1)}
        
        self.u = np.zeros((data_container.n, self.u_shape))
        for u_key, u_val in self.u_dict.items():
            self.u[:,self.u_indices[u_key]] = u_val
        self.u = np.concatenate(list(self.u_dict.values()), axis = 1)