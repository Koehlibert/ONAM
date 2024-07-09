import numpy as np

class data_container:
    def __init__(self, data = None, model_info = None):
        if(data is not None and model_info is not None):
            self.prepare_data(data, model_info)
    
    def prepare_data(self, data, model_info):
        self.n = data.shape[0]
        additive_data = {linear_feature: data.loc[:,linear_feature] for linear_feature in model_info.theta["linear"]}
        deep_data = {order : [np.array(data.loc[:,features]).reshape((-1,len(features))) for features in model_info.theta[order]] for order in set(model_info.theta) - set(["linear"])}
        self.model_data = {"linear": additive_data,
                           "deep": deep_data}
        self.data_dict = ({" X ".join(feature_names): deep_data[order][feature_idx]
                            for order in (set(model_info.theta) - set(["linear"]))
                            for feature_idx, feature_names in enumerate(model_info.theta[order])}|
                           {"linear_" + feature: additive_data[feature] 
                            for feature in model_info.theta["linear"]})
        
    def prepare_partial(self, data, feature):
        self.n = data.shape[0]
        self.data_dict = {feature : data.loc[:,feature]}