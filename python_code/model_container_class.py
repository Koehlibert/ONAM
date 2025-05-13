import model_definition
from pho_sub_model_class import pho_sub_model

class model_container:
    def __init__(self, model_info, inputs_dict):
        self.linear = [pho_sub_model(lin_input, model_definition.linearsubmodel) 
                         for lin_input in inputs_dict["linear"].values()]
        self.deep = {order:
                       [pho_sub_model(deep_input, model_info.dict_of_deep_models[model_info.model_names[order][model_index]])
                        for model_index, deep_input in enumerate(inputs_dict["deep"][order])]
                        for order in (set(model_info.theta) - set(["linear"]))}       
        self.model_dict = ({" X ".join(feature_names): self.deep[order][feature_idx]
                            for order in (set(model_info.theta) - set(["linear"]))
                            for feature_idx, feature_names in enumerate(model_info.theta[order])}|
                           {"linear_" + feature: self.linear[feature_idx] 
                            for feature_idx, feature in enumerate(model_info.theta["linear"])})