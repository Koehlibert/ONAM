import numpy as np
from itertools import compress
class model_info_object:
    def __init__(self, model_formula, dict_of_deep_models):
        outcome, rest = model_formula.split("~")
        self.outcome = outcome.strip()
        model_terms = rest.split("+")
        model_terms = [model_term.strip() for model_term in model_terms]
        model_info = [model_term.split("(") for model_term in model_terms]
        model_length = np.array([len(item) for item in model_info])
        linear_terms = [item[0] for item in 
                        list(compress(model_info, model_length == 1))]
        deep_info = list(compress(model_info, model_length != 1))
        deep_names = [item[0] for item in deep_info]
        deep_features = [[feature.strip() for feature in 
                           feature_set.split(",")] 
                          for feature_set in [item[len(item) > 1].split(")")[0] 
                                            for item in deep_info]]
        deep_model_length = np.array([len(item) 
                                           for item in deep_features])
        ordered_deep_length, ordered_deep_models, ordered_deep_names = zip(*sorted(zip(deep_model_length, deep_features, deep_names), reverse=True))
        ordered_deep_length = np.array(ordered_deep_length)
        unique_orders = sorted(list(set(ordered_deep_length)), reverse = True)
        theta_deep = {}
        self.model_names = {}
        for order in unique_orders:
            theta_deep.update({str(order): list(compress(ordered_deep_models, ordered_deep_length == order))})
            self.model_names.update({str(order): list(compress(ordered_deep_names, ordered_deep_length == order))})
        self.theta = theta_deep | {"linear": linear_terms}
        self.dict_of_deep_models = dict_of_deep_models