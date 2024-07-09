import numpy as np

def prepare_data(original_data, model_info):
    additive_data = {linear_feature: original_data.loc[:,linear_feature] for linear_feature in model_info.theta["linear"]}
    # deep_data = {order : [[np.array(original_data.loc[:,feature]).reshape((-1,1)) for feature in model] for model in model_info_list[0][order]] for order in set(model_info_list[0]) - set(["linear"])}
    deep_data = {order : [np.array(original_data.loc[:,features]).reshape((-1,len(features))) for features in model_info.theta[order]] for order in set(model_info.theta) - set(["linear"])}
    model_data = {"linear": additive_data,
                  "deep": deep_data}
    return(model_data)
