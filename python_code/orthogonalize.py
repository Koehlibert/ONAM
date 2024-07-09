import model_setup
import keras
import numpy as np
import model_definition
from pho_sub_model_class import pho_sub_model
from keras.layers import Dense, Input, Concatenate, Lambda, Dropout

def create_inputs(model_info):
    if "linear" in model_info.theta:
        lin_inputs = {tmp: keras.Input(shape=1)
                      for tmp in model_info.theta["linear"]}
    else:
        lin_inputs = {}
    deep_inputs = {order: [keras.Input(shape=int(order)) for model in model_info.theta[order]]
                   for order in set(model_info.theta) - set(["linear"])}
    inputs_dict = {"linear": lin_inputs,
                   "deep": deep_inputs}
    return (inputs_dict)

def compile_model(inputs_dict, submodels):
    sub_models_linear = submodels.linear
    sub_models_deep = [sub_model
           for models_of_order in submodels.deep.values()
           for sub_model in models_of_order]
    model_list = sub_models_linear + sub_models_deep
    whole_model = concatenate_model_list(model_list)
    merged_model = keras.models.Model(inputs_dict, whole_model)
    merged_model.compile(optimizer = keras.optimizers.Adam(),
                         loss = keras.losses.MeanSquaredError())
    return(merged_model)
    
def concatenate_model_list(model_list, bias = False):
    tmp_output = Dense(1, activation = "linear", use_bias = bias, 
                       trainable = False)(Concatenate(axis = -1)([model.output for model in model_list]))
    tmp_weights = [np.ones(len(model_list)).reshape((-1,1))]
    if bias :
        tmp_weights.append(np.ones(1))
    tmp_output.node.layer.set_weights(tmp_weights)
    return(tmp_output)
    
def get_model_index_list(model_info_list):
    model_index_list = [[order_index, model_index] 
                        for order_index, order in enumerate(model_info_list[0].keys()) 
                        for model_index in range(len(model_info_list[0][order]))]
    return(model_index_list)
