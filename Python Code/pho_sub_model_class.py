import keras

class pho_sub_model(keras.Model):
    def __init__(self, inputs, model):
        super().__init__(inputs, model(inputs).output)
        self.intermediary_model = keras.models.Model(inputs, 
                                                     self.layers[len(self.layers) - 2].output)
        self.order = inputs.shape[1]
        self.output_shape_u_dims = self.intermediary_model.output_shape[1] + (self.intermediary_model.output.node.layer.get_config()["use_bias"] if "use_bias" in self.intermediary_model.output.node.layer.get_config() else 0) 
    
    def get_u(self, data):
        return(self.intermediary_model(data))
    
    def get_w(self):
        return(self.layers[-2].get_weights())