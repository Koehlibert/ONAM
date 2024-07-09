import keras
from keras.layers import Dense, Dropout

def submodel(inputs, regularizer = None):
    model = keras.Sequential(
        [inputs,
        Dense(128, "relu", True, regularizer),
        Dropout(0.2),
        Dense(64, "relu", True, regularizer),
        Dense(32, "relu", True, regularizer),
        Dense(16, "relu", True, regularizer),
        Dense(8, "relu", True, regularizer),
        Dense(1, "linear", True)])
    return(model)

def linearsubmodel(inputs):
    model = keras.Sequential(
        [inputs,
        Dense(1, "linear", False)])
    return(model)