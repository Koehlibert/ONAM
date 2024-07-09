import model_definition
import pandas as pd
from pho_ensemble_class import pho_ensemble
import keras
from matplotlib import pyplot as plt
from u_object_class import u_object
from w_object_class import w_object
import numpy as np
import sympy
from copy import deepcopy


model_formula = "rf_pred ~ deep_model(Latitude) + deep_model(Longitude) + deep_model(BioregUpstream2) + deep_model(AreaSqKM) + deep_model(dep_so4_2011) + deep_model(elevation) + deep_model(hydrogroup_d4) + deep_model(percent_sandy) + deep_model(surfcoarse) + deep_model(deg_barr_all_local) + deep_model(upstream.total_precip) + deep_model(Agriculture) + deep_model(Development) + deep_model(Forest) + deep_model(Openwater) + deep_model(Barren) + deep_model(Grass) + deep_model(Woodywetland) + deep_model(Herbwetland) + deep_model(Longitude, Latitude) + deep_model(Development, elevation) + deep_model(Agriculture, elevation) + deep_model(Agriculture, Development) + deep_model(Latitude, Longitude, BioregUpstream2, AreaSqKM, dep_so4_2011, elevation, hydrogroup_d4, percent_sandy, surfcoarse, deg_barr_all_local, Openwater, Barren, upstream.total_precip, Agriculture, Development, Forest, Grass, Woodywetland, Herbwetland) + Barren + Development"
# model_formula = "rf_pred ~ deep_model(Agriculture) + deep_model(Development) + deep_model(elevation) + deep_model(Longitude, Latitude) + deep_model(Latitude, Longitude, BioregUpstream2, AreaSqKM, dep_so4_2011, elevation, hydrogroup_d4, percent_sandy, surfcoarse, deg_barr_all_local, Openwater, Barren, upstream.total_precip, Agriculture, Development, Forest, Grass, Woodywetland, Herbwetland)"
BIBIData = pd.read_excel("//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/BIBITrain.xlsx")
BIBIData["BioregUpstream2"] = pd.factorize(BIBIData["BioregUpstream2"])[0] + 1
dict_of_deep_models = {"deep_model": model_definition.submodel}

test = pho_ensemble(model_formula, dict_of_deep_models)
test.create_models(5)

callback = keras.callbacks.EarlyStopping(monitor = "loss", patience = 10)

test.fit(BIBIData, epochs = 500, callbacks = callback)

tmp_u_object = u_object(test.ensemble[0], test.fit_data)
separate_preds = {effect: np.matmul(tmp_u_object.u, w_effect) for effect, w_effect in test.ensemble[0].pho_w_dict.items()}
separate_preds_old = {effect: np.matmul(tmp_u_object.u, w_effect) for effect, w_effect in test.ensemble[0].w_object.w_dict.items()}
plot_old = lambda feature: plt.scatter(BIBIData.loc[:,feature], separate_preds_old[feature])
plot_pho = lambda feature: plt.scatter(BIBIData.loc[:,feature], separate_preds[feature])
