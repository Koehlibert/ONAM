import model_definition
import pandas as pd
from pho_ensemble_class import pho_ensemble
import keras
from matplotlib import pyplot as plt

model_formula = "Prediction ~ deep_model(Latitude) + deep_model(Longitude) + deep_model(BioregUpstream2) + deep_model(AreaSqKM) + deep_model(dep_so4_2011) + deep_model(elevation) + deep_model(hydrogroup_d4) + deep_model(percent_sandy) + deep_model(surfcoarse) + deep_model(deg_barr_all_local) + deep_model(upstream.total_precip) + deep_model(Agriculture) + deep_model(Development) + deep_model(Forest) + deep_model(Openwater) + deep_model(Barren) + deep_model(Grass) + deep_model(Woodywetland) + deep_model(Herbwetland) + deep_model(Longitude, Latitude) + deep_model(Development, elevation) + deep_model(Agriculture, elevation) + deep_model(Agriculture, Development) + deep_model(Latitude, Longitude, BioregUpstream2, AreaSqKM, dep_so4_2011, elevation, hydrogroup_d4, percent_sandy, surfcoarse, deg_barr_all_local, Openwater, Barren, upstream.total_precip, Agriculture, Development, Forest, Grass, Woodywetland, Herbwetland)"

BIBIData = pd.read_excel("//imbie-fs/Projekte/Biostatistik/Projekte_Koehler/Deepregression/ONAM/BIBITrain.xlsx")
dict_of_deep_models = {"deep_model": model_definition.submodel}

onam_model = pho_ensemble(model_formula, dict_of_deep_models)
onam_model.create_models(10)

callback = keras.callbacks.EarlyStopping(monitor = "loss", patience = 10)

onam_model.fit(BIBIData, epochs = 500, callbacks = callback)

sep_preds = onam_model.predict_separate()

eff_plot = lambda x: plt.scatter(BIBIData[x], sep_preds[x])
