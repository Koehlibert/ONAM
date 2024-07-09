# data_container = test.fit_data
# pho_model = test.ensemble[0]

# tmp_u_object = u_object(pho_model, data_container)
# pho_model.w_object = w_object(pho_model, tmp_u_object)
# new_w_dict = deepcopy(pho_model.w_object.w_dict)
# model_orders = {key: model.order
#                  for key, model in pho_model.submodels.model_dict.items()} | {"intercept": 1}
# unique_sorted_orders = -np.sort(-np.unique(list(model_orders.values())))
# for o_idx in unique_sorted_orders[1:]:
#     l_o_keys = [key for key in model_orders.keys() if model_orders[key] <= o_idx]
#     h_o_keys = [key for key in model_orders.keys() if model_orders[key] > o_idx]
#     rel_indices = np.concatenate([tmp_u_object.u_indices[key] for key in l_o_keys])
#     tmp_u = np.copy(tmp_u_object.u)
#     mask = np.ones(tmp_u.shape[1], dtype = bool)
#     mask.put(rel_indices, 0)
#     tmp_u[:, mask] = 0
#     tmp_u[:, -1] = 1
#     h = np.matmul(tmp_u.transpose(), tmp_u)
#     # tmp, pivot = sympy.Matrix(h).T.rref(iszerofunc=lambda x: abs(x)<1e-8)
#     # tmp_u_reduced = tmp_u[:,pivot]
#     # tmp_inverse = np.linalg.inv(np.matmul(tmp_u_reduced.transpose(), tmp_u_reduced))
#     outputs = {h_key: np.matmul(tmp_u_object.u, w.reshape((-1,1))) 
#                for h_key in h_o_keys for w in [new_w_dict[h_key]]}
#     z = {h_key: np.linalg.lstsq(h, np.matmul(tmp_u.transpose(), output))[0].flatten()
#          for h_key, output in outputs.items()}
#     # z = {h_key: np.put(vec := np.zeros(tmp_u.shape[1]), pivot, np.matmul(tmp_inverse, np.matmul(tmp_u_reduced.transpose(), output))) or vec 
#     #      for h_key, output in outputs.items()}
#     for h_key in h_o_keys:
#         new_w_dict[h_key] -= z[h_key]
#     for l_key in l_o_keys:
#         w_update = np.zeros(tmp_u.shape[1])
#         for h_key in h_o_keys:
#             w_update += z[h_key]
#         new_w_dict[l_key][tmp_u_object.u_indices[l_key]] += w_update[tmp_u_object.u_indices[l_key]]
# all_o_means = [np.mean(np.matmul(tmp_u_object.u, w)) for w in new_w_dict.values()]
# [w.put(-1, (-all_o_means[w_idx] if (w_idx != len(new_w_dict) - 1) else np.sum(all_o_means))) for w_idx, w in enumerate(new_w_dict.values())]
# pho_model.pho_w_dict = new_w_dict
# pho_w_matrix = np.concatenate(list(new_w_dict.values()), axis = 0).reshape(tmp_u_object.u.shape[1], len(pho_model.w_object.w_dict.values()), order = "F")
# pho_model.pho_w_big = np.sum(pho_w_matrix, 1)
# w = pho_model.w_object.w

# plt.scatter(pho_model.pho_w_big, w)
# plt.plot(pho_model.pho_w_big - w)

# pho_outputs_sep = {w_key: np.matmul(tmp_u_object.u, w) for w_key, w in new_w_dict.items()}
# pho_sep_test = {w_key: np.matmul(tmp_u_object.u_dict[w_key], w[tmp_u_object.u_indices[w_key]]) for w_key, w in new_w_dict.items()}
# pre_sep_test = {w_key: np.matmul(tmp_u_object.u_dict[w_key], w[tmp_u_object.u_indices[w_key]]) for w_key, w in test.ensemble[0].w_object.w_dict.items()}

# testplot = lambda index: plt.scatter(data_container.data_dict["Latitude"], tmp_u_object.u_dict["Latitude"][:,index] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][index]])

# plt.scatter(data_container.data_dict["Latitude"], tmp_u_object.u_dict["Latitude"][:,0] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][0]] +
#             tmp_u_object.u_dict["Latitude"][:,1] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][1]] +
#             tmp_u_object.u_dict["Latitude"][:,2] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][2]] +
#             tmp_u_object.u_dict["Latitude"][:,3] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][3]] +
#             tmp_u_object.u_dict["Latitude"][:,4] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][4]] +
#             tmp_u_object.u_dict["Latitude"][:,5] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][5]] +
#             tmp_u_object.u_dict["Latitude"][:,6] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][6]] +
#             tmp_u_object.u_dict["Latitude"][:,7] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][7]] +
#             tmp_u_object.u_dict["Latitude"][:,8] * new_w_dict["Latitude"][tmp_u_object.u_indices["Latitude"][8]])

# output_post = np.matmul(tmp_u_object.u, pho_model.pho_w_big)
# output_pre = np.matmul(tmp_u_object.u, w)
# plt.scatter(output_pre, output_post)
# plt.plot(output_pre - output_post)

member_predictions = [member.predict_separately(test.fit_data) 
                      for member in test.ensemble]
mean_predictions = {key: np.array([member_prediction[key] for member_prediction in member_predictions]).mean(axis = 0) for key in test.fit_data.data_dict.keys()}
model_orders = {key: model.order
                  for key, model in test.ensemble[0].submodels.model_dict.items()} | {"intercept": 1}
n_models = len(model_orders)
