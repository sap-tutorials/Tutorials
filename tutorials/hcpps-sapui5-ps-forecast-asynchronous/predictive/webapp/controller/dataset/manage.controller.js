sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast"
], function(Controller, MessageToast) {
	"use strict";

	return Controller.extend("pspredictive.controller.dataset.manage", {
		onInit: function() {
			if (typeof sap.ui.getCore().getModel() === 'undefined') {
				this.getView().setModel(new sap.ui.model.json.JSONModel(), "dataset_manage");
			}
		},
		getDatasetList: function() {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// get the model
			var oModel = oView.getModel("dataset_manage");

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/dataset",
				type: "GET",
				async: true,
				timeout: 3000000,
				success: function(data) {
					try {
						//Save data set description data in the model
						oModel.setProperty("/datasets", data);
					} catch (err) {
						MessageToast.show("Caught - dataset manage get list [ajax success] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - dataset manage get list [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		},
		getDatasetDescription: function(oControlEvent) {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();

			// get the current view
			var oView = this.getView();

			// get the model
			var oModel = oView.getModel("dataset_manage");

			if (oModel.getProperty("/datasets") !== undefined && oModel.getProperty(
					"/datasets")[oControlEvent.getParameter("rowIndex")] !== undefined) {
				oBusyIndicator.open();
				var dataSetId = oModel.getProperty("/datasets")[oControlEvent.getParameter("rowIndex")].ID;
				// call the service and define call back methods
				$.ajax({
					headers: {
						'Accept': 'application/json',
						'Content-Type': 'application/json'
					},
					url: "/ps/api/analytics/dataset/" + dataSetId,
					type: "GET",
					async: true,
					timeout: 3000000,
					success: function(data) {
						try {
							//Save data set description data in the model
							oModel.setProperty("/dataset", data);
						} catch (err) {
							MessageToast.show("Caught - dataset manage get dataset description [ajax success] :" + err.message);
						}
						oBusyIndicator.close();
					},
					error: function(request, status, error) {
						MessageToast.show("Caught - dataset manage get dataset description [ajax error] :" + request.responseText);
						oBusyIndicator.close();
					}
				});
			}
		},
		deleteDataset: function(event) {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current controller
			var oController = this;

			// get the current view
			var oView = this.getView();

			// get the model
			var oModel = oView.getModel("dataset_manage");
			var bindingProperty = event.getSource().data("bindingProperty");
			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/dataset/" + event.getSource().data("datasetId"),
				type: "DELETE",
				async: true,
				timeout: 3000000,
				success: function() {
					try {
						// remove the previous data
						oModel.setProperty(bindingProperty, undefined);
						// refresh the dataset list
						oController.getDatasetList();
					} catch (err) {
						MessageToast.show("Caught - dataset manage delete dataset description [ajax success] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - dataset manage delete dataset description [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		},
		updateDataset: function(event) {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current controller
			var oController = this;

			// get the current view
			var oView = this.getView();

			// get the model
			var oModel = oView.getModel("dataset_manage");

			var variables = oModel.getProperty("/dataset").variables;
			var param = [];
			for (var i = 0; i < variables.length; i++) {
				param[i] = {
					"name": variables[i].name,
					"value": variables[i].value
				};
			}

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/dataset/" + event.getSource().data("datasetId") + "/variables/update",
				type: "POST",
				data: JSON.stringify(param),
				dataType: "json",
				async: true,
				timeout: 3000000,
				success: function() {
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - dataset manage update dataset description [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		}
	});
});
