sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast",
	"pspredictive/fragment/dataset/DatasetList"
], function(Controller, MessageToast, DatasetList) {
	"use strict";

	jQuery.sap.require("pspredictive.fragment.dataset.DatasetList");

	return Controller.extend("pspredictive.controller.forecast.synchronous", {
		onInit: function() {
			if (typeof sap.ui.getCore().getModel() === 'undefined') {
				this.getView().setModel(new sap.ui.model.json.JSONModel(), "dataset_fragment");
				this.getView().setModel(new sap.ui.model.json.JSONModel(), "forecast_fragment");
			}
		},
		getDatasetList: function() {
			DatasetList.prototype.getDatasetList.apply(this, arguments);
		},
		getDatasetDescription: function(event) {
			DatasetList.prototype.getDatasetDescription.apply(this, arguments);
		},
		forecast: function(event) {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// get the model
			var oModelForecast = oView.getModel("forecast_fragment");

			// get the service parameters value
			var datasetId = this.getView().byId(event.getSource().data("eDatasetID")).getValue();
			var targetColumn = this.getView().byId(event.getSource().data("eTargetColumn")).getSelectedKey();
			var dateColumn = this.getView().byId(event.getSource().data("eDateColumn")).getSelectedKey();
			var numberOfForecasts = this.getView().byId(event.getSource().data("eNumberOfForecasts")).getValue();
			var referenceDate = this.getView().byId(event.getSource().data("eReferenceDate")).getValue();

			// define the service parameters
			var param = {
				datasetID: datasetId,
				targetColumn: targetColumn,
				dateColumn: dateColumn,
				numberOfForecasts: numberOfForecasts,
				referenceDate: referenceDate
			};

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/forecast/sync",
				type: "POST",
				data: JSON.stringify(param),
				dataType: "json",
				async: false,
				success: function(data) {
					try {
						//Save data set description data in the model
						oModelForecast.setProperty("/model", data);
						oBusyIndicator.close();
					} catch (err) {
						MessageToast.show("Caught - forecast [ajax success] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - forecast [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		}
	});
});