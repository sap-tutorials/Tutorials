sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast",
	"sap/m/MessageBox",
	"pspredictive/fragment/dataset/DatasetList",
	"pspredictive/model/formatter"
], function(Controller, MessageToast, MessageBox, DatasetList, formatter) {
	"use strict";

	jQuery.sap.require("pspredictive.fragment.dataset.DatasetList");

	return Controller.extend("pspredictive.controller.forecast.asynchronous", {
		formatter: formatter,
		onInit: function() {
			if (typeof sap.ui.getCore().getModel() === 'undefined') {
				this.getView().setModel(new sap.ui.model.json.JSONModel(), "dataset_fragment");
				this.getView().setModel(new sap.ui.model.json.JSONModel(), "forecast_fragment");
				this.getView().setModel(new sap.ui.model.json.JSONModel(), "job_fragment");
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
			var oModelJob = oView.getModel("job_fragment");

			// get the service parameters value
			var datasetId = this.getView().byId(event.getSource().data("eDatasetID")).getValue();
			var targetColumn = this.getView().byId(event.getSource().data("eTargetColumn")).getSelectedKey();
			var dateColumn = this.getView().byId(event.getSource().data("eDateColumn")).getSelectedKey();
			var numberOfForecasts = this.getView().byId(event.getSource().data("eNumberOfForecasts")).getValue();
			var referenceDate = this.getView().byId(event.getSource().data("eReferenceDate")).getValue();
			var forecastMethod = this.getView().byId(event.getSource().data("eForecastMethod")).getSelectedKey();
			var smoothingCycleLength = this.getView().byId(event.getSource().data("eSmoothingCycleLength")).getValue();
			var maxLag = this.getView().byId(event.getSource().data("eMaximumLag")).getValue();
			var numberOfPastValuesInOutput = this.getView().byId(event.getSource().data("eNumberOfPastValuesInOutput")).getValue();

			// define the service parameters
			var param = {
				datasetID: datasetId,
				targetColumn: targetColumn,
				dateColumn: dateColumn,
				numberOfForecasts: numberOfForecasts,
				referenceDate: referenceDate,
				forecastMethod: forecastMethod,
				smoothingCycleLength: smoothingCycleLength,
				maxLag: maxLag,
				numberOfPastValuesInOutput: numberOfPastValuesInOutput
			};

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/forecast",
				type: "POST",
				data: JSON.stringify(param),
				dataType: "json",
				async: true,
				timeout: 3000000,
				success: function(data) {
					try {
						//Save data set description data in the model
						oModelJob.setProperty("/job", data);
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
		},
		checkStatus: function() {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// get the model
			var oModelJob = oView.getModel("job_fragment");

			// get the service parameters value
			var jobId = oModelJob.getProperty("/job/ID");

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/forecast/" + jobId + "/status",
				type: "GET",
				async: true,
				timeout: 3000000,
				success: function(data) {
					try {
						//Save data set description data in the model
						oModelJob.setProperty("/job", data);
						oBusyIndicator.close();
					} catch (err) {
						MessageToast.show("Caught - forecast check status [ajax success] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - forecast check status [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		},
		getResults: function() {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// get the model
			var oModelJob = oView.getModel("job_fragment");
			var oModelForecast = oView.getModel("forecast_fragment");

			// get the service parameters value
			var jobId = oModelJob.getProperty("/job/ID");

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/forecast/" + jobId,
				type: "GET",
				async: true,
				timeout: 3000000,
				success: function(data) {
					try {
						//Save data set description data in the model
						oModelForecast.setProperty("/model", data);
						oBusyIndicator.close();
					} catch (err) {
						MessageToast.show("Caught - forecast get results [ajax success] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - forecast get results [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		},
		deleteResults: function() {
			// set the busy indicator to avoid multi clicks
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// get the model
			var oModelJob = oView.getModel("job_fragment");
			var oModelForecast = oView.getModel("forecast_fragment");

			// get the service parameters value
			var jobId = oModelJob.getProperty("/job/ID");

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'Content-Type': 'application/json'
				},
				url: "/ps/api/analytics/forecast/" + jobId,
				type: "DELETE",
				async: true,
				timeout: 3000000,
				success: function(data) {
					try {
						//Save data set description data in the model
						oModelJob.setProperty("/job", undefined);
						oModelForecast.setProperty("/model", undefined);
						oBusyIndicator.close();
					} catch (err) {
						MessageToast.show("Caught - forecast delete results [ajax success] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - forecast delete results [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
		}
	});
});
