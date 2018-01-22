sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast"
], function(Controller, MessageToast) {
	"use strict";

	var defaultFileSrcUrl = "/resources/sap/ui/documentation/sdk/images/logo_ui5.png";

	return Controller.extend("sapui5ml.controller.demo", {
		fileTypeMissmatch: function(oControlEvent) {
			MessageToast.show("Wrong file type!");
		},
		fileUploadChange: function(oControlEvent) {
			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// init the src file, name & url
			this.srcFileURL = null;
			this.srcFileName = null;
			this.srcFile = null;

			// keep a reference of the uploaded file name and create a url out of that when this is an image
			this.srcFile = oControlEvent.getParameters().files[0];
			this.srcFileName = this.srcFile.name;
			if (this.srcFile.type.match("image.*")) {
				this.srcFileURL = URL.createObjectURL(this.srcFile);
			}
			// keep a reference in the view to close it later
			this.oBusyIndicator = oBusyIndicator;
		},
		fileUploadComplete: function(oControlEvent) {
			// get the current view
			var oView = this.getView();

			var service = "imageclassifier";

			// clear previous results from the model
			oView.getModel("demo").setProperty("/result-" + service, null);
			oView.getModel("demo").setProperty("/resultVisible-" + service, false);

			var srcFileIsImage = this.srcFile.type.match('image.*');

			var processResult = function(oController, data, fileName, url) {
				// set the image urls to default if part of a zip
				if (!srcFileIsImage) {
					for (var i = 0; i < data.predictions.length; i++) {
						data.predictions[i].name = fileName + "  /  " + data.predictions[i].name;
						data.predictions[i].fileURL = defaultFileSrcUrl;
					}
				} else {
					data.predictions[0].fileURL = URL.createObjectURL(oController.srcFile);
				}

				// merge with existing results
				var result = oController.getView().getModel("demo").getProperty("/result-" + service);
				if (result) {
					result.push.apply(result, data.predictions);
				} else {
					result = data.predictions;
				}
				oController.getView().getModel("demo").setProperty("/result-" + service, result);

				// display the result table
				oController.getView().getModel("demo").setProperty("/resultVisible-" + service, true);
			};

			if (oControlEvent.getParameters().status === 200) {
				// get the response as JSON and process the results
				processResult(this, JSON.parse(oControlEvent.getParameters().responseRaw), this.srcFile.nam, this.srcFileURL);
			} else {
				oView.getModel("demo").setProperty("/resultVisible", false);
				MessageToast.show("Error " + oControlEvent.getParameters().status + " : " + oControlEvent.getParameters().responseRaw);
			}
			this.oBusyIndicator.close();
		},

		onPressImageClassifier: function(oControlEvent) {
			// get the current controller & view
			var oView = this.getView();

			// start the busy indicator
			this.oBusyIndicator = new sap.m.BusyDialog();
			this.oBusyIndicator.open();

			this.requestCount = 0;

			var service = "imageclassifier";
			var url = oView.getModel("demo").getProperty("/url");
			var type = "POST";
			var apiKey = oView.getModel("demo").getProperty("/APIKey");

			// clear previous results from the model
			oView.getModel("demo").setProperty("/result-" + service, null);
			oView.getModel("demo").setProperty("/resultVisible-" + service, false);

			var srcFile = null;
			var srcFileURL = null;
			var srcFileIsImage = false;

			var processResult = function(oController, data, fileName) {
				if (!srcFileIsImage) {
					for (var i = 0; i < data.predictions.length; i++) {
						data.predictions[i].name = fileName + "  /  " + data.predictions[i].name;
						data.predictions[i].fileURL = defaultFileSrcUrl;
					}
				} else {
					data.predictions[0].fileURL = srcFileURL;
				}

				var result = oController.getView().getModel("demo").getProperty("/result-" + service);
				if (result) {
					result.push.apply(result, data.predictions);
				} else {
					result = data.predictions;
				}
				oController.getView().getModel("demo").setProperty("/result-" + service, result);

				// display the result table
				oController.getView().getModel("demo").setProperty("/resultVisible-" + service, true);
			};

			// keep a reference of the uploaded files
			var mode = oControlEvent.getSource().data("mode");
			for (var fileIndex = 0; fileIndex < oControlEvent.getParameters().files.length; fileIndex++) {
				srcFile = oControlEvent.getParameters().files[fileIndex];
				if (srcFile.type.match("image.*")) {
					srcFileIsImage = true;
					srcFileURL = URL.createObjectURL(srcFile);
				} else {
					srcFileIsImage = false;
				}
				// create the form data to be sent in the request
				var formData = new window.FormData();
				formData.append("files", srcFile, srcFile.name);

				// increase request countor to close busy indicator
				this.requestCount++;

				// call the service
				this.callService(this, service, url, type, mode, apiKey, formData, processResult);
			}
		},
		callService: function(oController, service, url, type, mode, apiKey, formData, fnPrecessResult) {
			var ajaxSuccess = function(data, status, jqXHR) {
				// get the response as JSON and process the results
				fnPrecessResult(oController, data, formData.values().next().value.name);

				// close the busy indicator if all request have completed
				oController.requestCount--;
				if (oController.requestCount === 0) {
					// close the busy indicator
					oController.oBusyIndicator.close();
				}
			};
			var ajaxError = function(jqXHR, status, message) {
				oController.getView().getModel("demo").setProperty("/resultVisible-" + service, false);
				MessageToast.show("Error for file : " + formData.values().next().value.name + " \n status: " + status + "\n message: " + message);
			};
			var xhrReadyStateChange = function() {
				if (this.readyState === this.DONE) {
					if (this.status === 200) {
						// get the response as JSON and process the results
						fnPrecessResult(oController, JSON.parse(this.response), formData.values().next().value.name);
					} else {
						oController.getView().getModel("demo").setProperty("/resultVisible-" + service, false);
						MessageToast.show("Error for file : " + formData.values().next().value.name + " \n status: " + this.status + "\n message: " +
							this.response);
					}
					// close the busy indicator if all request have completed
					oController.requestCount--;
					if (oController.requestCount === 0) {
						oController.oBusyIndicator.close();
					}
				}
			};
			if (mode === "ajax") {
				$.ajax({
					type: type,
					url: url,
					headers: {
						"Accept": "application/json",
						"APIKey": apiKey
					},
					success: ajaxSuccess,
					error: ajaxError,
					contentType: false,
					async: false,
					data: formData,
					cache: false,
					processData: false
				});
			} else if (mode === "xhr") {
				var xhr = new XMLHttpRequest();
				xhr.withCredentials = false;
				xhr.addEventListener("readystatechange", xhrReadyStateChange);
				xhr.open(type, url, false); // setting request method & API endpoint, the last parameter is to set the calls as synchyronous
				xhr.setRequestHeader("Accept", "application/json"); // adding request headers
				xhr.setRequestHeader("APIKey", apiKey); // API Key for API Sandbox
				xhr.send(formData); // sending request
			} else {
				oController.oBusyIndicator.close();
			}
		}
	});
});
