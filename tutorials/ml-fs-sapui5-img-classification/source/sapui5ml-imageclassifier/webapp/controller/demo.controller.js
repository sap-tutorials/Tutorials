/* global JSZip:true */
sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageBox"
], function (Controller, MessageBox) {
	"use strict";
	return Controller.extend("sapui5ml.controller.demo", {
		fileTypeMissmatch: function (oControlEvent) {
			MessageBox.show("Wrong file type!");
		},
		clearPredictions: function () {
			this.getView().getModel("demo").setProperty("/predictions", null);
			this.getView().getModel("demo").setProperty("/visible", false);
		},
		addPrediction: function (prediction) {
			var current = this.getView().getModel("demo").getProperty("/predictions");
			if (!current) {
				current = [];
			}
			current.push(prediction);
			// add the results from the model
			this.getView().getModel("demo").setProperty("/predictions", current);
			this.getView().getModel("demo").setProperty("/visible", true);
		},
		displayErrorsOrFinish: function (oController) {
			if (oController.oFilesProcessed === oController.oFiles.length) {
				oController.oBusyIndicator.close();
				if (oController.oErrors.length === 0) {
					MessageBox.show("Process completed!\n Target URL: " + oController.getView().getModel("demo").getProperty("/url"));
				} else {
					var message = "";
					for (var i = 0; i < oController.oErrors.length; i++) {
						message += "\n\t  Error: " + oController.oErrors[i].status + " - " + oController.oErrors[i].message;
					}
					MessageBox.show("Errors: \n" + message);
				}
			}
		},
		getFileContentUrl: function (files, prediction, callback) {
			for (var i = 0; i < files.length; i++) {
				if (files[i].type.match("image.*")) {
					if (files[i].name === prediction.name) {
						callback(prediction, files[i].contentUrl);
					}
				} else {
					JSZip.loadAsync(files[i]).then(function (zip) {
						Object.keys(zip.files).forEach(function (zipEntry) {
							if (zipEntry === prediction.name) {
								zip.files[zipEntry].async("blob").then(function (zipEntryFile) {
									callback(prediction, URL.createObjectURL(zipEntryFile));
								});
							}
						});
					});
				}
			}
		},
		fileUploaderChange: function (oControlEvent) {
			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// clear previous results from the model
			this.clearPredictions();

			// keep a reference of the uploaded file name and create the local url
			var oFiles = oControlEvent.getParameters().files;
			for (var i = 0; i < oFiles.length; i++) {
				oFiles[i].contentUrl = URL.createObjectURL(oFiles[i]);
			}
			// keep a reference in the view to close it later
			this.oBusyIndicator = oBusyIndicator;
			this.oFiles = Object.assign({}, oFiles);
			this.oFiles.length = oFiles.length;
			this.oFilesProcessed = 0;
			this.oErrors = [];
		},
		fileUploaderComplete: function (oControlEvent) {
			var response = JSON.parse(oControlEvent.getParameters().responseRaw);
			this.processResults(this, response);
		},
		processResults: function (oController, response) {
			oController.oFilesProcessed++;
			if (response.status === "DONE") {
				for (var i = 0; i < response.predictions.length; i++) {
					var callback = function (prediction, contentUrl) {
						prediction.contentUrl = contentUrl;
						oController.addPrediction(prediction);
					};
					oController.getFileContentUrl(oController.oFiles, response.predictions[i], callback);
				}
			} else {
				oController.oErrors.push({
					"status": response.error.code,
					"message": response.error.message
				});
			}
			oController.displayErrorsOrFinish(oController);
		},
		/* the following code is used by the Ajax & XHR methods only*/
		onPressImageClassifier: function (oControlEvent) {
			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// clear previous results from the model
			this.clearPredictions();

			// get the call mode ajax or xhr
			var mode = oControlEvent.getSource().data("mode");

			// keep a reference of the uploaded file
			var oFiles = oControlEvent.getParameters().files;

			// keep a reference in the view to close it later
			this.oBusyIndicator = oBusyIndicator;
			this.oFiles = Object.assign({}, oFiles);
			this.oFiles.length = oFiles.length;
			this.oFilesProcessed = 0;
			this.oErrors = [];

			for (var i = 0; i < oFiles.length; i++) {
				this.oFiles[i].contentUrl = URL.createObjectURL(this.oFiles[i]);
				this.callService(this, mode, this.oFiles[i], this.processResults);
			}
		},
		callService: function (oController, mode, file, callback) {
			// create the form data to be sent in the request
			var formData = new window.FormData();
			formData.append("files", file, file.name);

			var url = oController.getView().getModel("demo").getProperty("/url");
			var type = oController.getView().getModel("demo").getProperty("/method");
			var apiKey = oController.getView().getModel("demo").getProperty("/APIKey");
			var accept = oController.getView().getModel("demo").getProperty("/accept");

			var callbackAjaxSuccess = function (data, status, jqXHR) {
				callback(oController, data);
			};
			var callbackAjaxError = function (jqXHR, status, message) {
				oController.clearPredictions();
				var error_message = {
					"error": jqXHR.responseJSON.error
				};
				callback(oController, error_message);
			};
			var callbackXHRReadyStateChange = function () {
				if (this.readyState === this.DONE) {
					if (this.status === 200) {
						callback(oController, JSON.parse(this.response));
					} else {
						oController.clearPredictions();
						var error_message = {
							"error": this.responseJSON.error
						};
						callback(oController, error_message);
					}
				}
			};
			if (mode === "ajax") {
				$.ajax({
					type: type,
					url: url,
					headers: {
						"Accept": accept,
						"APIKey": apiKey
					},
					success: callbackAjaxSuccess,
					error: callbackAjaxError,
					contentType: false,
					async: true,
					data: formData,
					cache: false,
					processData: false
				});
			} else if (mode === "xhr") {
				var xhr = new XMLHttpRequest();
				xhr.withCredentials = false;
				xhr.addEventListener("readystatechange", callbackXHRReadyStateChange);
				xhr.open(type, url, true); // setting request method & API endpoint, the last parameter is to set the calls as asynchyronous
				xhr.setRequestHeader("Accept", accept); // adding request headers
				xhr.setRequestHeader("APIKey", apiKey); // API Key for API Sandbox
				xhr.send(formData); // sending request
			} else {
				oController.oBusyIndicator.close();
			}
		}
	});
});
