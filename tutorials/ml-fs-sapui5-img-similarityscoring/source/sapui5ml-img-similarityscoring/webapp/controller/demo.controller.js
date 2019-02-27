/* global JSZip:true */
/* global saveAs:true */
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
			this.getView().getModel("demo").setProperty("/visible-similarityscoring", false);
			this.getView().getModel("demo").setProperty("/visible-featureextraction", false);
		},
		displayErrorsOrFinish: function (oController, service) {
			if (oController.oFilesProcessed === oController.oFiles.length) {
				oController.oBusyIndicator.close();
				if (oController.oErrors.length === 0) {
					MessageBox.show("Process completed!\n Target URL: " + oController.getView().getModel("demo").getProperty("/url_" + service));
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
		callService: function (oController, service, mode, file, callback, options) {
			// create the form data to be sent in the request
			var formData = new window.FormData();
			formData.append("files", file, file.name);
			if (options) {
				formData.append("options", JSON.stringify(options));
			}

			var url = oController.getView().getModel("demo").getProperty("/url_" + service);
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
		},

		addSimilarityScores: function (predictions) {
			// service name
			var service = "similarityscoring";
			var current = this.getView().getModel("demo").getProperty("/predictions");
			for (var i = 0; i < current.length; i++) {
				if (predictions.id === current[i].name + ".json") {
					current[i].results = predictions.similarVectors;
					break;
				}
			}
			// set the results visible
			this.getView().getModel("demo").setProperty("/visible-" + service, true);
		},
		processResultsSimilarity: function (oController, response) {
			// service name
			var service = "similarityscoring";

			oController.oFilesProcessed++;
			if (response.status === "DONE") {
				for (var i = 0; i < response.predictions.length; i++) {
					oController.addSimilarityScores(response.predictions[i]);
				}
			} else {
				oController.oErrors.push({
					"status": response.error.code,
					"message": response.error.message
				});
			}
			oController.displayErrorsOrFinish(oController, service);
		},
		onPressScoreSimilarity: function (oControlEvent) {
			// Keep a reference to the controller
			var oController = this;

			// service name
			var service = "similarityscoring";

			// get the call mode : ajax or xhr
			var mode = oControlEvent.getSource().data("mode");

			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// keep a reference in the view to close it later
			this.oBusyIndicator = oBusyIndicator;
			this.oFiles = {
				"length": 1
			};
			this.oFilesProcessed = 0;
			this.oErrors = [];

			// create the zip file from the array of features vector
			var zip = new JSZip();
			var features = this.getView().getModel("demo").getProperty("/predictions");
			for (var i = 0; i < features.length; i++) {
				features[i].results = [];
				zip.file(features[i].name + ".json", JSON.stringify(features[i].featureVectors));
			}

			// call the service with the generated zip file containing the feature vectors
			zip.generateAsync({
					type: "blob"
				})
				.then(function (content) {
					saveAs(content, "input.zip");
					content.name = "input.zip";
					var options = {
						"numSimilarVectors": (features.length - 1)
					};
					oController.callService(oController, service, mode, content, oController.processResultsSimilarity, options);
				});
		},

		addExtractedFeatures: function (predictions) {
			// service name
			var service = "featureextraction";
			var current = this.getView().getModel("demo").getProperty("/predictions");
			if (!current) {
				current = [];
				this.getView().getModel("demo").setProperty("/predictions", current);
			}
			current.push(predictions);
			// set the results visible
			this.getView().getModel("demo").setProperty("/visible-" + service, true);
		},
		processResultsExtractFeatures: function (oController, response) {
			// service name
			var service = "featureextraction";

			oController.oFilesProcessed++;
			if (response.status === "DONE") {
				for (var i = 0; i < response.predictions.length; i++) {
					var callback = function (prediction, contentUrl) {
						prediction.contentUrl = contentUrl;
						oController.addExtractedFeatures(prediction);
					};
					oController.getFileContentUrl(oController.oFiles, response.predictions[i], callback);
				}
			} else {
				oController.oErrors.push({
					"status": response.error.code,
					"message": response.error.message
				});
			}
			oController.displayErrorsOrFinish(oController, service);
		},
		onPressExtractFeatures: function (oControlEvent) {
			// service name
			var service = "featureextraction";
			// get the call mode : ajax or xhr
			var mode = oControlEvent.getSource().data("mode");

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

			for (var j = 0; j < oFiles.length; j++) {
				this.oFiles[j].contentUrl = URL.createObjectURL(this.oFiles[j]);
				this.callService(this, service, mode, this.oFiles[j], this.processResultsExtractFeatures);
			}
		}
	});
});
