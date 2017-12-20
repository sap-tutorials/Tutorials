sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast",
	"sap/m/Image"
], function(Controller, MessageToast, Image) {
	"use strict";
	return Controller.extend("sapui5ml.controller.demo", {

		fileUploadChange_xhr: function(oControlEvent) {
			// start the busy indicator
			// var oBusyIndicator = new sap.m.BusyDialog();
			// oBusyIndicator.open();
			var oView = this.getView();

			var file = oControlEvent.getParameters().files[0];
			var data = new window.FormData();

			data.append("files", file, "myimage.png");

			var xhr = new window.XMLHttpRequest();
			xhr.withCredentials = false;
			xhr.addEventListener("readystatechange", function() {
				if (this.readyState === this.DONE) {
					if (xhr.status === 200) {

						if (file.type.match('image.*')) {
							this.oFileSrc = URL.createObjectURL(file);
							// display the uploaded image
							var image = oView.byId("idImage");
							if (this.oFileSrc !== null) {
								image.setSrc(this.oFileSrc);
								image.setVisible(true);
							} else {
								image.setVisible(false);
							}
						} else {
							this.oFileSrc = null;
						}

						// set the response as JSON in the demo model
						oView.getModel("demo").setProperty("/predictions", JSON.parse(xhr.response).predictions);

						// display the result table
						oView.getModel("demo").setProperty("/resultVisible", true);

						// display the uploaded image
						var image = oView.byId("idImage");
						if (this.oFileSrc !== null) {
							image.setSrc(this.oFileSrc);
							image.setVisible(true);
						} else {
							image.setVisible(false);
						}
					} else {
						MessageToast.show("Error " + xhr.status + " : " + xhr.readyState + " : " + xhr.response);
					}
					// // keep a reference in the view to close it later
					// this.oBusyIndicator = oBusyIndicator;
				}
			});
			//setting request method //API endpoint for API sandbox
			xhr.open("POST", oView.getModel("demo").getProperty("/url"), true);
			//adding request headers
			// xhr.setRequestHeader("Content-Type", "multipart/form-data");
			xhr.setRequestHeader("Accept", "application/json");
			//API Key for API Sandbox
			xhr.setRequestHeader("APIKey", oView.getModel("demo").getProperty("/APIKey"));
			//sending request
			xhr.send(data);
		},

		fileUploadChange: function(oControlEvent) {
			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// keep a reference of the uploaded file if this is an image only
			var file = oControlEvent.getParameters().files[0];
			if (file.type.match('image.*')) {
				this.oFileSrc = URL.createObjectURL(file);
				// display the uploaded image
				var image = oView.byId("idImage");
				if (this.oFileSrc !== null) {
					image.setSrc(this.oFileSrc);
					image.setVisible(true);
				} else {
					image.setVisible(false);
				}
			} else {
				this.oFileSrc = null;
			}

			// keep a reference in the view to close it later
			this.oBusyIndicator = oBusyIndicator;
		},
		fileUploadComplete: function(oControlEvent) {
			// get the current view
			var oView = this.getView();
			if (oControlEvent.getParameters().status === 200) {
				// set the response as JSON in the demo model
				oView.getModel("demo").setProperty("/predictions", JSON.parse(oControlEvent.getParameters().responseRaw).predictions);

				// display the result table
				oView.getModel("demo").setProperty("/resultVisible", true);

				// display the uploaded image
				var image = oView.byId("idImage");
				if (this.oFileSrc !== null) {
					image.setSrc(this.oFileSrc);
					image.setVisible(true);
				} else {
					image.setVisible(false);
				}
			} else {
				MessageToast.show("Error " + oControlEvent.getParameters().status + " : " + oControlEvent.getParameters().responseRaw);
			}
			this.oBusyIndicator.close();
		}
	});
});
