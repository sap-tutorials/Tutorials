sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast",
	"sap/m/Image"
], function(Controller, MessageToast, Image) {
	"use strict";
	return Controller.extend("sapui5ml.controller.demo", {

		fileUploadChange: function(oControlEvent) {
			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			oBusyIndicator.open();

			// keep a reference of the uploaded file if this is an image only
			var file = oControlEvent.getParameters().files[0];
			if (file.type.match('image.*')) {
				this.oFileSrc = URL.createObjectURL(file);
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