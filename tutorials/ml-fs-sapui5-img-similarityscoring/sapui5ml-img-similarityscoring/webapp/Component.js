sap.ui.define([
	"sap/ui/core/UIComponent",
	"sap/ui/Device",
	"demosapui5ml-img-similarityscoring/model/models",
	/* for the JSZip library    */ "demosapui5ml-img-similarityscoring/libs/jszip.min",
	/* for the FileSaver library*/ "demosapui5ml-img-similarityscoring/libs/FileSaver.min"	
], function(UIComponent, Device, models) {
	"use strict";

	return UIComponent.extend("demosapui5ml-img-similarityscoring.Component", {

		metadata: {
			manifest: "json"
		},

		/**
		 * The component is initialized by UI5 automatically during the startup of the app and calls the init method once.
		 * @public
		 * @override
		 */
		init: function() {
			// call the base component's init function
			UIComponent.prototype.init.apply(this, arguments);

			// set the device model
			this.setModel(models.createDeviceModel(), "device");
		}
	});
});