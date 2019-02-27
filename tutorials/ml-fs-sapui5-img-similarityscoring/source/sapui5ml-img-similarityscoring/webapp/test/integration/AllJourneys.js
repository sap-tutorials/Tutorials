/* global QUnit*/

sap.ui.define([
	"sap/ui/test/Opa5",
	"demo/sapui5ml-img-similarityscoring/test/integration/pages/Common",
	"sap/ui/test/opaQunit",
	"demo/sapui5ml-img-similarityscoring/test/integration/pages/demo",
	"demo/sapui5ml-img-similarityscoring/test/integration/navigationJourney"
], function (Opa5, Common) {
	"use strict";
	Opa5.extendConfig({
		arrangements: new Common(),
		viewNamespace: "demo.sapui5ml-img-similarityscoring.view.",
		autoWait: true
	});
});