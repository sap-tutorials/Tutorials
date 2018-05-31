sap.ui.define([
	"sap/ui/core/format/NumberFormat"
], function() {
	"use strict";
	var isNumeric = function(oValue) {
		var tmp = oValue && oValue.toString();
		return !jQuery.isArray(oValue) && (tmp - parseFloat(tmp) + 1) >= 0;
	};
	jQuery.sap.require("sap.ui.core.format.NumberFormat");
	var oNumberFormat = sap.ui.core.format.NumberFormat.getFloatInstance({
		maxFractionDigits: 2,
		groupingEnabled: true,
		groupingSeparator: ",",
		decimalSeparator: "."
	}, sap.ui.getCore().getConfiguration().getLocale());

	var round = function(value, decimal) {
		if (value !== 'undefined' && isNumeric(value)) {
			// return Number(value).toFixed(decimal);
			return oNumberFormat.format(value);
		} else {
			return "nothing";
		}
	};
	return {
		round: function(value) {
			return round(value);
		}
	};
});
