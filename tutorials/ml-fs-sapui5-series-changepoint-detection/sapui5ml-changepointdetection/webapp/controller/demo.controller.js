sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast",
	"sap/m/Image"
], function(Controller, MessageToast, Image) {
	"use strict";
	return Controller.extend("sapui5ml.controller.demo", {

		onGenerate: function(oControlEvent) {
			// get the current view
			var oView = this.getView();

			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			// keep a reference in the view to close it later
			oBusyIndicator.open();

			// get a random number
			var rnd = Math.random();

			// get the expected data points count
			var iRowCount = oView.getModel("demo").getProperty("/options/numDataPoints");
			var iColCount = oView.getModel("demo").getProperty("/options/numDataSeries");

			// build the array to store the generated data
			var oSeriesCol = new Array(iColCount);
			var oSeriesRow = new Array(iRowCount);
			for (var iCol = 0; iCol < iColCount; iCol++) {
				var oCol = {
					"colid": iCol
				};
				oCol.rows = new Array(iRowCount);
				oSeriesCol[iCol] = (oCol);
			}
			for (var iRow = 0; iRow < iRowCount; iRow++) {
				var oRow = {
					"rowid": iRow
				};
				oRow.cols = new Array(iColCount);
				oSeriesRow[iRow] = (oRow);
			}

			// generate the data
			for (var iColValue = 0; iColValue < iColCount; iColValue++) {
				for (var iRowValue = 0; iRowValue < iRowCount; iRowValue++) {
					var value = (Math.cos((1 + iRowValue) * rnd * (1 + iColValue) * 20) * 100).toFixed(2);
					oSeriesCol[iColValue].rows[iRowValue] = (value);
					oSeriesRow[iRowValue].cols[iColValue] = (value);
				}
			}

			// save it in the model
			oView.getModel("demo").setProperty("/cols", oSeriesCol);
			oView.getModel("demo").setProperty("/rows", oSeriesRow);
			oView.getModel("demo").setProperty("/hasData", true);
			oBusyIndicator.close();
		},

		onExecute: function(oControlEvent) {
			// start the busy indicator
			var oBusyIndicator = new sap.m.BusyDialog();
			// keep a reference in the view to close it later
			oBusyIndicator.open();

			// get the current view
			var oView = this.getView();

			// get service settings and options
			var url = oView.getModel("demo").getProperty("/url");
			var APIKey = oView.getModel("demo").getProperty("/APIKey");
			var separator = oView.getModel("demo").getProperty("/options/separator");
			var seriesSeparator = oView.getModel("demo").getProperty("/options/series_separator");

			// generate the options as JSON stringify
			var options = {
				"separator": oView.getModel("demo").getProperty("/options/separator"),
				"series_separator": oView.getModel("demo").getProperty("/options/series_separator")
			};
			oView.getModel("demo").setProperty("/optionsJs", JSON.stringify(options));

			// convert the input data in the proper format using the separator & seriesSeparator options
			var text = "";
			var series = oView.getModel("demo").getProperty("/cols");
			for (var i = 0; i < series.length; i++) {
				text += series[i].rows.join(separator);
				if (i < series.length - 1) {
					text += seriesSeparator;
				}
			}

			// call the service and define call back methods
			$.ajax({
				headers: {
					'Accept': 'application/json',
					'APIKey': APIKey
				},
				url: url,
				type: "POST",
				data: $.param({
					"options": "{\"separator\":\"" + oView.getModel("demo").getProperty("/options/separator") + "\", \"series_separator\":\"" +
						oView.getModel("demo").getProperty("/options/series_separator") + "\"}",
					"texts": text
				}),
				async: false,
				success: function(data) {
					try {
						//get the result size
						var iRowCount = data.response[0].split(",").length;
						var iColCount = data.response.length;

						var oSeriesCol = new Array(iColCount);
						var oSeriesRow = new Array(iRowCount);

						// build the array to store the result data
						for (var iCol = 0; iCol < data.response.length; iCol++) {
							var oCol = {
								"colid": iCol
							};
							oCol.rows = new Array(iRowCount);
							oSeriesCol[iCol] = (oCol);
						}
						for (var iRow = 0; iRow < iRowCount; iRow++) {
							var oRow = {
								"rowid": iRow
							};
							oRow.cols = new Array(iColCount);
							oSeriesRow[iRow] = (oRow);
						}

						// get the reslt data
						for (var iColValue = 0; iColValue < data.response.length; iColValue++) {
							var row = data.response[0].split(",");
							for (var iRowValue = 0; iRowValue < row.length; iRowValue++) {
								oSeriesCol[iColValue].rows[iRowValue] = row[iRowValue];
								oSeriesRow[iRowValue].cols[iColValue] = row[iRowValue];
							}
						}

						// save it in the model
						oView.getModel("demo").setProperty("/resultCols", oSeriesCol);
						oView.getModel("demo").setProperty("/resultRows", oSeriesRow);
						oView.getModel("demo").setProperty("/hasResult", true);
						oBusyIndicator.close();
					} catch (err) {
						MessageToast.show("Caught - [ajax error] :" + err.message);
					}
					oBusyIndicator.close();
				},
				error: function(request, status, error) {
					MessageToast.show("Caught - [ajax error] :" + request.responseText);
					oBusyIndicator.close();
				}
			});
			oBusyIndicator.close();
		}
	});
});