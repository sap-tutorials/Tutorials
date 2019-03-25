sap.ui.define([
  "sap/ui/core/mvc/Controller",
  "sap/m/MessageToast",
  'sap/ui/model/Filter',
  'sap/ui/model/FilterOperator'
], function(Controller, MessageToast, Filter, FilterOperator) {
  "use strict";

  return Controller.extend("mlmovielens.controller.contentbased", {
    onInit: function() {
      if (typeof sap.ui.getCore().getModel() === 'undefined') {
        sap.ui.getCore().setModel(new sap.ui.model.json.JSONModel());
      }
      this.getView().setModel(new sap.ui.model.json.JSONModel(), "item");
    },
    onAfterRendering: function() {
      this.selectItem("");
    },
    formatEpoch: function(value) {
      if (value !== 'undefined' && this.isNumeric(value)) {
        return new Date(value * 1000).toDateString();
      } else {
        return "";
      }
    },
    formatNumber: function(value) {
      if (value !== 'undefined' && this.isNumeric(value)) {
        return Number(value).toFixed(2);
      } else {
        return "";
      }
    },
    formatPercent: function(value) {
      if (value !== 'undefined' && this.isNumeric(value)) {
        return Number(value * 100).toFixed(2) + "%";
      } else {
        return "";
      }
    },
    isNumeric: function(oValue) {
      var tmp = oValue && oValue.toString();
      return !jQuery.isArray(oValue) && (tmp - parseFloat(tmp) + 1) >= 0;
    },
    selectItem: function(value) {
      var oItemModel = this.getView().getModel("item");
      var tableFilters = [];

      // only allow numeric direct input
      if (this.isNumeric(value)) {
        // get the current model
        var oModel = this.getView().getModel();
        var item = oModel.getProperty("/SUMMARY_RATING_MOVIE(" + value + ")", this, true);
        if (item !== 'undefined') {
          tableFilters = [
            new Filter([
              new Filter("MOVIEID", FilterOperator.EQ, item.MOVIEID)
            ], false)
          ];

          oItemModel.setProperty("/selectedItemId", item.MOVIEID);
          oItemModel.setProperty("/item", item);
        }
      } else {
        tableFilters = [
          new Filter([
            new Filter("MOVIEID", FilterOperator.EQ, -1)
          ], false)
        ];
      }
      this.getView().byId("history").getBinding("rows").filter(tableFilters);
      this.getView().byId("recommendation_apl").getBinding("rows").filter(tableFilters);
      this.getView().byId("recommendation_pal").getBinding("rows").filter(tableFilters);
    },
    onSubmit: function(oEvent) {
      var key = oEvent.getParameter("value");
      this.selectItem(key);
    },
    onSuggestionItemSelected: function(oEvent) {
      if (oEvent.getParameter("selectedItem") !== null) {
        var key = oEvent.getParameter("selectedItem").getKey();
        this.selectItem(key);
      }
    },
    onSuggest: function(oEvent) {
      var value = oEvent.getSource().getValue();
      var suggestionFilters = [];
      if (value) {
        // don't search numeric field if the input is not numerci
        if (!this.isNumeric(value)) {
          suggestionFilters = [
            new Filter([
              new Filter("tolower(DESCRIPTION)", FilterOperator.Contains, "'" + value.toLowerCase() + "'"),
              new Filter("tolower(TITLE)", FilterOperator.Contains, "'" + value.toLowerCase() + "'")
            ], false)
          ];
        } else {
          suggestionFilters = [
            new Filter([
              new Filter("tolower(DESCRIPTION)", FilterOperator.Contains, "'" + value.toLowerCase() + "'"),
              new Filter("tolower(TITLE)", FilterOperator.Contains, "'" + value.toLowerCase() + "'"),
              new Filter("MOVIEID", FilterOperator.EQ, value),
              new Filter("RATING_COUNT", FilterOperator.EQ, value)
            ], false)
          ];
        }
      }
      this.getView().byId("input").getBinding("suggestionItems").filter(suggestionFilters);
    }
  });
});