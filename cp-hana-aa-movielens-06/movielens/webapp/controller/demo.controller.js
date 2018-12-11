sap.ui.define([
  "sap/ui/core/mvc/Controller",
  "sap/m/MessageToast"
], function(Controller, MessageToast) {
  "use strict";

  return Controller.extend("mlmovielens.controller.demo", {
    onInit: function() {
      if (typeof sap.ui.getCore().getModel() === 'undefined') {
        sap.ui.getCore().setModel(new sap.ui.model.json.JSONModel());
      }
    },
    getSplitAppObj: function() {
      var result = sap.ui.getCore().byId(this.createId("SplitAppDemo"));
      if (!result) {
        MessageToast.show("SplitApp object can't be found", {
          duration: 5000
        });
      }
      return result;
    },
    onDetailListItemPress: function(oEvent) {
      var sToPageId = oEvent.getParameter("listItem").getCustomData()[0].getValue();
      this.getSplitAppObj().toDetail(this.createId(sToPageId));
    }
  });
});