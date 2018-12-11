---
title: Consume a Basic OData Service
description: Consume a Basic OData Service within UI5 binding the service to a Table
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>html5, topic>odata, topic>sapui5, products>sap-hana, products>sap-hana\,-express-edition   ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [SAPUI5 User Interface](https://www.sap.com/developer/tutorials/xsa-sapui5.html)

## Next Steps
- [Use OData Metadata to dynamically create the columns](https://www.sap.com/developer/tutorials/xsa-sapui5-metadata.html)

## Details
### You will learn  
SAPUI5 uses data binding to bind two data sources or information sources together to keep them in sync: All changes in one source are also reflected in the other one. In this tutorial, you will consume a Basic OData Service within UI5 binding the service to a table.


### Time to Complete
**15 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Create new folder)]

Create a folder named `odataBasic` within the `web/resources` folder

![new folder](1.png)

Within this folder, create two files – one named `Component.js` and one named `index.html`.  Also create a sub-folder named `view` and another one named `controller`.

![new files](2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create required files)]

Add the following code in `index.html`:

```html
<!DOCTYPE html>
<html>
<head>
	<meta http-equiv="X-UA-Compatible" content="IE=edge" />
	<meta charset="UTF-8">
	<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />
   		<link type="image/x-icon" href="/images/favicon.ico" rel="shortcut icon">
        <link type="image/x-icon" href="/images/favicon.ico" rel="icon">
	    <!-- <script id="sap-ui-bootstrap" src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js" -->
	<script id="sap-ui-bootstrap" src="{{{ui5liburl}}}/resources/sap-ui-core.js"
		data-sap-ui-theme="sap_belize_plus"
		data-sap-ui-xx-bindingSyntax="complex"
		data-sap-ui-compatVersion="edge"
		data-sap-ui-preload="async"
		data-sap-ui-language="en"
		data-sap-ui-resourceroots='{
				"dev.odataBasic": "./" }'		
		data-sap-ui-libs="sap.m,sap.ui.comp,sap.ui.core,sap.ui.layout">
	</script>

	<script>

		sap.ui.getCore().attachInit(function () {
				var ComponentContainer = new sap.ui.core.ComponentContainer({
            		height : "100%"
            	});

            	new sap.m.Shell({
            		app: ComponentContainer,
            		showLogout: true
            	}).placeAt("content");

            	var oComponent = sap.ui.component({
            		id: "comp",
            		name: "dev.odataBasic",
            		manifestFirst: true,
            		async: true
            	}).then(function(oComponent){
            		ComponentContainer.setComponent(oComponent);
            	});
		});
	</script>
</head>
<body class="sapUiBody" role="application">
	<div id="content"></div>
</body>
</html>

```

Add the following code to `Component.js`:

```javascript
/*eslint no-console: 0, no-unused-vars: 0, no-use-before-define: 0, no-redeclare: 0*/
sap.ui.define([
	"sap/ui/core/UIComponent",
	"sap/ui/model/json/JSONModel",
	"sap/ui/Device"
], function(UIComponent,JSONModel, Device) {
	"use strict";

	return UIComponent.extend("dev.odataBasic.Component", {

		metadata: {
			manifest: "json"
		},

		init: function() {
			jQuery.sap.require("sap.m.MessageBox");
			jQuery.sap.require("sap.m.MessageToast");

			var oModel = new JSONModel(Device);
			oModel.setDefaultBindingMode("OneWay");

			this.setModel(oModel, "device");

			sap.ui.core.UIComponent.prototype.init.apply(
				this, arguments);
			this.getSessionInfo();
		},

		destroy: function() {
			// call the base component's destroy function
			UIComponent.prototype.destroy.apply(this, arguments);
		},

		getSessionInfo: function() {
			var aUrl = "/xsjs/exercisesMaster.xsjs?cmd=getSessionInfo";
			this.onLoadSession(
				JSON.parse(jQuery.ajax({
					url: aUrl,
					method: "GET",
					dataType: "json",
					async: false
				}).responseText));
		},

		onLoadSession: function(myJSON) {
			for (var i = 0; i < myJSON.session.length; i++) {
				var config = this.getModel("config");
				config.setProperty("/UserName", myJSON.session[i].UserName);
			}
		}
	});

});

```

At the same level as `index.html` create a file called `manifest.json`. Enter the following code:

```javascript
{
  "_version": "1.4.0",
  "start_url": "index.html",
  "sap.app": {
  	"_version": "1.4.0",
	"type": "application",
	"resources": "resources.json",
  	"id": "odataBasic",
  	"title": "Basic OData App",
  	"description": "This is a SAPui5 application",
  	"applicationVersion": {
			"version": "${project.version}"
	},
	//To-Do data source

  },
  "sap.fiori": {
	"_version": "2.0.0",
	"registrationIds": [],
	"archeType": "transactional"
  },
  "sap.ui": {
  	"_version": "1.40.0",
	"technology": "UI5",
  	 "icons":  {
  	 	"icon": "/images/favicon.ico",
  	 	"favIcon": "/images/favicon.ico"
  	 },
  	"deviceTypes": {
		"desktop": true,
		"tablet": true,
		"phone": true
	},
	"supportedThemes": [
			"sap_hcb",
			"sap_bluecrystal",
			"sap_belize"
	]
  },
  "sap.ui5": {
    "config": {
      "sapFiori2Adaptation": true
    },
    "rootView": {
    	"viewName": "dev.odataBasic.view.App",
    	"type": "XML",
    	"id": "app"
    },
    "dependencies": {
			"minUI5Version": "1.40.0",
			"libs": {
				"sap.ui.core": {
					"minVersion": "1.40.0"
				},
				"sap.ui.comp": {
					"minVersion": "1.40.0"					
				},
				"sap.m": {
					"minVersion": "1.40.0"
				},
				"sap.ui.layout": {
					"minVersion": "1.40.0"
				}
			}
		},
	"contentDensities": {
		"compact": true,
		"cozy": true
	},
    "handleValidation": true,
    "models": {
    	"": {
    		"type": "sap.ui.model.json.JSONModel",
    		"settings": {
    			"defaultBindingMode": "TwoWay"
    		}
    	},
    	//To-Do bpModel

    	"config": {
    		"type": "sap.ui.model.json.JSONModel"
    	}
    }

  }
}
```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create controller and view)]

Within the view folder, create a file called `App.view.xml` with the following code:

```xml
<core:View xmlns="sap.m" xmlns:mvc="sap.ui.core.mvc" xmlns:u="sap.ui.unified" xmlns:core="sap.ui.core"
	xmlns:smartTable="sap.ui.comp.smarttable" controllerName="dev.odataBasic.controller.App">
	<u:Shell id="myShell" icon="/images/sap_18.png">
		<u:user>
			<u:ShellHeadUserItem image="sap-icon://person-placeholder" username="{config>/UserName}"/>
		</u:user>
		<u:content>
			<smartTable:SmartTable id="bpTable" header="Business Partners List" editable="false" showRowCount="true" enableAutoBinding="true"
				showFullScreenButton="true" tableType="Table">
				<smartTable:layoutData>
					<FlexItemData growFactor="1" baseSize="0%"/>
				</smartTable:layoutData>
			</smartTable:SmartTable>
		</u:content>
	</u:Shell>
</core:View>
```

Within the `controller` folder, add the following code:


```javascript
/*eslint no-console: 0, no-unused-vars: 0, no-use-before-define: 0, no-redeclare: 0, no-undef: 0*/
//To use a javascript controller its name must end with .controller.js
sap.ui.define([
    "sap/ui/core/mvc/Controller",
		"sap/ui/core/routing/History",
	  "sap/ui/model/json/JSONModel"
], function(Controller, History, JSONModel) {
	"use strict";

	return Controller.extend("dev.odataBasic.controller.App", {

		onInit: function() {
			this.getView().addStyleClass("sapUiSizeCompact"); // make everything inside this View appear in Compact mode
			var oConfig = this.getOwnerComponent().getModel("config");
			var userName = oConfig.getProperty("/UserName");
			var bpModel = this.getOwnerComponent().getModel("bpModel");
			var oTable = this.getView().byId("bpTable");

			//To-Do Bind Model to table

		},

		onErrorCall: function(oError) {
			if (oError.statusCode === 500 || oError.statusCode === 400 || oError.statusCode === "500" || oError.statusCode === "400") {
				var errorRes = JSON.parse(oError.responseText);
				if (!errorRes.error.innererror) {
					sap.m.MessageBox.alert(errorRes.error.message.value);
				} else {
					if (!errorRes.error.innererror.message) {
						sap.m.MessageBox.alert(errorRes.error.innererror.toString());
					} else {
						sap.m.MessageBox.alert(errorRes.error.innererror.message);
					}
				}
				return;
			} else {
				sap.m.MessageBox.alert(oError.response.statusText);
				return;
			}

		},
    /**The following methods could be added to a reusable base controller
			 * Convenience method for accessing the router in every controller of the application.
			 * @public
			 * @returns {sap.ui.core.routing.Router} the router for this component
			 */
			getRouter : function () {
				return this.getOwnerComponent().getRouter();
			},

			/**
			 * Convenience method for getting the view model by name in every controller of the application.
			 * @public
			 * @param {string} sName the model name
			 * @returns {sap.ui.model.Model} the model instance
			 */
			getModel : function (sName) {
				return this.getView().getModel(sName);
			},

			/**
			 * Convenience method for setting the view model in every controller of the application.
			 * @public
			 * @param {sap.ui.model.Model} oModel the model instance
			 * @param {string} sName the model name
			 * @returns {sap.ui.mvc.View} the view instance
			 */
			setModel : function (oModel, sName) {
				return this.getView().setModel(oModel, sName);
			},


			/**
			 * Event handler for navigating back.
			 * It there is a history entry we go one step back in the browser history
			 * If not, it will replace the current entry of the browser history with the master route.
			 * @public
			 */
			onNavBack : function() {
				var sPreviousHash = History.getInstance().getPreviousHash();

					if (sPreviousHash !== undefined) {
					history.go(-1);
				} else {
					this.getRouter().navTo("master", {}, true);
				}
			}

	});
});

```
![new files](3.png)




[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Understanding the code - fill in the `To-Dos` to create a model from a data source)]

So far, you have created a skeleton of an application.  Just copying and pasting is no fun, so you have some pending coding to do. The objective here is to bind the `xsodata` service you created within the `js` module to a table displayed in your SAPUI5 interface. You will reuse the service `businessPartners.xsodata`. In order to achieve this, you need to declare this service as a data source.  This is done in the `manifest.json` file, replacing the first `//To-Do` comment with the following:

```javascript

"dataSources": {
      "bpService": {
        "uri": "/xsodata/businessPartners.xsodata/",
        "type": "OData",
        "settings": {
          "odataVersion": "2.0"
        }
      }
    }

```

Scroll down until the next `//To-Do` comment and add the following model:

```javascript
"bpModel": {
        	"dataSource": "bpService",
        	"type": "sap.ui.model.odata.v2.ODataModel",
        	"preload": true,
        	"settings": {
        		"useBatch": false,
        		"json": true,
				"defaultBindingMode": "TwoWay",
				"defaultUpdateMethod": "PUT"
        	}
    	},    


```



[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Fill in the `To-Do` in the controller)]

Now that you have a data source and a model exposing it, you need to bind it to the table in your view. You will first bind the table to the entity `BusinessPartners` and tell the `SmartTable` control which columns to display. You fill find the to-do in `onInit` method in the controller:

```javascript

oTable.setModel(bpModel);
oTable.setEntitySet("BP");
oTable.setInitiallyVisibleFields("PARTNERID,COMPANYNAME,PARTNERROLE");

```


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Save and Run)]

Save the files you have modified and run the web module

![Run the web module](4.png)

Change the path in the new tab:

![Run the web module](5.png)

You will get this error:

![Error](6.png)

Which you can solve in the next tutorial. You will then see the model based on the `hardcoded` columns.

![BP Model](7.png)


[ACCORDION-END]


## Next Steps
- [Use OData Metadata to dynamically create the columns](https://www.sap.com/developer/tutorials/xsa-sapui5-metadata.html)
