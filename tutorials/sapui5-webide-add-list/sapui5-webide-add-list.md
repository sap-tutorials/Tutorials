---
title: Add a List Control to the View
description: Add a list view to a SAPUI5 page.
auto_validation: true
primary_tag: topic>sapui5
author_name: Marius Obert
author_profile: https://github.com/iobert
tags: [  tutorial>beginner, topic>html5, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, products>sap-web-ide ]
time: 15
---

## Details
### You will learn  
Add a list to the XML view (`View1.xml`). The list will display data from the data model, that got filled with data from the Northwind data service. Every list element will represent a product entity in the data model. Thanks to the data binding, SAPUI5 takes the path of the aggregation and automatically creates as many list items as the aggregation includes (all the product entities).For now, you will just display the product names, more specifically the attribute `ProductName`. In a future tutorial, you will add a detail view with additional information per list element, so you will also implement a mock function for the press event, which is triggered on key-press or tap on mobile devices.You will also change the master view and controller. To fix a design-flaw of the template, we will also quickly update the CSS styling.

---

[ACCORDION-BEGIN [Step : ](Add a list control to the view)]
Open the file `mta_app/app/webapp/view/View1.view.xml` in your editor.  

You will add a new `<List>` element, and define how every item will be displayed.

```XML
<List items="{/Products}">
  <StandardListItem type="Active" press="handleListItemPress" title="{ProductName}"/>
</List>
```

![View1.view.xml file](1.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Add controller logic)]
Open the `mta_app/app/webapp/controller/View1.controller.js` file. You will add an event handler function for the press event and import a new library to the header.

```JavaScript
sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageBox"
], function (Controller, MessageBox) {
	"use strict";

	return Controller.extend("sapcp.cf.tutorial.app.controller.View1", {
		onInit: function () {

		},

		// show in a pop-up which list element was pressed
		handleListItemPress: function (oEvent) {
			MessageBox.show(
				"You pressed item: " + oEvent.getSource().getBindingContext(), {
					icon: sap.m.MessageBox.Icon.INFORMATION,
					title: "It works!",
					actions: [sap.m.MessageBox.Action.OK]
				}
			);
		}
	});
});
```

![controller](2.png)
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Re-run your app)]
Now **Run** your application again.  

![rerun](3.png)

> This time the deploy job should be way faster.

![Running application with list view](4.png)
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Re-run your app)]
Click on an item in your application, and a pop-up will appear.  

Notice that the pop-up displays the number of the item pressed, in parenthesis (in the image below: `Products(7)`).

![Alert pop-up](5.png)

[VALIDATE_1]
[ACCORDION-END]
---

### Additional Reading
- [Data Binding](https://sapui5.netweaver.ondemand.com/#docs/guide/68b9644a253741e8a4b9e4279a35c247.html)- [`<List>`](https://sapui5.netweaver.ondemand.com/#docs/guide/295e44b2d0144318bcb7bdd56bfa5189.html)- [`<StandardListItem>`](https://sapui5.netweaver.ondemand.com/explored.html#/entity/sap.m.StandardListItem/properties)
- [`sap.m.MessageBox`](https://sapui5.netweaver.ondemand.com/sdk/#docs/api/symbols/sap.m.MessageBox.html)
