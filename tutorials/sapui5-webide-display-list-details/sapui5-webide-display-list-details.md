---
title: SAPUI5 - Display List Element Details
description: Add details to a list, making it a multi-line list element
tags: [  tutorial>beginner, topic>html5, topic>sapui5, products>sap-hana-cloud-platform ]
---
## Prerequisites  
- **Proficiency:** Beginner 
- **How-To** [Start this tutorial series](http://www.sap.com/developer/tutorials/sapui5-webide-open-webide.html)
- **Tutorials:** This tutorial is part of a series.  The previous tutorial is part 5: [Enable Routing in your Application](http://www.sap.com/developer/tutorials/sapui5-webide-enable-routing.html)

## Next Steps
 - Part 7 is next: [Update Internationalization](http://www.sap.com/developer/tutorials/sapui5-webide-update-internationalization.html)

## Details
### You will learn  
In order to improve our web app, you can display a more detail information for each sales order/list item. In this tutorial, you will create a new view and controller for a detail screen. The detail screen will receive the ID of the list item and will access the data model in order to get the requested data.

You will also add a Detail view and controller as well as alter the `View1` view and controller.  

### Time to Complete
**10-15 Minutes**.

---
>  **Web IDE** If you don't have the Web IDE open, follow these steps: [Enable and open the HANA Cloud Platform Web IDE](http://www.sap.com/developer/tutorials/sapui5-webide-open-webide.html)


1.  Open the `webapp/view/View1.view.xml` file, and change the `<StandardListItem type="Active">` tag to read:

    ```xml
    type="Navigation"
    ```
    
    ![Change the type to Navigation](1.png)
    
2.  Open the `webapp/view/View1.controller.js` file, and modify the `handleListItemPress` event to the `Controller.extend` method:

    ```javascript
    handleListItemPress: function (evt) {
	 	var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
	 	var selectedProductId = evt.getSource().getBindingContext().getProperty("ProductID");
    	oRouter.navTo("detail", {
    		productId: selectedProductId
    	});
    }
    ```
    
    ![change handleListItemPress](2.png)
  
  
3.  Next, we will add another new SAPUI5 view.  Right click on the **`webapp`** folder, then select **New > SAPUI5 View**.

    ![Create new SAPUI5 view](3.png)
  
4.  In the *View Name* field, type in `Detail`.  Click **Next**, then on the next screen click **Finish**.

    ![Set the name of the new view to Detail](4.png)
  
5.  Open the new file`webapp/view/Detail.view.xml`.  In the file, replace the code with the following:
    
    ```xml
    <mvc:View controllerName="HelloWorld.controller.Detail"
              xmlns="sap.m"
              xmlns:mvc="sap.ui.core.mvc" >
    	<Page title="{i18n>DetailTitle}"
    	      showNavButton="true"
    	      navButtonPress="handleNavButtonPress" >
    		<VBox>
    			<Text text="{ProductName}" />
    			<Text text="{UnitPrice}" />
    			<Text text="{QuantityPerUnit}" />
    			<Text text="{UnitsInStock}" />
    		</VBox>
    	</Page>
    </mvc:View>
    ```
    
    ![Add the code to Detail.view.xml](5.png)


6.  Open the file `webapp/controller/Detail.controller.js`, and replace the code with the following:  
    
    ```Javascript
    sap.ui.define([
    	"sap/ui/core/mvc/Controller"
    ], function(Controller) {
    	"use strict";
    	return Controller.extend("HelloWorld.controller.Detail", {
    		onInit: function () {
    			var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
    			oRouter.getRoute("detail").attachMatched(this._onRouteMatched, this);
    		},
    		_onRouteMatched : function (oEvent) {
    			var oArgs, oView;
    			oArgs = oEvent.getParameter("arguments");
    			oView = this.getView();
    			oView.bindElement({
    				path : "/Products(" + oArgs.productId + ")",
    				events : {
    					dataRequested: function () {
    						oView.setBusy(true);
    					},
    					dataReceived: function () {
    						oView.setBusy(false);
    					}
    				}
    			});
    		},
    		handleNavButtonPress : function (evt) {
    			var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
    			oRouter.navTo("home");
    		}
    	});
    });
    ```
    
    ![Add code to Detail.controller.js](6.png)
   
   
7.  Open the `webapp/manifest.json` file.  Select the *Routing* tab, and scroll down to the *Manage Targets* section.

    Click on the **+** icon to create a new target.
   
   ![Create a new target](7.png)
   
8.  Create a new target called `detail`, and click **OK**

    ![Name the target](8.png)
    
9.  Select the new `detail` target, and change the following values.  (Don't forget to save your file!)

    |           |           |
    |----------:|---------- |
    |View Name  |Detail     |
    |View Level |2          |
    
    ![Update the new target information](9.png)
    
10. Scroll up to the *Routes* section, and then click the **+** button to create a new route.  Fill in the following information:

    |           |                   |
    |----------:|------------------ |
    |Name       |detail             |
    |Pattern    |`detail/{productId}` |
    |Greedy     |(off)              |
    |Targets    |detail             |
    
    ![Set up the new Detail route](10.png)
    
11. In the `webapp/mainfest.json` file, switch to the **Code Editor** tab (at the bottom of the screen).  

    Find the section under `sap.ui5` called `routing`.  in the `config` area, add the following text:
    
    ```xml
    "routerClass": "sap.m.routing.Router",
    ```
    
    ![Define the routing system](11.png)
      


    > **Additional Information**
    
    > As you are focusing on the separation of MVC, the definition of views with XML has a key benefit: You can't get confused and mix up business logic (usually inside the controller) with the view implementation because XML doesn't allow you to define methods to be executed. It requires you to refer to a method, which should be defined in the controller. Another aspect is that XML appears to be easier to read and write, as it's a markup language. The SAPUI5 community prefers this type of view definition and in fact, most of the code samples in the documentation are using XML.

    > Inside of the page declaration, you are making use of the `<VBox>` UI component. `<VBox>` is a vertical aligned `<Flexbox>` element. It's essentially a way to arrange all the elements inside of the `<VBox>` in a vertical order. You can imagine it as rows in a table. Every UI element will be placed below the earlier ones. In our case, we will place four "Text" elements in a vertical order.
    
    > Another important aspect is the declaration of "showNavButton" and the related "navButtonPress" event. While the first one enables the visibility of a "back" button, the latter one defines the function to be executed when this button is pressed. In our case, we assign the function "handleNavButtonPress" as event handler. This function will be implemented in the detail controller.




    > You will implement one function to handle the back button press event. This method will use the "navTo" method of the router and initiate the navigation via the "home" route. The "onInit" method is more complex. The method expects a navigation parameter for the ProductId. It hooks into the "detail" route and executes the "_onRouteMatched" method to extract the ProductId and set the data binding for the Detail view.
    
12.  Run your application!  When you click on a row, the screen should slide sideways to show the detail view.  To return to the list, click the back arrow (in the upper left corner).
    
    ![Current application - list view](12a.png)
    
    ![Current application - detail view](12b.png)

## Next Steps
 - Part 7 is next: [Update Internationalization](http://www.sap.com/developer/tutorials/sapui5-webide-update-internationalization.html)

## Additional Reading
- [Routing with mandatory parameters](http://help.sap.com/saphelp_nw75/helpdata/en/f9/6d2522a5ca4382a274ae3c6d002ca0/content.htm)
- [`<VBox>`](https://sapui5.hana.ondemand.com/docs/api/symbols/sap.m.VBox.html)


