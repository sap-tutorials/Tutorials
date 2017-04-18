---
title: Calculate and display a new field in an SAPUI5 app
description: Compute a new field from data in the OData service and display it in your app.
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, tutorial>intermediate ]
---

## Prerequisites
- **Proficiency:** Intermediate
- **Tutorials:** [Add a tab and additional fields to an SAPUI5 app](http://www.sap.com/developer/tutorials/hcp-webide-add-tab.html)

## Next Steps
- [Commit your project files to your SAP Cloud Platform Git repository](http://www.sap.com/developer/tutorials/hcp-webide-commit-git.html)

## Details

### You will learn
There are times when a use case calls for the generation of a data field that is not available from the back-end system. One example of this would be calculation of a customized risk value for a sales opportunity and mapping the result to a high, medium or low category.

In this tutorial, you will learn how to generate a new field from data received from the OData service and display it on the detail view. You will also become familiar with a few of the features in the Google Chrome debugger.

The field you will generate is an approximated "Current Inventory Value" based on inventory (Units in Stock), Unit Price and a fixed markup percentage. Typically, the markup percentage and unit price would vary by product, region and other factors – but the intent here is simply to show how a field can be generated and displayed.

For this tutorial, you can assume:
* The cost of each item is equal to: `UnitPrice / (1+markup)`
* The markup is 0.65 (65%)
* The Current Inventory Value is: `(UnitPrice – cost) * UnitsInStock`

The field is calculated each time the detail view is displayed and is not persisted. Your app will look like the image below when complete.

![Calculated Field Example](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_0.png)

### Time to Complete
**10-15 min**

---


[ACCORDION-BEGIN [Step 1: ](Log into your SAP Cloud Platform account)]

Log into your SAP Cloud Platform account and open SAP Web IDE in a Google Chrome browser.

Open the **northwind** project folder and then the **view** folder. Double-click on `Detail.controller.js`, and scroll down to the `bindView` function.

The purpose of the `bindView` function is to associate data fields with the view components and is called when the detail view is updated. Since it is called each time a list item is selected, it is the perfect place to compute the current inventory value.

![original bindview function](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Pass data into detail view)]

The first step is to add some code to get the data passed in for the detail view. To do this, you will create a local variable (`record`) and make a call on the `oView` object to get the model and data (passing in the entity path). You will also insert the `debugger;` command to pause execution so you can review the data available to you in `record`.

Insert the following lines just after `oView.bindElement(sEntityPath);`, Beautify the code and save your edits.

Your file should look like the image below, and you can ignore the warning and error alerts from the code checker.

```javascript

// ******* BEGIN INSERTED CODE TO COMPUTE CURRENT INVENTORY VALUE *******       
var record = oView.getModel().getData(sEntityPath);
debugger;

// ******* END INSERTED CODE *******
```
![bindview with debugger](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run with debugging)]

Select `index.html` and run your app. In the preview tab, open Developer Tools (F12 or Option-Command-I), then select one of the items in the master list.

You will see that execution has stopped at the debugger statement (1), and if you scroll down below the Call Stack, you will see the Scope section (2). Expand the record variable (3) and you will see the data made available by line 83 in your code.

With the debugger paused, you will not be able to edit the code in Web IDE. To get it running, click one of the Resume script execution button (4), and switch back to Web IDE.
![execution halted at debugger statement](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Compute Current Inventory Value)]

The next step is to compute the Current Inventory Value field, round the number, and then update the model/UI bindings.

Insert the following lines just after the debugger; statement (line 84), Beautify the code and save your edits.

```javascript
var NUM_DECIMAL_PLACES = 2;
var MARKUP = 0.65; //65%
record.inventoryValue = (Number(record.UnitPrice) - (Number(record.UnitPrice) / (1+MARKUP))) * Number(record.UnitsInStock);
record.inventoryValue = Number(record.inventoryValue).toFixed(NUM_DECIMAL_PLACES);
oView.getModel().updateBindings();
```
Add some blank lines so that your line numbers are the same as the image below. This will allow you to follow along with the explanations. You can ignore the error alert from the code checker.

The next few steps walk you through what the code does.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add constants)]

The first two lines add some constants used in the calculation:

* Line 86: A constant for the `toFixed()` method – specifying the number of decimal places
* Line 88: A constant for the mark up percentage that will be used to calculate the unit cost

![constants](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Perform calculations)]

Line 90 does the real work:

- A new field name (`inventoryValue`) is created in record, and will be used to display the value in the details view
- The unit cost is calculated and
- is subtracted from the unit price
- then the result (the profit for each unit) is multiplied by the number of units in stock

![calculating inventory value](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Round result and update bindings)]

The result is rounded to two decimal places (1) using the same approach used when rounding the Unit Price in an earlier tutorial. Lastly, a call is made (2) to update the bindings which includes the new field.

![rounding](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add label for new field)]

To add a label for the new field, open the **`i18n > messageBundle.properties`** file and add a new line for the inventory value label:

```xml
label_CurrentInventoryValue=Current Inventory Value
```
![messageBundle.properties file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Add attribute and save)]

Open the **view > `Detail.view.xml`** file, add the following XML to the `ObjectHeader`, Beautify and save your change.

```xml
<ObjectAttribute text="{i18n>currencySymbol}{inventoryValue}" title="{i18n>label_CurrentInventoryValue}"/>
```
This line adds the label and currency symbol from the `messageBundle.properties` file along with the computed `inventoryValue` field.

![Detail.view.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Reload page)]

With all changes saved, switch back to your preview tab (with Developer Tools open), and reload the page by right-clicking on the browser refresh button and selecting Empty Cache and Hard Reload.  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Step through debugger)]

When the app loads, it will pause on the debugger statement again. Click on the **Step Over** button to advance the debugger line by line, and watch the **Scope** pane for variable values as the execution proceeds.

![Detail.view.xml file](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_11.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Check record variable)]

With execution stopped at line 92, you can see that `inventoryValue` now appears in the record variable (with an non-rounded value).

![computed but unrounded](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_12.png)

Click the **Step Over** button again and the value is now rounded.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Resume execution)]

Click the **Resume script execution** button to allow the execution to continue. If you look in the detail view header, you will see the Current Inventory Value field displayed.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Clean up the code)]

For clean-up, you can remove the `debugger;` statement in `Detail.controller.js` along with the blank lines, save your changes and re-deploy to SAP Cloud Platform.  

![cleaned up function](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-calculate-new-field/mob3-2_14.png)

[DONE]
[ACCORDION-END]



## Next Steps
- [Commit your project files to your SAP Cloud Platform Git repository](http://www.sap.com/developer/tutorials/hcp-webide-commit-git.html)
