---
title: Round the currency field using JavaScript
description: Learn how to round numbers received from the OData source using JavaScript.
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, tutorial>beginner ]
---

## Prerequisites
- **Proficiency:** Beginner
- **Tutorials:** [Add labels and a new field to your app](http://www.sap.com/developer/tutorials/hcp-webide-add-labels-field.html)

## Next Steps
- [Insert a currency symbol for display](http://www.sap.com/developer/tutorials/hcp-webide-insert-currency-symbol.html)

## Details

### You will learn
The OData service provides the Unit Price field with with four decimal places. For most currencies, this can be rounded to two decimal places (or even to whole integers) depending on the users' needs. In this tutorial you will learn how to round numbers in JavaScript and where to insert a function to do the rounding in your app.

![fields without labels](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-round-currency/mob2-2_0.png)

To explore a bit more of SAP Web IDE, you will use the Web IDE search feature to identify which file to edit.

You will also learn how to add a field to your app. This is useful since the template can quickly generate a working app for you, but you are not restricted to what it generates.

### Time to Complete
**5 min**

---


[ACCORDION-BEGIN [Step 1: ](Changing type of Unit Price)]

The `UnitPrice` field is defined as a decimal number, with "scale" of 4 in the [OData service](http://services.odata.org/V2/Northwind/Northwind.svc/$metadata)

![View of OData model](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-round-currency/mob2-2_1.png)


In JavaScript, the best way to round the number for display is to convert the string to a number, then use the `toFixed(NUM)` method to round to the specified number of decimal places. See the example below (there's no need to copy the code).

```javascript
product.UnitPrice = Number(product.UnitPrice).toFixed(NUM)
```

Now that you know how to round the number, the trick is to figure out where to use this approach.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Find where to make changes)]

To make the change so it affects the master and detail views, you want to make the change as far "upstream" as possible, which is in the `_setModel` function of `Component.js`.

![Beginning of `_setModel` function](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-round-currency/mob2-2_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create type conversion function)]

You will do this by adding an `attachRequestCompleted()` function near the top of the `_setModel` function as shown in the image below.

The code to insert:

```javascript
// Reformat UnitPrice string to show only two decimal places
oModel.attachRequestCompleted(function()
{
    var NUM_DECIMAL_PLACES = 2;

    //Get all objects
    var products = this.getModel().mContexts;

    // loop through the objects and round to NUM_DECIMAL_PLACE
    for (var i in products)
    {
        var product = products[i].getObject();
        product.UnitPrice = Number(product.UnitPrice).toFixed(NUM_DECIMAL_PLACES);
    }
    this.getModel().updateBindings();
}, this);
```
>Note that if you want to round to an integer, you can just change  `var NUM_DECIMAL_PLACES = 0;` (and update the comment).

![Inserted code](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-round-currency/mob2-2_4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Save and run)]

Save your edits, click on `index.html` and click the **Run** button to test this change. Your app should look like this:

![Modified app](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-round-currency/mob2-2_5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Clear the cache)]

If the numbers are not rounded, remember to clear the cache as described at the bottom of the [Add labels and a new field to your app](http://www.sap.com/developer/tutorials/hcp-webide-add-labels-field.html) tutorial.

[DONE]
[ACCORDION-END]



## Next Steps:
- [Insert a currency symbol for display](http://www.sap.com/developer/tutorials/hcp-webide-insert-currency-symbol.html)
