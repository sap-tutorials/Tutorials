---
title: Create Handlebars Helper Card
description: Enhance the visual representation of a card by implementing a logic.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-cloud-platform-mobile-services, products>sap-mobile-cards ]
time: 20
---

## Details
### You will learn
- How to implement a logic in a card

Handlebars helper help developer to use JavaScript logic in rendering of the card.

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A Company wants managers to have a set of `KPIs` available on a mobile device. In order to simplify the card, a sum of sales orders is represented in certain Colors. For example, if the sum is larger than 1000 Euros then the sales order's status is visualized and highlighted with traffic light symbols.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new card)]

Make sure you have logged into the SAP Cloud Platform Mobile Services cockpit. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_000.png)

Click the **Create a New Card** icon.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_001.png)

Provide the required information:

| Field | Value |
|----|----|
| **Name** | `HandlebarHelperCard` |
| **Destination** | `SAPCPMobileServices` |
| **Template Source** | `Template Manager` |
| **HTML Template** | `Sample Sales Orders Template` |
| **Card Template** | `Default` |

> If you see a pop-up, click **OK** for the confirmation.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_007.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Insert JSON keys into handlebars)]

Click **Editor** to view the **HTML** which builds this card and to add handlebars classes.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_009.png)

Check **Define source without mapping** to define the actual mapping of the data directly in the editor.

>In this template, you need to add a **d.** in front of all [Handlebars](https://handlebarsjs.com) expression.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_010.png)

Replace `{{SalesOrderId}}` with `{{d.SalesOrderId}}`. By doing so, handlebars expression points to the correct JSON path.

```JSON
{{d.SalesOrderId}}
```

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_011.png)

Repeat the same for below handlebars:

| Current | New |
|----|----|
| `{{CreatedAt}}` | `{{d.CreatedAt}}` |
| `{{LifeCycleStatusName}}` | `{{d.LifeCycleStatusName}}` |
| `{{CustomerId}}`| `{{d.CustomerId}}` |

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_014.png)

Replace `{{#each Items}}` with `{{#each d.Items.results}}`  

>[Handlebars block helper](https://handlebarsjs.com/block_helpers.html) function **each** points to the correct JSON path of the items result set.

```JSON
{{#each d.Items.results}}
```
![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_015.png)

Add below Handlebars helper function in front of `{{d.LifeCycleStatusName}}` class.

```JSON
{{lcsHelper d.LifeCycleStatus}}
```
![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_016.png)

You will see a pop-up window alerting on missing helper function. Click **Close** to implement it.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_017.png)

Click **Handle...** to switch to the **Handlebars** tab where missing Handlebars helper function can be implemented.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_018.png)

Copy and paste the following code.

```JavaScript
Handlebars.registerHelper("lcsHelper", function (passedString) {
if (passedString.includes("A")) {
return new Handlebars.SafeString( "<img src='Traffic-light.png' style=' width: 24px; height: 24px;'>");
}
if (passedString.includes("R")) {
return new Handlebars.SafeString("<img src='Traffic-light-1.png' style=' width: 24px; height: 24px;'>");
}
return new Handlebars.SafeString( "<img src='Traffic-light-3.png' style=' width: 24px; height: 24px;'>");
});
```

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_019.png)

>This Handlebars helper function will evaluate the `passedString` which is the `LifeCycleStatus` JSON value. This function will insert an image depending on the status and return the related `HTML` snippet. If the Status is **C** (closed) than the function will not insert any image.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Upload assets to the card)]

Open below links and save images on local machine. These images will be used as assets for this tutorial.

[Orange Traffic light](https://openclipart.org/image/300px/svg_to_png/195672/Traffic-light-3.png)

[Red  Traffic light](https://openclipart.org/image/300px/svg_to_png/195670/Traffic-light-1.png)

[Green Traffic light](https://openclipart.org/image/300px/svg_to_png/195669/Traffic-light.png)

Once done, click **Assets** to upload asset files.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_020.png)

Click the **upload** icon to upload the images.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_022.png)

Navigate to folder where you have saved all three images and upload them one by one.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_023.png)

Make sure you have all three images uploaded.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_024.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Implement logic for gross amount)]

Click the **Editor** tab.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_025.png)

Click **Handle...**.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_026.png)

Copy and paste the following code below the existing function:

```JavaScript
Handlebars.registerHelper("gaCheckHelper", function (grossAmount) {
if (grossAmount > 1000) {
return new Handlebars.SafeString( "<b> <font color=\"GreenYellow\">" + grossAmount + "</b> </font>");
}
return new Handlebars.SafeString( grossAmount);
});
```

>This handlebars helper function will evaluate the  `grossAmount` which is the `grossAmount` JSON value. If the value is above 1000 it will Color it green.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_027.png)

Switch to **HTML** editor.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_029.png)

Replace `{{GrossAmount}}` with `{{gaCheckHelper GrossAmount}}` .

```JSON
{{gaCheckHelper GrossAmount}}
```

>`gaCheckHelper` handlebars helper function is getting the value `GrossAmount` passed in. `GrossAmount` does not need the path as this function is getting called in the Handlebars helper block with `d.Items.results` path.

As a result, the value above 1000 is coloured green.

>If the editor does not refreshed by itself, you can force it to reload by switching tabs. Click `css` and then back on `HTML`.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_030.png)

Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Subscribe to the card)]

In the SAP Mobile Cards application, click the **+** icon to open the subscriptions.

![Chrome Remote Desktop](Markdown_files/img_032.png)

Click `HandlebarsHelperCard` under the **All** tab.

![Chrome Remote Desktop](Markdown_files/img_034.png)

Click **Subscribe** to activate the `HandlebarsHelperCard` subscription.

![Chrome Remote Desktop](Markdown_files/img_035.png)

Click any card to open it.

![Chrome Remote Desktop](Markdown_files/img_036.png)

Here, you can see a preview of the card. Click **Done**.

![Chrome Remote Desktop](Markdown_files/img_037.png)

Congratulations, you have successfully implemented handlebars helper in your card.

>These helpers are only called when the card is updating, this means, if there is no change in data the handlebar functions are not going to be called.

![Chrome Remote Desktop](Markdown_files/img_038.png)

[DONE]
[ACCORDION-END]
