---
title: Display Fetched Data in Your AppGyver Application
description: Display data fetched from a public API, such as product names and calorific information, in your AppGyver application.
auto_validation: true
time: 10
tags: [ tutorial>beginner, tutorial>license, topic>mobile, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-appgyver
author_name: Akseli Virtanen
author_profile: https://github.com/akseliv
---

## Prerequisites
- Access to an SAP BTP account in EU10 with Low-Code / No-Code entitlements
- Previously followed the steps provided in [Fetch Data from Public API to your AppGyver Application](appgyver-fetch-data)
 - Access to the AppGyver Previewer App on a smart phone or tablet: [iOS](https://apps.apple.com/us/app/sap-appgyver-preview/id1585856868) / [macOS](https://downloads.appgyver.com/SAP_AppGyver_preview_v3.4.4.zip)/ [Android](https://play.google.com/store/apps/details?id=com.sap.appgyver.preview.release)

## Details
### You will learn
  - How to display data fetched from a public API within an AppGyver application once a request has been triggered.

In the previous tutorial, you learned how to configure your application to read specific information from an API once a barcode has been scanned. Now, in this final tutorial for this mission, you will learn how to display the fetched information on your application interface.


---

[ACCORDION-BEGIN [Step 1: ](Add text components to app)]

To start with, you'll add further visual information to your app in the form of a title and a paragraph of text. This will eventually be used to display specific calorific information obtained from scanning a food item.

To do this, open your draft application in your AppGyver Composer account, displaying your barcode scanner app.

From the core component panel, drag and drop a **Title** component to underneath your ***Scan*** button in your app.

![Add title component](add_title.png)

Click the **Title** component and change the **Headline Field** to `Product Information`.

![Change title text](change_title.png)

From the core component panel, drag and drop a **Paragraph** component to underneath this title.

![Add a paragraph](add_paragraph.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Display product name)]

This paragraph now needs to be bound to the data variable you configured in earlier tutorials. This will allow you to define what food information to display once it has been fetched from the API, as you may not want to display all information available for each food item.

For this example, we will show you how to display the Product Name and Energy-kcals_100g.

To display the product name, click your **Paragraph** component and then click **Currently bound to: Static text**.

![Click to bind paragraph](bind_paragraph.png)

>**IMPORTANT:** The following provides 2 ways to do this. The first way is the standard way, but for some people this may cause the AppGyver editor to hang (you can click to exit). So we have provided a second way to store the data using a formula.

- Select **Data and Variables**.

    ![Select data and variables](data_variables.png)

    Select **Data Variables** and then click your ***Open food facts*** variable.

    Scroll down to and select the `product_name` field. For this, we suggest using the 'Find text' function within your browser.

    ![Product name variable](product_name.png)

- Instead, you can do the same thing with a formula. Most, if not all, bindings can be done with the UI or manually with a formula.

    Select **Formula**, and then enter for the formula the following:

    ```
    data.OpenFoodFacts1.product.product_name
    ```

Click **Save**.

![Save paragraph](save_paragraph.png)

The paragraph will now display the product name of the scanned food item.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Display calorific information)]

We'll now add the calorific information to your app, using the same steps provided in for the product name information. Repeat Step 1 and Step 2, adding a new paragraph component to your interface.

Click your **Paragraph** component and then click **Currently bound to: Static text**.

![Click to bind paragraph](bind_secondpara.png)

>**IMPORTANT:** The following provides 2 ways to do this. The first way is the standard way, but for some people this may cause the AppGyver editor to hang (you can click to exit). So we have provided a second way to store the data using a formula.

- Select **Data and Variables**.

    ![Select data and variables](data_variables.png)

    Select **Data Variables** and then click your ***Open food facts*** variable.

    Scroll down to and select the `energy` field. For this, we suggest using the 'Find text' function within your browser.

- Instead, you can do the same thing with a formula. Most, if not all, bindings can be done with the UI or manually with a formula.

    Select **Formula**, and then enter for the formula the following:

    ```
    data.OpenFoodFacts1.product.nutriments.energy
    ```

Click **Save**.

![Add calorific information](add_calories.png)

The paragraph will now display the calorific information of the scanned food item.

Should you wish, you can now continue to add other information from the API using the same steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Display image)]

We'll now display the image for the product (if there is one).

1. Add an image component after the calorific text field you added in the previous step.

2. Click the **Source** binding in the **Properties** pane.

    ![Add image countrol](addimage.png)

    Select **Formula**, and then enter for the formula the following:

    ```
    data.OpenFoodFacts1.product.image_front_url
    ```

    Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Save and test)]

Now click **Save**.

![Save application](save_application.png)

Your draft application is now complete, allowing you to scan a food item and then read the product name and calorific information per 100g. To do this, open your ***AppGyver Preview*** app and test the scan feature.

In this example, we are scanning some confectionery:

![Scan](Scan.png)

With the app returning the product name and the calorific information:

![Scan results](ScanDisplay.png)

[VALIDATE_4]
[ACCORDION-END]
