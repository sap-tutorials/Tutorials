---
title: Display Fetched Data in your AppGyver Application
description: In this tutorial, you will learn how to display data fetched from a public API, such as product names and calorific information, in your AppGyver application.
auto_validation: true
time: 10
tags: [ tutorial>beginner, tutorial>license, topic>mobile]
primary_tag: products>sap-business-technology-platform
author_name: Tom Beck
author_profile: https://github.com/heytombeck
---

## Prerequisites
- Access to an SAP BTP account in EU10 with Low-Code / No-Code entitlements
- Previously followed the steps provided in [Fetch Data from Public API to your AppGyver Application](appgyver-fetch-data)
- Access to the AppGyver Previewer App on a smart phone or tablet: [iOS](https://itunes.apple.com/us/app/appgyver/id1311492157) / [Android](https://play.google.com/store/apps/details?id=com.appgyver.agclient)

## Details
### You will learn
  - How to display data fetched from a public API within an AppGyver application once a request has been triggered.

In the previous tutorial, you learned how to configure your application to read specific information from an API once a barcode has been scanned. Now, in this final tutorial for this mission, you will learn how to display the fetched information on your application interface.

---

[ACCORDION-BEGIN [Step 1: ](Add Text Components to your App)]

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

[ACCORDION-BEGIN [Step 2: ](Display Product Name)]

This paragraph now needs to be bound to the data variable you configured in earlier tutorials. This will allow you to define what food information to display once it has been fetched from the API, as you may not want to display all information available for each food item.

For this example, we will show you how to display the Product Name and Energy-kcals_100g.

To display the product name, click your **Paragraph** component and then click **Currently bound to: Static text**.

![Click to bind paragraph](bind_paragraph.png)

Select **Data and Variables**.

![Select data and variables](data_variables.png)

Select **Data Variables** and then click your ***Open food facts*** variable.

Scroll down to and select the `product_name` field. For this, we suggest using the 'Find text' function within your browser.

![Product name variable](product_name.png)

Then click **Save**.

![Save paragraph](save_paragraph.png)

The paragraph will now display the product name of the scanned food item.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Display Calorific Information)]

We'll now add the calorific information to your app, using the same steps provided in for the product name information. Repeat Step 1 and Step 2, adding a new paragraph component to your interface.

Click your **Paragraph** component and then click **Currently bound to: Static text**.

![Click to bind paragraph](bind_secondpara.png)

Select **Data and Variables**.

![Select data and variables](data_variables.png)

Select **Data Variables** and then click your ***Open food facts*** variable.

Scroll down to and select the `energy` field. For this, we suggest using the 'Find text' function within your browser.

Then click **Save**.

![Add calorific information](add_calories.png)

The paragraph will now display the calorific information of the scanned food item.

Should you wish, you can now continue to add other information from the API using the same steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Save and Test)]

Now click **Save**.

![Save application](save_application.png)

Your draft application is now complete, allowing you to scan a food item and then read the product name and calorific information per 100g. To do this, open your ***AppGyver Preview*** app and test the scan feature.

In this example, we are scanning some confectionery:

![Scan chocolate](appgyver_scan_chocolate.png)

With the app returning the product name and the calorific information:

![Scan results](appgyver_scan_result.png)

[VALIDATE_4]
[ACCORDION-END]
