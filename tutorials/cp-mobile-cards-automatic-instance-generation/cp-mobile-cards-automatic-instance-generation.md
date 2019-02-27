---
title: Create Cards Automatically With Automatic Instance Generation
description: Learn how to create more advanced cards and send them to your SAP Mobile Cards application.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards ]
time: 20
---

## Prerequisites

- **Tutorials:** [Create a Welcome Card](cp-mobile-cards-welcome)

## Details

### You will learn

 - How to implement a mobile Card of Automatic Instance Generation type.

>Ensure you have completed the prerequisites for this tutorial before continuing.

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A company wants to show the last three pay slips to your employee on the mobile device. The query for this solution would ask for the top 3 pay slips sorted by date. With the Automatic Instance Card, this would show three cards with pay slip information. Each card would be one of the last three months (April, May, June). As soon as the result set changes (when a new pay slip is available), the cards will be updated. The new month plus the last two months would show (May, June, July).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Automatic Instance Card)]

Make sure you have logged in to SAP Cloud Platform Mobile Services cockpit. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_000.png)

Click on  create icon to create a new card.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_001.png)

Provide the required information as per below.

| Field | Value |
|----|----|
| Name | `AutomaticInstanceCard` |
| Destination | `SAPCPMobileServices` |
| Template Source | choose `Template Manager` |
| HTML Template | choose `Sample Sales Order Template` |

> If you see a pop-up, click OK for the confirmation.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_010.png)

> **Destination** defines the root for the queries which is going to be used for this card.

>The Type **Automatic Instance Generation** will create a card instance for each result in the result set returned by the query. The Query is based on the destination. This query returns a result set of three sales orders from the sample services.

Click **Sample Data** to view the sample JSON response. The JSON response here represents one result set from the three cards the query would return. This Data is used by the Data Mapper to render a sample card in the editor.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_012.png)

Click **Editor** to view the **HTML** which builds this card. The **HTML** describes the layout of the card. The [Handlebars](https://handlebarsjs.com/) `{{}}` are the placeholders where the Data Mapper will but the actual JSON response values during runtime.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_014.png)

Click **Data Mapping** to view the Mapping between the HTML which contains handlebars and the actual JSON data. The **Data Mapper** allows now the mapping of the Handlebar placeholders to the actual JSON entities. The actual result shows in the card to the right.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_016.png)

Click **Save** to save the card and make it available for the Mobile Cards application.

This is a pop-up asking you whether you want to lock this mobile card or not. If you select **YES**, this means when you need to do any modification to this card, you need to unlock it first. For this tutorial, click on **NO**.

![SAP Cloud Platform Mobile Services - Opera](Markdown_files/img_018.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Download the card on Mobile Cards client)]

To ensure that the client refreshes, pull down the Welcome card to trigger a manual refresh and then click on **+** icon to subscribe a new card.

![Chrome Remote Desktop](Markdown_files/img_020.png)

Click on `AutomaticInstanceCard` under **All** tab.

![ Chrome Remote Desktop](Markdown_files/img_022.png)

Click **Subscribe** to trigger the download of the Automatic Instance Cards in your Mobile Cards app.

![ Chrome Remote Desktop](Markdown_files/img_023.png)

Click on a card to open it.

![ Chrome Remote Desktop](Markdown_files/img_024.png)

Congratulations, you have built a query based card which creates an instance of a card for each query result set.

![ Chrome Remote Desktop](Markdown_files/img_025.png)

Here you can view respective Sales Order Items details by clicking on 3 dots at right hand bottom side.

![Chrome Remote Desktop](Markdown_files/img_026.png)

[DONE]
[ACCORDION-END]
