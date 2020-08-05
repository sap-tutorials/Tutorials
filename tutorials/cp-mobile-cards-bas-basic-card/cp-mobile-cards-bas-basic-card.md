---
title: Create Your First Card in Business Application Studio
description: Configure SAP Mobile Cards Development for your Business Application and create your first Basic Card.
auto_validation: true
time: 30
tags: [ tutorial>beginner, topic>mobile, operating-system>ios, operating-system>android, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services, products>sap-business-application-studio ]
primary_tag: products>sap-mobile-cards
---

## Prerequisites
 - [You have set up your SAP Cloud Platform Mobile Services account](https://developers.sap.com/tutorials/cp-mobile-cards-setup.html)
 - You have set up your Business Application Studio for Mobile Technologies

## Details
### You will learn
  - Create a card using a template in Business Application Studio. [Click here to learn more about SAP Mobile Cards in SAP Business Application Studio.](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-2-2-development-bas.html)
  - Design and develop SAP Mobile Cards in Business Application Studio
  - Deploy a card to SAP Cloud Platform Mobile Services from Business Application Studio
---

[ACCORDION-BEGIN [Step 1: ](Create Mobile Service Connection)]

Open Business Application Studio and enter your dev workspace.

!![Business Application Studio Dashboard](img_1_1.png)

In the menu bar, go to View &rarr; Find Command, click **Find Command**.

!![Open Command Pallete](img_1_2.png)

> For faster development, setup a shortcut key for Find Command and use it.

Type Mobile Cards and select **Mobile Cards: Create Service Connection**.

!![Service Connection Option](img_1_3.png)

Enter a name for your Mobile Services connection; e.g. `cf-ms-trial`

!![Service Connection Name](img_1_4.png)

Enter the Admin API of your Mobile Services Cockpit.

!![Service Connection API](img_1_5.png)

> You can find the Admin API in the Important Links section of your Mobile Services cockpit.
  !![CPMS Important Links](img_1_6.png)

Upon successful connection, you will see a toast message at the bottom right corner of your screen.

!![Sucess toast message](img_1_7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a New Card)]

Open Find Command, search for `mobile cards` and select **Mobile Cards: New**.

!![Command for New Card](img_2_1.png)

> You may be prompted to enter your username & password. If so, please provide the details and proceed.

Select **Basic Mobile Card Template**.

!![Template list](img_2_2.png)

Enter a name for the card; e.g. `Welcome BAS`.

!![Enter a name for the card](img_2_3.png)

In your File Explorer, expand `Welcome BAS` Folder.

!![Card Structure](img_2_4.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Modify the Card)]

In your File Explorer, click **metadata.json**.

!![Metadata.json View](img_3_1.png)

Add a description; e.g. `Created in SAP Business Application Studio`.

!![Description View](img_3_2.png)

In your File Explorer, click **template_en.html**.

!![HTML File view](img_3_3.png)

> If you see a confirmation dialog for `.mckstate`, click Yes.
  !![MCKState dialog](img_3_4.png)

Open Find Command, search for `mobile cards` and select **Mobile Cards: Preview**.

!![Command Pallete Preview](img_3_5.png)

A preview window for your card will open to the side.

!![Preview Window](img_3_6.png)

Collapse `<div class="card-content">` and replace the enclosing code inclusive of the div tags with the following code and save the file.

```html
<div class="card-content">
  This is my first card in Business Application Studio.
</div>
```

!![Updated HTML File](img_3_7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy the Card from SAP Business Application Studio)]

Open Find Command, search for `mobile cards` and select **Mobile Cards: Deploy**.

!![Card Structure](img_4_1.png)

Select the card you have created; e.g. `Welcome BAS`.

!![Card Structure](img_4_2.png)

Upon successful deployment, you will see a toast message at the bottom right corner of your screen.

!![Sucess toast message](img_4_3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Publish the Card in Mobile Service Cockpit)]

Open your Mobile Services Cockpit.

!![CPMS Cockpit](img_5_1.png)

Click **SAP Mobile Cards**.

!![Mobile Cards Cockpit View](img_5_2.png)

Click on your card; e.g. `Welcome BAS`.

!![Cards List](img_5_3.png)

In the Versions table, click the ![Publish Icon](ico_check.png) icon to change the state to **Productive**.

!![Card in development status](img_5_4.png)

Choose **Yes** to confirm.

!![Publish confirmation dialog](img_5_5.png)

The status of the card should now be **Productive**.

!![Card in productive status](img_5_6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View the Card on Your Device)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

[OPTION BEGIN [Android]]

In the SAP Mobile Cards Android client, tap **+** | **Subscriptions**.

!![Android: App Home](img_6_1.png)

Tap **Welcome BAS** under the **All Subscriptions** tab.

!![Android: All Subscriptions](img_6_2.png)

Tap **Subscribe** to trigger the download of the card in your SAP Mobile Cards app.

!![Android: Card Detail View](img_6_3.png)

> Notice the description.

Tap ![Android: Back Icon](ico_android_back.png) 'Back' to to see latest cards downloaded on the device.

!![Android: Card View](img_6_4.png)

[OPTION END]

[OPTION BEGIN [iOS]]

In the SAP Mobile Cards iOS client, tap **More** &rarr; **Subscriptions**.

!![iOS: More View](img_6_5.png)

Tap **All** tab &rarr; **Welcome BAS**.

!![iOS: All Subscriptions](img_6_6.png)

Tap **Subscribe** to trigger the download of the card in your SAP Mobile Cards app.

!![iOS: Card Detail View](img_6_7.png)

Tap **New Cards** to to see latest cards downloaded on the device.

![MobileCardsImage](img_6_8.png)

[OPTION END]

[DONE]
[ACCORDION-END]

**Congratulations!** You have completed this tutorial.

You have created your first card through SAP Business Application Studio.

---
