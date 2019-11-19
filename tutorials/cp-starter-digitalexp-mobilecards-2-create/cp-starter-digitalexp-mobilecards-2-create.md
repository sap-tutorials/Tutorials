---
title: Create SAP SuccessFactors To Dos and Timesheet Card
description: Create a To Do and a timesheet card displaying data from SAP SuccessFactors.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services ]
time: 20
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
**Install SAP Mobile Cards Application:** Download and install on your [iPhone](https://itunes.apple.com/us/app/sap-content-to-go/id1168110623?mt=8) or [Android](https://play.google.com/store/apps/details?id=com.sap.content2go)

## Details
### You will learn
  - How to create a SAP mobile card in the Mobile Services cockpit
  - How to subscribe to cards on your mobile device

Continuing from the destination you created in the previous tutorial, you will create two cards:

  - `**To Do card:**` A collection of cards that captures tasks in a To-do list -- each task has a card of its own

  - `**Timesheet card:**` A card that shows a user's Time Sheet log for a given date

---

[ACCORDION-BEGIN [Step 1: ](Create To Do card)]

Enter **Name** as `ToDosCard` and click **Save**.

![MobileCards](img_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Timesheet card)]

Choose **Create a New Card** to display a user's Time Sheet log for a given date.

![MobileCards](img_4.png)

Provide the following values for the new card:

| Field | Value |
|----|----|
| **Name** | `TimeSheetCard` |
| **Destination** | `SFDEMO_APIHUB` |
| **Template Source** | `Template Manager` |
| **HTML Template** | `SuccessFactors My TimeSheet` |

> If you see a pop-up, choose **OK**.

End result should like below, choose **Save**.

![MobileCards](img_5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set card status to productive)]

By default, status of newly created cards is **Development**. Cards can be subscribed to only when they are in a **Productive** state.

To change the card status, click on `TimeSheetCard`.

![MobileCards](img_5.1.png)

Under **Actions**, click on highlighted icon to change the state to **Productive**.

![MobileCards](img_5.2.png)

Choose **Yes** to confirm.

![MobileCards](img_5.3.png)

You will notice that the **State** has been changed to **Productive**.

![MobileCards](img_5.4.png)

Repeat the above step for the `ToDosCard`. Status of both the cards should be **Productive**.

![MobileCards](img_5.5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure SAP Mobile Cards client)]

>Make sure you are choosing the right device platform tab **above**.

>Make sure you have installed the SAP Mobile Cards app on your phone.

You can now start onboarding your SAP Mobile Cards client onto SAP Cloud Platform.

Choose the **APIs** tab to view the registration QR code. Scan the QR code applicable to your device type (Android or iOS).

![MobileCards](img_101.png)

[OPTION BEGIN [Android]]

Tap **SCAN QR CODE** to begin the on-boarding process.

![MobileCards](img_301.png)

>**Note**: In CF trial accounts, all applications are stopped every night for saving resources. This is by design.

>If you see some trouble during on-boarding process, make sure that Mobile Services app state is in **Started** mode. In SAP Cloud Platform cockpit, navigate to subaccount > Spaces> Applications
>![MobileCards](img_102.png)

Tap **I AGREE** on `End User License Agreement`.

![MobileCards](img_302.png)

Enter your SAP Cloud Platform credentials, and choose **Log On**.

![MobileCards](img_303.png)

Choose a passcode with at least 8 characters to unlock the app, and tap **NEXT**.

![MobileCards](img_304.png)

Confirm the passcode and tap **DONE**.

![MobileCards](img_305.png)

Tap the **+** icon to add subscriptions.

![MobileCards](img_307.png)

Tap **Subscriptions**.

![MobileCards](img_308.png)

Tap `TimeSheetCard` to subscribe this card.

![MobileCards](img_308.1.png)

Tap **SUBSCRIBE**.

![MobileCards](img_309.png)

Tap on arrow key to navigate back to **All Subscriptions** list.

![MobileCards](img_311.png)

Tap `ToDosCard` to subscribe it.

![MobileCards](img_312.png)

Tap **SUBSCRIBE**.

![MobileCards](img_313.png)

You can now view both the cards.

![MobileCards](img_11.gif)

[OPTION END]

[OPTION BEGIN [iOS]]

Scan the QR code with your camera and select the toast message to launch the SAP Mobile Cards app.

![MobileCards](img_017.png)

>**Note**: In CF trial accounts, all applications are stopped every night for saving resources. This is by design.

>If you see some trouble during on-boarding process, make sure that Mobile Services app state is in **Started** mode. In SAP Cloud Platform cockpit, navigate to subaccount > Spaces> Applications
>![MobileCards](img_102.png)

Enter your SAP Cloud Platform credentials and choose **Log On**.

![MobileCards](IMG_1.1.PNG)

Choose a passcode with at least 8 characters to unlock the app.

![MobileCards](IMG_1.2.PNG)

Confirm the passcode and choose **Done**.

![MobileCards](IMG_1.3.PNG)

Tap **More** to add subscriptions.

![MobileCards](IMG_1.5.PNG)

Choose **Subscriptions**.

![MobileCards](IMG_1.6.PNG)

Select **All** and then choose `TimeSheetCard`.

![MobileCards](IMG_1.7.PNG)

Tap **Subscribe** to subscribe the timesheet card.

![MobileCards](IMG_1.8.PNG)

Repeat steps above for the `ToDosCard` and choose **Subscribe**.

![MobileCards](IMG_1.9.PNG)

Tap the **All Cards** at bottom of your screen to see all your currently subscribed cards.

![MobileCards](IMG_1.10.PNG)
![MobileCards](IMG_1.11.PNG)

[OPTION END]

Congratulations, you have now deployed two cards with data from SAP SuccessFactors.

[VALIDATE_1]
[ACCORDION-END]

---
