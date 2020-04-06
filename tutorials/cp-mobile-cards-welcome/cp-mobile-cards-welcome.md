---
title: Create A New Welcome Card
description: Create your own welcome card in the SAP Cloud Platform Mobile Services.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services ]
time: 5
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---
## Prerequisites

## Details
### You will learn
- How to create a Welcome card

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

An organization wants to share a welcome message to a new employee. Upon successful authentication in the SAP Mobile Cards app, the new employee sees a welcome card. In this card, the organization shows basic details to help the employee have a great first day. This card is available to the user without the user having to subscribe to the card.

Welcome Cards are Server Managed Cards that are automatically subscribed. When such a card is created, users automatically get instances of the card because they are automatically subscribed to the card.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Welcome card)]
Make sure you have logged into SAP Cloud Platform Mobile Services cockpit. In the cockpit, navigate to **SAP Mobile Cards** to look into the Mobile Cards configuration.

!![MobileCards](img_1.png)

Click the **Create a New Card** icon ![MobileCardsIcon](ico_new_card.png) to create a new card.

!![MobileCards](img_2.png)

Provide the required information as indicated in the table below.

| Field | Value |
|----|----|
| **Name** | `WelcomeCard` |
| **Template Source** | `Template Manager` |
| **HTML Template** | `Sample Template Welcome Card` |
| **Card Template** | `Welcome Card` |

>If you see a pop-up, click **OK** for the confirmation.

Click **Save**.

!![MobileCards](img_3.png)

You will then see a list of existing mobile cards created in your account.

!![MobileCards](img_4.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set card status to productive)]

By default, the status of newly created cards is **Development**. Cards can be subscribed to only when they are in a **Productive** state.

Click `WelcomeCard` in the Card Templates Tab.

!![MobileCardsImage](img_5.png)

Under **Actions**, click the check mark icon ![MobileCardsIcon](ico_check.png) to change the state to **Productive**.

!![MobileCardsImage](img_6.png)

Choose **Yes** to confirm.

!![MobileCards](img_7.png)

You will notice that the **State** has been changed to **Productive**.

!![MobileCardsImage](img_8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](View the Welcome Card in Mobile Cards client)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

Welcome cards are auto subscribed by default and downloaded automatically after registration.

You should see a Welcome Card in your Mobile application.

[OPTION BEGIN [Android]]

!![MobileCards](img_9.png)

[OPTION END]

[OPTION BEGIN [iOS]]

!![MobileCards](img_10.png)

[OPTION END]

Congratulations, you can now view your first SAP Mobile card on the device.

[DONE]
[ACCORDION-END]
