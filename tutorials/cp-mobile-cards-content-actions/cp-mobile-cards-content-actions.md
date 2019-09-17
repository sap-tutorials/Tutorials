---
title: Implement A Card With Content-Based Actions
description: Create a card with various actions on its content, such as trigger a phone call, send an email, open maps app or launch a website with more details.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services ]
time: 20
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to implement a card with content-based actions
  - How to enable cards to trigger content-based actions, including native mobile device capabilities

 Content-based actions allow users to trigger a phone call, send an email, open maps app, or launch a website with more details from a card.

![SAP Cloud Platform Mobile Services - Opera](img_0.gif)

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A company wants to mobilize a paper-based customer ticketing service process where a technician has to work on it. With content-based actions enabled on a card, the technician can launch the maps application on the device with the customer address to help him to find the customer. The content-based card allows him to call the customer with a single click.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new action card)]

Make sure you have logged into the **SAP Cloud Platform Mobile Services cockpit**. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

![SAP Cloud Platform Mobile Services - Opera](img_000.png)

Click the **Create a New Card** icon.

![SAP Cloud Platform Mobile Services - Opera](img_001.png)

Provide the required information:

| Field | Value |
|----|----|
| **Name** | `ContentActionCard` |
| **Destination** | `com.sap.edm.sampleservice.v2` |
| **Template Source** | `Template Manager` |
| **HTML Template** | `Sample Product Template` |

> If you see a pop-up, click **OK** for the confirmation.

![SAP Cloud Platform Mobile Services - Opera](img_009.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Implement the card with content-based actions)]

Navigate to the **Editor** tab, and click **Flip to Back** to implement actions on the content.

![SAP Cloud Platform Mobile Services - Opera](img_010.png)

Copy and paste the following `<div>` tag before `{{street}}` into the **address** `<span>` to declare the address:

```HTML
<div class="c2g_address">
```
Add the closing `<div>` tag before the closing `<span>` tag:

```HTML
</div>
```

 ![SAP Cloud Platform Mobile Services - Opera](img_011.png)

>By adding the `c2g_address` tag to the address, SAP Mobile Cards client can now parse the address and offer the user to show it on a map.

Next, copy and paste the following `<div>` tag before `{{SupplierEmailDetailAddress}}` into the **email** `<span>` to declare the email address:

```HTML
<div class="c2g_email">
```
Add the closing `<div>` tag right before the closing `<span>` tag:

```HTML
</div>
```
![SAP Cloud Platform Mobile Services - Opera](img_013.png)

>By adding the `c2g_email` tag to the email, SAP Mobile Cards client can now parse the email correctly and offer the user to start writing an email directly to the contact email address of the card.

Next, copy and paste the following `<div>` tag after the address `<class>` tag to add a phone number and website address to the card:

```HTML
<div class="listitem" style="padding-top: 28px;">
<span style="font-weight:normal;float:left;font-size:12px">
Supplier Phone
</span>
<br>
<span class="listitem-value" >
<div class="c2g_phoneNumber">
{{PhoneNumber}}
</div>
</span>
<br>
<span class="c2g_website"> http://www.sap.com</span>
</div>
```

![SAP Cloud Platform Mobile Services - Opera](img_014.png)

>By adding the `c2g_phoneNumber` tag to the phone number, SAP Mobile Cards client can now parse the phone number correctly and offer the user to start a call or send an SMS directly to the contact number of the card.

>By adding the `c2g_website` tag to a link the card will allow the user to open an external link from a card. This allows to show more details to a card or give users direct access to relevant information outside the card.

Click **Save**.

![SAP Cloud Platform Mobile Services - Opera](img_015.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Subscribe to the card in SAP Mobile Cards)]

In the SAP Mobile Cards application, click **More** | **Subscriptions**.

![Card](img_031.png)

Click `ContentActionCard` under the **All** tab.

![Chrome Remote Desktop](img_019.png)

Click **Subscribe** to activate the `ContentActionCard` subscription.

![Chrome Remote Desktop](img_020.png)

Click **All Cards** or **New Cards** to view this card.

![Chrome Remote Desktop](img_022.png)

Click the action icon at the bottom-left to open the available actions on the card.

![Chrome Remote Desktop](img_023.png)

Congratulations, you added content based actions to your card.

![Chrome Remote Desktop](img_024.png)

>In case of any failure, you can navigate to **Activity Logs** by clicking on **More** | **Logs** and then look in **Pending Actions**.

>![SAP Cloud Platform Mobile Services - Opera](img_00.gif)

[DONE]
[ACCORDION-END]
