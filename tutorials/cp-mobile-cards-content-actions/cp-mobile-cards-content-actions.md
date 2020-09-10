---
title: Implement A Card With Content-Based Actions
description: Create a card with various actions on its content, such as trigger a phone call, send an email, open maps app or launch a website with more details.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services ]
time: 20
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

## Details
### You will learn
  - How to implement a card with content-based actions
  - How to use native mobile device capabilities with SAP Mobile Cards

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with real world use case)]

A procurement manager at a company is about to visit the company's partner vendor. Rather than making a note of the company's details, the procurement manager uses SAP Mobile Cards with content based actions. The procurement manager can call the vendor, open the address in the maps app or send an email to the vendor from within SAP Mobile Cards App.

!![MobileCardsImage](img_1.gif)
!![MobileCardsImage](img_2.gif)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new action card)]

Make sure you have logged in to the **SAP Cloud Platform Mobile Services cockpit**. Navigate to **SAP Mobile Cards** to look into Mobile Cards configuration.

![MobileCardsImage](img_3.png)

Click the **Create Card Template** icon ![MobileCardsIcon](ico_new_card.png).

![MobileCardsImage](img_4.png)

Provide the required information:

| Field | Value |
|----|----|
| **Name** | `Content Action Card` |
| **Version** | `1.0` |
| **Destination** | `com.sap.edm.sampleservice.v2` |
| **Template Source** | `Template Manager` |
| **HTML Template** | `Sample Product Template` |
| **Card Template** | `Server Managed Card` |

> If you see a notification, click **`X`** to close it.
  ![MobileCardsImage](img_5_1.png)

![MobileCardsImage](img_5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Implement the card with content-based actions)]

Navigate to the **Editor** tab and click **Flip to Back** to implement actions on the content.

![MobileCardsImage](img_6.png)

Replace the **Supplier Address** span, as highlighted in the image, with the following code.

```HTML
<span class="listitem-value">
    <div class="c2g_address">
        {{d.results.[0].SupplierDetails.Street}}
        &nbsp;
        {{d.results.[0].SupplierDetails.HouseNumber}}
        <br>
        {{d.results.[0].SupplierDetails.PostalCode}}
        &nbsp;
        {{d.results.[0].SupplierDetails.City}}
        ,
        {{d.results.[0].SupplierDetails.Country}}
    </div>
</span>
```

 ![MobileCardsImage](img_7.png)

>By adding the `c2g_address` tag to the address, SAP Mobile Cards client can now parse the address and offer the user to show it on a map.

Next, replace the **Supplier Email** span, as highlighted in the image, with the following code.

```HTML
<span class="listitem-value">
    <div class="c2g_email">
      {{d.results.[0].SupplierDetails.EmailAddress}}
    </div>
</span>
```

![MobileCardsImage](img_8.png)

>By adding the `c2g_email` tag to the email, SAP Mobile Cards client can now parse the email correctly and offer the user to start writing an email directly to the contact email address of the card.

Finally, add new entries for **Supplier Phone Number** and **Supplier Website** by pasting the entire code-block below.

```HTML
<div class="listitem" style="padding-top: 28px;">
    <span style="font-weight:normal;float:left;font-size:12px">
        Supplier Phone Number
    </span>
    <br>
        <span class="listitem-value" >
        <div class="c2g_phoneNumber">
            {{d.results.[0].SupplierDetails.PhoneNumber}}
        </div>
    </span>
    <br>
    <span style="font-weight:normal;float:left;font-size:12px">
        Supplier Website
    </span>
    <br>
    <span class="listitem-value" >
        <div class="c2g_website">
            http://www.sap.com
        </div>
    </span>
</div>
```

![MobileCardsImage](img_9.png)

>By adding the `c2g_phoneNumber` tag to the phone number, SAP Mobile Cards client can now parse the phone number correctly and offer the user to start a call or send an SMS directly to the contact number of the card.

>By adding the `c2g_website` tag to a link the card will allow the user to open an external link from a card. This allows to show more details to a card or give users direct access to relevant information outside the card.

Click **Save**.

!![MobileCardsImage](img_10.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Set card status to productive)]

By default, the status of newly created cards is **Development**. Cards can be subscribed to only when they are in a **Productive** state.

Click **Content Action Card** in the Card Templates tab.

![MobileCardsImage](img_11.png)

Under **Actions**, click the ![MobileCardsIcon](ico_check.png) icon to change the state to **Productive**.

![MobileCardsImage](img_12.png)

Choose **Yes** to confirm.

![MobileCards](img_13.png)

You will notice that the **State** has been changed to **Productive**.

![MobileCardsImage](img_14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Subscribe to the card in SAP Mobile Cards)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

[OPTION BEGIN [Android]]

In the SAP Mobile Cards Android client, tap **+** | **Subscriptions**.

!![MobileCardsImage](img_15.png)

Tap `Content Action Card` under the **All Subscriptions** tab.

!![MobileCardsImage](img_16.png)

Tap **Subscribe** to trigger the download of the Content Action Card in your SAP Mobile Cards app.

!![MobileCardsImage](img_17.png)

Tap ![MobileCardsIcon](ico_android_back.png) 'Back' icon to to see the latest cards downloaded to the device.

!![MobileCardsImage](img_18.png)

> The data is dynamically generated by the sample service on SAP Cloud Platform Mobile Service server. Thus, the data inside the card on your device may be different than what you see in the screenshot.

Select a card to open it.

!![MobileCardsImage](img_19.png)

Tap on the **flip** button ![MobileCardsIcon](ico_android_flip.png) to view the other side of the card.

!![MobileCardsImage](img_20.png)

Tap on the **Action Bar** button ![MobileCardsIcon](ico_android_action_menu.png), and then tap **Open Maps**.

!![MobileCardsImage](img_21.png)

> SAP Mobile Cards App passes the supplier's address to the maps application.

Tap ![MobileCardsIcon](ico_android_back.png) 'Back' to to see the latest cards downloaded on the device.

!![MobileCardsImage](img_23.png)

Tap ![MobileCardsIcon](ico_android_expand.png) button to expand the card action menu.

!![MobileCardsImage](img_24.png)

Tap on **Email `abc@xyz.com`** action to draft an email to the supplier.

!![MobileCardsImage](img_25.png)

> SAP Mobile Cards launches the default mail client and populates the receiver's email id.

[OPTION END]

[OPTION BEGIN [iOS]]

In the SAP Mobile Cards iOS client, tap **More** | **Subscriptions**.

![MobileCardsImage](img_26.png)

Tap `Content Action Card` under the **All** tab.

![MobileCardsImage](img_27.png)

Tap **Subscribe** to trigger the download of the Content Action Card in your SAP Mobile Cards app.

![MobileCardsImage](img_28.png)

Tap on **All Cards**.

![MobileCardsImage](img_29.png)

> The data is dynamically generated by the sample service on SAP Cloud Platform Mobile Service server. Thus, the data inside the card on your device may be different than what you see in the screenshot.

Tap any card to open it.

![MobileCardsImage](img_30.png)

Tap on the **flip** button ![MobileCardsIcon](ico_ios_flip.png) to view the other side of the card.

![MobileCardsImage](img_31.png)

Tap on the **flip** button ![MobileCardsIcon](ico_ios_flip.png) to return to the front view of the card.

![MobileCardsImage](img_32.png)

Tap on the **Action Menu** ![MobileCardsIcon](ico_ios_action.png) button to view the content based actions.

!![MobileCardsImage](img_33.png)

Tap on **Email `abc@xyz.com`** action to draft an email to the supplier, or tap on **Open Maps** action to view the location of the supplier on the default maps client.

!![MobileCardsImage](img_34.png)

[OPTION END]

>In case of any failure, you can navigate to **Activity Logs** by clicking on **More** | **Logs** and then look in **Pending Actions**.

Congratulations, you have built a content action card that creates action based on the content.

[DONE]
[ACCORDION-END]
