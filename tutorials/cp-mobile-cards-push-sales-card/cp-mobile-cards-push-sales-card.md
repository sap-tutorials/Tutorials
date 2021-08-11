---
title: Push a Sales Order Card to Your Mobile Device
description: Push a card to the mobile device by triggering an action in an external system.
auto_validation: true
time: 30
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>sap-mobile-cards, products>sap-mobile-services, products>sap-business-application-studio]
primary_tag: products>sap-mobile-cards
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

## Prerequisites
  - You are working with Postman client, CURL or any other API development platform.

## Details
### You will learn
  - Push a card to your Mobile Device from an external system
  - View and delete a Web Page Matching Card
  - Perform an action using push notification

---

[ACCORDION-BEGIN [Step 1: ](Understand the use case)]

In the previous tutorial, you created a card that Alice will subscribe to. In this tutorial, you will make Watson create a request in a different application. Once the request is created, Alice will receive a push notification.

You will use Postman to mock Watson's request.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create service key)]

Open your Mobile Services Cockpit.

!![CPMS Cockpit](img_2_1.png)

Click **SAP Mobile Cards**.

!![Mobile Cards Cockpit View](img_2_2.png)

Navigate to **Service Keys** tab and click **+** to create a new service key.

!![Service Keys Tab](img_2_3.png)

> If you can't see the Service Keys tab, refresh your page.

Enter/Select the values as indicated in the table, and click **OK**.

|Alias|Role|
|---|---|
|`SalesPushCardKey`| `register_card` |

!![New Service Keys](img_2_4.png)

You can now see the newly created service key in the table.

Make a note of the **Alias**, **API Key**, and the **URL** for your service key. You will be using these values in an upcoming step.

!![Service Keys Table](img_2_5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Copy receiver's user name)]

**Click Features Tab  &rarr; Push Notification**.
!![Push Notification Feature](img_3_1.png)

Make a note of your **User Name**. You will need it in an upcoming step.

> If you are unable to determine your User Name, use the **Send Notification** to send a custom message as a push notification.

!![Push Notification Cockpit](img_3_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Copy sales order ID)]

**Click Features Tab  &rarr; Mobile Sample OData ESPM**.

!![Sample Data Feature](img_4_1.png)

**Click V2**.

!![Sample Data Cockpit](img_4_2.png)

Add `/SalesOrderHeaders` to the URL in your browser.

!![Sample Data in Browser](img_4_3.png)

Make a note of a **Sales Order Id**. You will need it in the next step.

!![Sales Order Headers](img_4_4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Make HTTP request)]

In this step, you will make an HTTP Request. This knowledge is transferrable, i.e. you can implement this logic in your Web/Desktop/Mobile applications.

>Make sure you are choosing the preferred tab **above**: **Postman** or **cURL**.

[OPTION BEGIN [Postman]]

Create your HTTP Request by using the following configuration:

* Request Type & URL

|HTTP Method Type|`POST`|
| --- | --- |
|URL|`<The URL you copied from the service key table>/mobileservices/origin/hcpms/CARDS/v1/register/templated`|

* Authorization

|Authorization Type|`BASIC`|
| --- | --- |
|Username|`<The Alias you copied from the service key table>` <br> `SalesPushCardKey`|
|Password| `<The API Key you copied from the service key table>`|

* Headers

|Key|`Content-Type`|
|---|---|
|Value| `application/json` |

* Body - Raw + JSON

```JSON
{
  "method": "REGISTER",
  "username": "<The User Name you copied from the Push Registration table>",
  "templateName": "Sales Push Card BAS",
  "parameters": {
    "ID1": "<The Sales Order Id you copied from the Sample OData Service>"
  }
}
```

[OPTION END]

[OPTION BEGIN [cURL]]

Use the following cURL code to make the HTTP request. You will need to modify the text mentioned in **< >**.

```Curl
curl --user SalesPushCardKey:<The API Key you copied from the service key table> --location --request POST '<The URL you copied from the service key table>/mobileservices/origin/hcpms/CARDS/v1/register/templated' \
--header 'Content-Type: application/json' \
  --data-raw '{
    "method": "REGISTER",
    "username": "<Your User Name>",
    "templateName": "Sales Push Card BAS",
    "parameters": {
      "ID1": "<Sales Order Id>"
    }
  }'
```

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View notification on device)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

[OPTION BEGIN [Android]]

You must be subscribed to `Sales Push Card BAS`.

!![Android Subscriptions](img_6_1.png)

> If you can't see the card on your device, please complete this tutorial before moving ahead.

Either terminate the app or send it to background.

!![Android Background](img_6_2.png)

Upon a successful HTTP Request call, you will see a **New card added* notification**; **Tap** on the notification.

!![Android Notification](img_6_3.png)

A place holder card is created while the app syncs data from the back end. **Tap 'Back' icon** ![MobileCardsIcon](ico_android_back.png) to to see the updated card.

!![Android Placeholder](img_6_4.png)

**Tap on the card** to to open it.

!![Android Home](img_6_5.png)

You can now see a sales order card for the **Sales Order Id** you specified in the HTTPS request method.

!![Android Pushed Card](img_6_6.png)

[OPTION END]

[OPTION BEGIN [iOS]]

You must be subscribed to `Sales Push Card BAS`.

!![iOS Subscriptions](img_6_7.png)

> If you can't see the card on your device, please complete this tutorial before moving ahead.

Either terminate the app or send it to background.

!![iOS Background](img_6_8.png)

Upon a successful HTTP Request call, you will see a **New card added** notification; **Tap** on the notification.

!![iOS Notification](img_6_9.png)

A place holder card is created while the app syncs data from the back end. **Wait** for the card to sync to to see the updated card.

!![iOS Placeholder](img_6_10.png)

> If you can't see the updated card. Perform a pull refresh.

You can now see a sales order card for the **Sales Order Id** you specified in the HTTPS request method.

!![iOS Pushed Card](img_6_11.png)

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Perform action through notification)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

Copy another **Sales Order Id** from the sample OData service (e.g., `439bb7dd-50cb-4556-82f4-c835a4627650`). You will perform an action that will update `LifeCycleStatus` and `LifeCycleStatusName`.

!![Action SalesOrder Id](img_7_1.png)

**Send an HTTP Request** for the new Sales Order Id.

```JSON
{
  "method": "REGISTER",
  "username": "<The User Name you copied from the Push Registration table>",
  "templateName": "Sales Push Card BAS",
  "parameters": {
    "ID1": "439bb7dd-50cb-4556-82f4-c835a4627650"
  }
}
```

[OPTION BEGIN [Android]]

Upon a successful HTTP Request call, you will see a *New card added* notification**; **Tap** on the **Accept Status** action in the notification.

!![Android Notification Action](img_7_2.png)

Upon performing the action successfully, you will see a toast message on your device.

!![Android Action Toast](img_7_3.png)

Open SAP Mobile Cards application, and tap on the card with the correct Sales Order Id `439bb7dd-50cb-4556-82f4-c835a4627650`.

!![Android Card Tap](img_7_4.png)

Notice that the `LifeCycleStatus` & `LifeCycleStatusName` have been updated.

!![Android Card View](img_7_5.png)

[OPTION END]

[OPTION BEGIN [iOS]]

Upon a successful HTTP Request call, you will see a *New card added* notification**; **Pull** the notification, and **Tap** **Accept Status**.

!![iOS Notification Action](img_7_6.png)

**Open SAP Mobile Cards** application. Upon performing the action successfully, you will see a toast message on your device.

!![iOS Action Toast](img_7_7.png)

**Tap** on the card with the correct Sales Order Id `439bb7dd-50cb-4556-82f4-c835a4627650`.

Notice that the `LifeCycleStatus` & `LifeCycleStatusName` have been updated.

!![Android Card View](img_7_8.png)

[OPTION END]

> After performing the action, the card update may take time. To force the update, *Unsubscribe & Subscribe* to the card.

Open the sample OData service and verify that the changes have taken effect for Sales Order Id - `439bb7dd-50cb-4556-82f4-c835a4627650`.

!![Android Card View](img_7_9.png)

[VALIDATE_1]
[ACCORDION-END]

**Congratulations!** You can now push cards to your Mobile Device effortlessly.

---
