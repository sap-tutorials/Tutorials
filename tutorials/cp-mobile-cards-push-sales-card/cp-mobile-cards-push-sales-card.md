---
parser: v2
auto_validation: true
time: 30
tags: [  tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>sap-mobile-cards, products>sap-mobile-services, products>sap-business-application-studio]
primary_tag: products>sap-mobile-cards
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

# Push a Sales Order Card to Your Mobile Device
<!-- description --> Push a card to the mobile device by triggering an action in an external system.

## Prerequisites
  - You have [created a push enabled card](cp-mobile-cards-create-sales-push).
  - Postman client, CURL or any other API development platform

## You will learn
  - How to push a card to your mobile device from an external system
  - How to view and delete subscription cards on your mobile device

---

### Understand the use case


In the [previous tutorial of this group](cp-mobile-cards-create-sales-push), you created a card that will be used by the Sales Managers to Approve / Reject sales orders.

In this tutorial, you will mimic Watson's request by triggering a request from an external system. Once the request is created, Alice will receive a push notification.


### Create service key


1. Open your Mobile Services Cockpit.

    <!-- border -->![CPMS Cockpit](img_2_1.png)

2. Click **SAP Mobile Cards**.

    <!-- border -->![Mobile Cards Cockpit View](img_2_2.png)

3. Navigate to **Service Keys** tab and click **+** to create a new service key.

    <!-- border -->![Service Keys Tab](img_2_3.png)

      > If you can't see the Service Keys tab, refresh your page.

4. Enter/Select the values as indicated in the table, and click **OK**.

    |Alias|Role|
    |:---|:---|
    |`RegisterCardServiceKey`| `register_card` |

    <!-- border -->![New Service Keys](img_2_4.png)

You can now see the newly created service key in the table.

Make a note of the **Alias**, **API Key**, and the **URL** for your service key. You will be using these values in an upcoming step.

<!-- border -->![Service Keys Table](img_2_5.png)


### Copy receiver's user name


1. In the Mobile Services cockpit, Click **Features** tab  &rarr; **Push Notification**.

    <!-- border -->![Push Notification Feature](img_3_1.png)

2. In the *Push Registrations* tab, make a note of your **User Name**. You will need it in an upcoming step.

    <!-- border -->![Push Notification Feature](img_3_2.png)

    > If you are unable to determine your User Name, use the **Send Notification** to send a custom message as a push notification.


### Copy sales order ID


1. **Click Features Tab  &rarr; Mobile Sample OData ESPM**.

    <!-- border -->![Sample Data Feature](img_4_1.png)

2. **Click V2**.

    <!-- border -->![Sample Data Cockpit](img_4_2.png)

3. Access `/SalesOrderHeaders` Entity in the browser window that opens.

    <!-- border -->![Sample Data in Browser](img_4_3.png)

4. Make a note of a **Sales Order ID**. You will need it in the next step.

    <!-- border -->![Sales Order Headers](img_4_4.png)


### Make HTTP request


>Make sure you are choosing the preferred tab **above**: **Postman** or **cURL**.

In this step, you will make an HTTP Request. This knowledge is transferrable, i.e. you can implement this logic in your Web/Desktop/Mobile applications.

[OPTION BEGIN [Postman]]

1. Create your HTTP Request by using the following configuration:

    * **Request Type & URL**

    |HTTP Method Type|URL|
    | :--- | :--- |
    |**`POST`**|**`<The URL you copied from the service key table>/mobileservices/origin/hcpms/CARDS/v1/register/templated`**|

    * **Authorization**

    |Authorization Type|Username|Password|
    | :--- | :--- | :--- |
    |**`BASIC`** | **`RegisterCardServiceKey`** | **`<The API Key you copied from the service key table>`** |

    * **Headers**

    |Key|Value|
    |:---|:---|
    |`Content-Type`| `application/json` |

    * **Body - Raw &rarr; JSON**

    ```JSON
    {
      "method": "REGISTER",
      "username": "<The User Name you copied from the Push Registration table>",
      "templateName": "Sales Push Card BAS",
      "parameters": {
        "SalesOrderID": "<The Sales Order Id you copied from the Sample OData Service>"
      }
    }
    ```

[OPTION END]

[OPTION BEGIN [cURL]]


1. Use Base64 encoding to encode the following:

    **`RegisterCardServiceKey:<The API Key you copied from the service key table>`**

2. Use the following cURL code to make the HTTP request. You will need to modify the text mentioned in **< >**.

```Curl
curl --location --request POST '<The URL you copied from the service key table>/mobileservices/origin/hcpms/CARDS/v1/register/templated' \
--header 'Content-Type: application/json' \
--header 'Accept: application/json' \
--header 'Authorization: Basic <BASE_64 Encoded Value>' \
--data-raw '{
  "method": "REGISTER",
  "username": "<The User Name you copied from the Push Registration table>",
  "templateName": "Sales Order Push Card",
  "parameters": {
    "SalesOrderID": "<The Sales Order Id you copied from the Sample OData Service>"
  }
}'
```

[OPTION END]


### View notification on device


>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

> **Before proceeding, ensure that you have created a Push Card by following [this tutorial](cp-mobile-cards-create-sales-push).**

[OPTION BEGIN [Android]]

1. Either terminate the app or send it to background.

    <!-- border -->![Android Background](img_6_1.png)

2. Upon a successful HTTP Request call, you will see a **New card added** notification; **Tap** on the notification to see thee card.

    <!-- border -->![Android Notification](img_6_2.png)

3. **Tap** on the card to to open it.

    <!-- border -->![Android Home](img_6_3.png)

4. You can now see a sales order card for the **Sales Order ID** you specified in the HTTPS request method.

    <!-- border -->![Android Pushed Card](img_6_4.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Either terminate the app or send it to background.

    <!-- border -->![iOS Background](img_6_5.png)

2. Upon a successful HTTP Request call, you will see a **New card added** notification; **Tap** on the notification.

    <!-- border -->![iOS Notification](img_6_6.png)

3. **Tap** on the card to to open it.

    <!-- border -->![iOS Placeholder](img_6_7.png)

4. You can now see a sales order card for the **Sales Order ID** you specified in the HTTPS request method.

    <!-- border -->![iOS Pushed Card](img_6_8.png)

[OPTION END]



**Congratulations!** You can now push cards to your Mobile Device effortlessly.

---
