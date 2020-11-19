---
title: Connect SAP Cloud Platform to Your SAP Gateway Demo System Account (ES5)
description: Create a connection between SAP Cloud Platform and the SAP Gateway Demo System (ES5).
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform
author_name: Lindsay Bert
author_profile: https://github.com/LindsayBert
---

## Prerequisites
- You have created an account on the Gateway Demo System: [Create an Account on the Gateway Demo System](https://developers.sap.com/tutorials/gateway-demo-signup.html). Note that when you create your account, make a note of the user name and password that you entered -- you'll be needing it in this tutorial.

## Details
### You will learn
  - How to create a destination between SAP Cloud Platform and the SAP Gateway Demo System account

[ACCORDION-BEGIN [Step 1: ](Open your subaccount)]

1. Log on to SAP Cloud Platform and click **Enter Your Trial Account**.

    !![Log on to SAP Cloud Platform](1-enter-trial-account.png)

2. Click on the **trial** tile to navigate to your trial subaccount in the SAP Cloud Platform cockpit. If you are using your own subaccount, you can select it instead.

    !![Click trial account](2-click-trial.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create destination to SAP Gateway Demo System)]

In this step, you'll create connectivity between SAP Cloud Platform and the SAP Gateway Demo system account.

1.  In the left navigation panel, click **Destinations** under **Connectivity**.

    !![Open destinations](3-open-destinations.png)

2. Click **New Destination**.

    !![New destination](4-create-destination.png)



3.  Add the following destination properties:

    >Important: If you haven't already created a user in ES5 as explained in the prerequisites above, please do so now and follow the steps in this topic:  [Create an Account on the Gateway Demo System](https://developers.sap.com/tutorials/gateway-demo-signup.html).

    |  Field     | Value
    |  :------------- | :-------------
    |  Name           | `ES5`
    |  Type          | `HTTP`
    |  Description    | `SAP Gateway ES5`
    |  URL           | `https://sapes5.sapdevcenter.com`
    |  Proxy Type          | `Internet`
    |  Authentication    | `BasicAuthentication`
    |  User Name          | Your ES5 Gateway user
    |  Password    | Your ES5 Gateway password

4. Make sure that the **Use default JDK truststore** checkbox is checked.

5. Enter the following **Additional Properties** depending on your scenario. Click the **New Property** button each time to add a new property.

    >If you are not sure which ones to enter, enter all of them.

    |  Field     | Value
    |  :------------- | :-------------
    | `WebIDEEnabled`          | `true`
    | `WebIDESystem`    | `Gateway`
    | `WebIDEUsage`           | `odata_abap`
    | `sap-platform`          | `ABAP` (enter this property manually as it is not available in the dropdown list)
    | `sap-client`          | `002`
    | `HTML5.DynamicDestination`          | `true` (enter this property manually as it is not available in the dropdown list)

    >Note that SAP Business Application Studio reuses SAP Web IDE properties. In this way, the destination configuration as well as the migration from SAP Web IDE to SAP Business Application Studio, is much simpler.

    >For more information about these properties, see [Connect to ABAP Systems](https://help.sap.com/viewer/825270ffffe74d9f988a0f0066ad59f0/CF/en-US/5c3debce758a470e8342161457fd6f70.html) and [Configure (HTML5) Destinations](https://help.sap.com/viewer/29badeeee3684338b2e870139bdc4d86/Cloud/en-US/fab4035652cb4fc48503c65dc841d335.html).

    Your screen will look like this:

    ![Destination Properties](5-destination-properties.png)

6. Click **Save**.


7. Click **Check Connection** to make sure that everything is working.

    !![Check connection](6-check-connection.png)

    You'll get confirmation that the connection is established.

    ![Connection established](7-connection-established.png)

    >This is not an error. The check connection operation tries to open the ES5 URL and gets redirected to the login page.

[VALIDATE_6]
[ACCORDION-END]
