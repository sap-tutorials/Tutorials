---
title: Enable and configure (IoT for Neo)
description: Setup and configure the use of the IoT for Neo environment in SAP Cloud Platform
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [topic>internet-of-things, products>sap-cloud-platform-internet-of-things, tutorial>beginner ]

---

## Prerequisites
- **Proficiency:** Beginner
- **Tutorials:** [Setup the Tessel device](https://developers.sap.com/tutorials/iot-tessel.html)


## Next Steps
- [SAP Cloud Platform IoT for Neo: Configuring the device for environmental sensors data](https://developers.sap.com/tutorials/iot-part7-add-device.html)

## Details
### You will learn

This tutorial will detail the steps needed to simply the process of connecting your hardware device to SAP.

SAP's IoT Services provide a robust and secure way of connecting your device as well as simplified way of specifying your data structures and changing those structures.

This procedure assumes you are using the trial account of the SAP Cloud Platform, but you can use a production account if you have one.

### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Register for or log into SAP Cloud Platform trial)]

Go to the [SAP Cloud Platform trial edition](https://account.hanatrial.ondemand.com/cockpit) log in (or sign up if you don't have an account yet).


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable IoT service)]

Once you log in, click on **Services** in the left-hand navigation bar, scroll down to find **Internet of Things** tile and click on it.

![Services](p6-2.png)

Click on the **Enable** button. After a few seconds the page will update and show **Enabled**.

![Enable Service](p6_3a.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Go to IoT Service Cockpit)]

Once the service is enabled click the **Go to Service** link and a new browser tab will open.

![Access Service](p6_4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy Message Management Service)]

With IoT service enabled, you can begin the steps necessary to configure your device's digital twin and to enable message communication.

The first step will be to deploy and to configure the Message Management Service (MMS). Click on the **Deploy Message Management Service** tile.

Enter in your information in the fields, where your account ID is your p-number (or s-number if you are SAP's customer or partner, or i-/d-number if you are SAP employee) with the world "trial" (no space between the p-number and trial) and your user name is just your p-number.

![Deploy Service](p6_6a.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Go to IoT MMS application dashboard)]

Once successfully deployed then Java application `iotmms` is started in your account.

Return to the SAP Cloud Platform cockpit view and click on **Java Applications** in the left navigation bar. You will see the new `iotmms` application you just deployed. Click on the `iotmms` link to display the **IoT MMS application cockpit**.

![Deployed application](p6_7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Assign IoT MMS user role)]

With the **IoT MMS application cockpit** displayed click on the **Security** > **Roles** in the left navigation bar.

Select the **IoT-MMS-User** (click the empty cell next to the Name to select the row if it is not highlighted in blue).

![Authorizations](p6_8.png)

Then under **Individual Users**, click **Assign** and enter your SAP Cloud Platform user ID (e.g. your p-number without the word "trial" on the end).

![Assign Role](p6_9.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Restart the app)]

Once user is assigned to the role, stop and start `iotmms` application.

![Start/stop](p6_10.png)

[ACCORDION-END]
