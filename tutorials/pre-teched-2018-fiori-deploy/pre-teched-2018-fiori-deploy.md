---
title: Deploy your app to Fiori Launchpad
description: Learn how to deploy your application
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>sapui5, products>sap-cloud-platform, products>sap-web-ide, products>sap-fiori ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Details
### You will learn  
Deploy your application to the SAP Cloud Platform so it can be available to be added to the Fiori Launchpad. SAP BUILD applications are designed for the Fiori Launchpad.


### Time to Complete
**15 Min**

---


[ACCORDION-BEGIN [Step 1: ](Deploy app on SAP Cloud Platform)]

Deploying your app is simple.

Right-click on your **`te2018lotteryapp`** project folder, and select **Deploy > Deploy to SAP Cloud Platform**.

![deploy to SAP Cloud Platform menu](1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deployment dialog box)]

In the **Deploy Application to SAP Cloud Platform** dialog box, confirm that **Deploy a new application**, and the **Activate** checkbox are both selected. Click **Deploy**.

![deploy app options](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open the active version of your app)]

In the **Successfully Deployed** confirmation dialog, click the **Register to SAP Fiori launchpad** to register your app. Applications imported from BUILD are *generally* Fiori apps, meaning they don't use an `index.html` so they can't be run as a standalone web app.

![successful deployment](3.png)

Answer the true or false question below.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Setup the launchpad app)]

A new wizard will pop-up to create the Fiori Launchpad app.

On the first screen, **General Information**, leave the settings as is and click **Next**.

![general information for Fiori app](4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Configure the launchpad tile)]

Provide a title and description for the tile. If you want, change the Icon associated with the tile. There is a live preview of the tile.

Field Name     | Value
:------------- | :-------------
Title           | Pre-TechEd 2018
Subtitle    | Lottery app for TechEd 2018
Icon            | `e-learning`


Click **Next**.

![tile configurations](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Assign the tile)]

Select the **`te2018lotteryapp`** you created earlier in this tutorial series as the **Site**. Leave the preset **Catalog** and **Group** as is.

Click **Finish** to register your app to Fiori launchpad.

![assign the tile and app to a Fiori site](6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](View your launchpad app)]

If you successfully set up your app, Fiori Launchpad, and tile, you will see a **Successfully Registered** dialog box.

![registered app](8.png)

Click **Open the registered app** to see your live BUILD app and personal Fiori Launchpad!

![Fiori launchpad](9.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Copy the Fiori Launchpad URL)]

In your running Fiori Launchpad, copy the **Launchpad URL** and paste it into the field below and click **Validate**.


[VALIDATE_9]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Register for the ticket drawing)]
Thank you for completing the pre-SAP TechEd 2018 drawing mission!

Since you have gotten to the last step in this mission, you have the option of entering a drawing for a chance to win a free SAP TechEd pass (includes show floor and lectures access only) to the location of your choice. To register, please fill out one of the following registration forms, depending on which SAP TechEd you want to register for.

>**IMPORTANT** You can only register for one location.

Contest ends Sept. 10, 2018.

- [Las Vegas](https://www.sap.com/cmp/oth/crm-xb18-evt-teched010/index.html)
- [Barcelona](https://www.sap.com/cmp/oth/crm-xb18-evt-teched01/index.html)
- [Bangalore](https://www.sap.com/cmp/oth/crm-xb18-evt-teched011/index.html)


[VALIDATE_10]

[ACCORDION-END]
