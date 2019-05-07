---
title: Prepare SAP Web IDE for Cloud Foundry Development
description: Enable and open SAP Web IDE Full-Stack in SAP Cloud Platform, and configure the CF API endpoint to deploy.
author_name: Marius Obert
author_profile: https://github.com/iobert
primary_tag: products>sap-cloud-platform-for-the-cloud-foundry-environment
tags: [  tutorial>beginner, topic>html5, topic>sapui5, products>sap-cloud-platform ]
time: 5
---
## Prerequisites  
- **Tutorials:** If you don't have an SAP Cloud Platform account, follow the tutorial to [set up a free developer account](hcp-create-trial-account).

## Details
### You will learn  
  - How to enable SAP Web IDE Full-Stack inside of SAP Cloud Platform
  - How to open SAP Web IDE Full-Stack

---

[ACCORDION-BEGIN [Step 1: ](Open your SAP Cloud Platform account)]

Open your SAP Cloud Platform account (if you have a free developer account, click [here](https://account.hanatrial.ondemand.com/) to open the console).

Choose **Neo Trial**.

>**Trouble logging in?** If you have trouble logging in to your SAP Cloud Platform Cockpit, and you are using a company account (one provided by your employer), it is possible that the Cloud access has been locked.  Create a new FREE trial account by clicking the link above, and use your personal email address to set up the new account.

![SAP Cloud Platform Developer Account Login Screen](login_screen.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to Services)]

You should now be in the SAP Cloud Platform cockpit, as shown below.  Click the **Services** menu item on the left.

Next, click the **SAP Web IDE Full-Stack** box.  

>You may need to scroll down to find this box.

![SAP Cloud Platform Console - Services](service_web_ide.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enable SAP Web IDE Full-Stack)]



>**Enabled**: If the box does **NOT** say **Enabled**, click the **Not Enabled** button to enable this service.  In the screen that appears, click the **Enable** button at the top of the page to confirm this feature is set up.

![Go to service](enable-service.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open SAP Web IDE Full-Stack)]

In the SAP Web IDE Full-Stack screen, at the bottom, click **Go to Service**.  

![SAP Cloud Platform Console - services link](services_link.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Bookmark the page)]

A new tab opens, and SAP Web IDE Full-Stack loads.

>**Bookmark this page!**  If you create a bookmark to this page, it is easy to get back to SAP Web IDE Full-Stack later.

![SAP Cloud Platform Console - services button](web_ide_start_screen.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Go to the Cloud Foundry settings)]

Click on the **gears icon** in the bar on the left-hand side to open the settings menu. Then, choose the **Cloud Foundry** settings.

![webidesettings](cfconfig.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Enter the Cloud Foundry endoint you want to use)]

1. Select the API endpoint according to the data center in which your subaccount lives. Click on the dropdown control and the most common endpoints will be suggested to you. You can see find [a list](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/350356d1dc314d3199dca15bd2ab9b0e.html?platform=hootsuite) of all endpoints in the help portal.

    ![cfendpoints](cfendpoints.png)

2. You might be prompted for your Cloud Foundry credentials. Enter the credentials you used for the SAP Cloud Cockpit.

    ![cfcred](cfcred.png)

3. If there is a subaccount in the selected data center, the *Organization* and *Space* field will be populated by default. You can change these values via the dropdown control. Don't forget to **Save** this configuration.

    > If the fields are not being populated, you probably selected an incorrect endpoint or the entered credentials were invalid.

    ![cfsave](cfsave.png)

> You can also choose specific API endpoints per project in the project settings.

[DONE]
[ACCORDION-END]
