---
auto_validation: true
title: Create and Deploy HTML5 and SAP Fiori Launchpad Site Modules
description: Create HTML5 Module and SAP Fiori Launchpad Site Module. Deploy your UI to Cloud Foundry and run your application as a business user.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
---

## Prerequisites  
- You need a SAP Cloud Platform ABAP Environment trial user or a license.
- You need to have enough quota for HTML5 application and portal services. These services can be found in the entitlements of your subaccount.  


## Details
### You will learn  
- How to add ABAP services to multi-target application
- How to create HTML5 module
- How to create SAP Fiori launchpad site module
- How to deploy UI to Cloud Foundry
- How to run application as business user

---
[ACCORDION-BEGIN [Step 1: ](Create multi-target application)]

  1. If you are using your trial user, then login to your [SAP Cloud Platform trial cockpit](https://cockpit.hanatrial.ondemand.com/) and select **Launch SAP Web IDE**.

      ![open web ide](WebIDE0.png)

     Otherwise login to your [SAP Cloud Platform cockpit](https://account.hana.ondemand.com/), click **Services**, choose **SAP Web IDE Full-Stack** and click **Go to Service**.

      ![open web ide](WebIDE.png)

      ![go to service](gotoservice.png)

  2. In your SAP Web IDE account select **File** > **New** > **Project from Template**.

      ![template](fromTemplate.png)

  3. Search for multi-target, select **Multi-Target Application** and click **Next**.

      ![multi target](MultiTarget.png)

  4. Enter project name **`MTA_Project_XXX`** and click **Next**.

      ![project name](projectName.png)

  5. Enter **`MTA_Project_XXX`** as application ID, select **`0.0.1`** as application version.
  Check **Use HTML5 Application Repository** and click **Finish**.

      ![HTML5 app](AppID.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Copy API endpoint)]

  1.  Login to your **SAP Cloud Platform cockpit trial** and select **trial**.

      ![global](trial2.png)

     Or Login to your **SAP Cloud Platform cockpit** and select your global account. Select **Subaccounts** and your **subaccount**.

      ![sub2](sub2.png)

  4. Copy your API endpoint for later use.

      ![global](endpoint2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure project settings)]

  1. Switch to SAP Web IDE and right-click on your project **`MTA_Project_XXX`** select **Project** > **Project Settings**.

      ![open web ide](setting.png)

  2. Select **Cloud Foundry** as Project and custom Cloud Foundry settings.
     - API Endpoint: **`<your_api_endpoint>`**
     - Organization: **`<your_organization>`**
     - Space: **`<your_space>`**

     Click **Save**.

      ![open web ide](builder.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add ABAP service to multi-target application)]

  1. Select your project **`MTA_Project_XXX`** > **New** > **SAP Cloud Platform Service**.

      ![open web ide](new.png)

  2. If you are using the SAP Cloud Platform trial cockpit, search for **ABAP**, select **`abap-trial`** and click **Next**.

      ![open web ide](abaptrial.png)

     If you are using the SAP Cloud Platform cockpit, then search for **ABAP**, select it and click **Next**.

      ![open web ide](abap.png)

  3. Select **Reuse instance**, your instance, provide a resource name and click **Finish**.

      ![open web ide](instance.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create HTML5 module)]

  1. Right-click on your project **`MTA_Project_XXX`** and select **New** > **HTML5 Module**.

      ![HTML5 Module](HTML5.png)

  2. Choose **List Report Application** and click **Next**.

      ![Module](module1.png)

  3. Provide following information:
     - Module Name: **`TRAVEL_APP_XXX`**
     - Title: **`TRAVEL_APP_XXX`**
     - Namespace: **`namespace_xxx`**

     Click **Next**.

      ![Module](module2.png)

  4. Select **SAP Cloud Platform Service**, then click on your resource.

      ![Choose service catalog](ServiceCatalog.png)

  5. Logon to SAP Cloud Platform ABAP environment trial or SAP Cloud Platform ABAP environment and switch back to SAP Web IDE.

      ![Environment](environment.png)

  6. Select your resource **`ZUI_C_TRAVEL_M_XXX`** and click **Next**.

      ![Service](service2.png)

  7. Check **Selected Service Metadata** and click **Next**.

      ![Service](service3.png)

  8. Select **`TravelProcessor`** as OData collection and click **Finish**.

      ![Service](service4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Remove destination service resource)]

  1. Open your **`mta.yaml`** file, click on **MTA Editor** and **Resources**.

      ![HTML5 Module](destination.png)

  2. Select your destination resource **`dest_MTA_Project_XXX`** and delete it.

      ![Module](destination2.png)

  3. Save your **`mta.yaml`** file.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Test UI on Cloud Foundry)]

  1. Right-click on **`TRAVEL_APP_XXX`** and select **Run** > **Run Configurations**.

      ![run](run.png)

  2. Click **`+`** to add a new run configuration.

      ![add](add.png)

  3. Select **Run as Web Application**.

      ![run](run2.png)

  4. Create your run configuration.
     - Name: **`Run TRAVEL APP XXX on Cloud Foundry`**
     - Select your **`flpSandbox.html`** file.

     Click **Run on Cloud Foundry**, select **Without Frame** and click **Save and Run**.

     ![run](run3.png)

  5. Logon to your SAP Cloud Platform ABAP environment trial or SAP Cloud Platform ABAP environment.

      ![run](environment.png)

  6. Select the **`TRAVEL_APP_XXX`** tile to test your application.

      ![test](test.png)

  7. Select **Go**, to see your result.

      ![run](test4.png)

  8. Check your result.

      ![run](test5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create SAP Fiori launchpad site module)]

  1. Open SAP Web IDE and right-click on your project **`MTA_Project_XXX`** and select **New** > **SAP Fiori Launchpad Site Module**.

      ![Define inbound tile](site.png)

  2. Create a SAP Fiori launchpad site module:
     - Module name: **`FLP_Site_Module_XXX`**

      ![Define inbound tile](site2.png)

    Click **Finish**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create inbound tile)]

1. Open **`manifest.json`** and select **Navigation**.

    Create semantic object:

    - Semantic object: `Travel_App_XXX`
    - Action: display

    Create inbound tile:

    - Title: `Travel_App_XXX`
    - Subtitle: `Travel_booking_application`
    - icon: `sap-icon://sap-logo-shape`

      ![Define inbound tile](manitile.png)

2. Save your changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Create SAP Fiori launchpad site module)]
  1. Open the **`CommonDataModel.json`** file in **`FLP_Site_Module_XXX`** and click **Add Group**.

    ![Define inbound tile](tile.png)

  2. Create new group:
     - Group name: **`Travel App XXX`**

    ![Define inbound tile](tile2.png)

  3. Click on your group.

    ![Define inbound tile](tile3.png)

  4. Add your project app **`Travel_App_XXX`** to your group and click **Select**.

    ![Define inbound tile](tile4.png)

  5. Now your project app **`Travel_App_XXX`** is added to your group.

    ![Define inbound tile](tile5.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ](Deploy UI to Cloud Foundry)]

  1. Right click on **`MTA_Project_XXX`** and select **Build** > **Build with Cloud MTA Build Tool (recommended)**.

      ![Deploy UI to Cloud Foundry](Build22.png)

  2.  Open **`mta_archives`**.

      ![Deploy UI to Cloud Foundry](build2.png)

  3.  Right-click on **`MTA_Project_XXX_0.0.1.mtar`**, select **Deploy** > **Deploy to SAP Cloud Platform**.

      ![Deploy UI to Cloud Foundry](build3.png)

  4.  Deploy your **`mtar`** file to SAP Cloud Platform.

       - Cloud Foundry API Endpoint: **`<your_api_endpoint>`**
       - Organization: **`<your_organization>`**
       - Space: **`<your_space>`**

       Click **Deploy**.

      ![Deploy UI to Cloud Foundry](build4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Run application on Cloud Foundry as business user)]

  1. Open SAP Cloud Platform cockpit trial or SAP Cloud Platform cockpit and navigate to your **trial subaccount**.  
     Select your space **dev**.

      ![Deploy UI to Cloud Foundry](dev.png)

  2. Run your app router **`mta-project-xxx-approuter`**.  

      ![Deploy UI to Cloud Foundry](devx.png)

  3.  Select **`mta-project-xxx-approuter`**.

      ![Deploy UI to Cloud Foundry](dev2.png)

  4.  Click on your application routes to open your application in your launchpad.

      ![Deploy UI to Cloud Foundry](dev3.png)

  5. Logon to your SAP Cloud Platform ABAP environment trial or SAP Cloud Platform ABAP environment system.

      ![Deploy UI to Cloud Foundry](environment.png)

  6. You application is now available as a tile in the launchpad. Select your application **`Travel_App_XXX`**.

      ![Deploy UI to Cloud Foundry](app.png)

  7. Click **Go** to see your result.

      ![Deploy UI to Cloud Foundry](app2.png)

  8. Check your result.

      ![Deploy UI to Cloud Foundry](app3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://s.userzoom.com/m/NiBDODgzUzQxNiAg" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
