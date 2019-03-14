---
auto_validation: true
title: Create and Expose Core Data Services Based on a Database Table for productive usage
description: Create UI in Neo and deploy it to Cloud Foundry via multi-target application.

primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
---

## Prerequisites  
- OData Service in ABAP System created 
- you are a Space Developer in the Space your ABAP Instance is running in

## Details
### You will learn  
- How to add ABAP services to multi-target application
- How to create HTML5 module
- How to deploy UI to Cloud foundry
- How to check application existence

---
[ACCORDION-BEGIN [Step 1: ](Create multi-target application)]

  1. Login to your SAP Cloud Platform Cockpit with your global account and choose your Neo subaccount.

  2. Click **Services** and choose **SAP Web IDE Full-Stack**.

      ![open web ide](WebIDE.png)

  3. Click **Go to Service**.

      ![go to service](gotoservice.png)

  4. Login to your SAP Web IDE account and choose **Project from Template**.

      ![template](fromTemplate.png)

  5. Search for MTA, select **Cloud Foundry** as environment and click **Multi-Target Application** and **Next**.

      ![multi target](MultiTarget.png)

  6. Enter project name **`MTA_Project_XXX`** and click **Next**.

      ![project name](projectName.png)

  7. Enter **`MTA_Project_XXX`** as application ID, select **`0.0.1`** as application version. Check **Use HTML5 Application Repository** and click  **Finish**.

      ![HTML5 app](AppID.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure project settings and SAP Web IDE Builder)]

  1. Right-click on your project **`MTA_Project_XXX`** select **Project** > **Project Settings**.

      ![open web ide](setting.png)

  2. Select **Cloud Foundry** as Project and custom Cloud Foundry settings.
     - API Endpoint: **`<your_api_endpoint>`**
     - Organization: **`<your_organization>`**
     - Space: **`<your_space>`**

     Click **Install Builder** and **Save**.

      ![open web ide](builder.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add ABAP service to multi-target application)]

  1. Select **File** > **New** > **SAP Cloud Platform Service**.

      ![open web ide](new.png)

  2. Search for **ABAP**, select it and click **Next**.

      ![open web ide](abap.png)

  3. Select **Use an existing instance**, your instance, provide a resource name and click **Finish**.

      ![open web ide](instance.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create HTML5 module)]

  1. Right-click on your project **`MTA_Project_XXX`** and select **New** > **HTML5 Module**.

      ![HTML5 Module](HTML5.png)

  2. Choose **List Report Application** and click **Next**.

      ![Module](module1.png)

  3. Provide following information:
     - Module Name: **`MTA_Project_XXX`**
     - Title: **`MTA_Project_XXX`**

     Click **Next**.

      ![Module](module2.png)

  4. Select **SAP Cloud Platform Service**, then click on your service instance.

      ![Choose service catalog](ServiceCatalog.png)

  5. Logon to SAP Cloud Platform ABAP environment and switch back to SAP Web IDE.

      ![Environment](environment.png)

  6. Select **SAP Cloud Platform Service** and your service instance **`Z_I_BOOKING_XXX`** and click **Next**.

      ![Service](service2.png)

  7. Select **Selected Service Metadata** and click **Next**.

      ![Service](service3.png)

  8. Select **Booking** as OData collection and click **Finish**.

      ![Service](service4.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Test UI on Cloud Foundry)]

  1. Right-click on **`MTA_Project_XXX`** and select **Run** > **Run Configurations**.

      ![run](run.png)

  2. Click **`+`** to add a new run configuration.

      ![add](add.png)

  3. Select **Run as Web Application**.

      ![run](run2.png)

  4. Create your run configuration.
     - Name: **`Run index.html on Cloud Foundry`**
     - Select your **`flpSandbox.html`** file.

     Click **Run on Cloud Foundry**, select **Without Frame** and click **Save and Run**.

     ![run](run3.png)

  5. Logon to your SAP Cloud Platform ABAP environment.

      ![run](environment.png)

  6. Select the **`MTA_Project_XXX`** tile to test your application.

      ![test](test.png)

  7. Select the gear symbol to add columns.

      ![run](test2.png)

  8. Click **Select All** to add all columns, then click **OK**.

      ![run](test3.png)

  9. Select **Go**, to see your result.

      ![run](test4.png)

  10. Check your result.

      ![run](test5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy UI to Cloud Foundry)]

  1. Right-click on your project **`MTA_Project_XXX`** and select **Build** > **Build**.

      ![build](build.png)

  2. Click on the **`+`** symbol to open **`mta_archives`**.

      ![plus](plus.png)

  3. Right-click on your `mtar` file **`MTA_Project_XXX_0.0.1.mtar`** and select **Deploy** > **Deploy to SAP Cloud Platform**.

      ![deploy](deploysapcp.png)

  4. Deploy your **`mtar`** file to SAP Cloud Platform.

     - Cloud Foundry API Endpoint: **`<your_api_endpoint>`**
     - Organization: **`<your_organization>`**
     - Space: **`<your_space>`**

     Click **Deploy**.

    ![run](deploysapcp2.png)

  5. Check your deployment.

      ![check](check.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check app existence on Cloud Foundry and run as a business user)]

  1. Login to your SAP Cloud Platform Cockpit and select your global account.

      ![global](global.png)

  2. Select **Subaccounts**.

      ![subaccount](subaccount.png)

  3. Select your subaccount.

      ![sub2](sub2.png)

  4. Click Spaces.

      ![space](space.png)

  5. Select your space.

      ![space2](space2.png)

  6. Select your application **`MTA_Project_XXX_appRouter`**.

      ![mta](mta.png)

  7. Copy application routes URL for later use.

      ![url](url.png)

  8. Switch to SAP Web IDE, select your **`MTA_Project_XXX`** and open the **`manifest.json`** file. Copy the ID and application version for later use.

      ![copy](copy.png)

  9. Create your URL by using following:
     `<application_routes>/<id>-<version>/`

```swift
Example:
        <application_routes>/MTA_Project_XXX-1.0.0/

```

  10. Copy your URL into a browser of choice and logon to SAP Cloud Platform ABAP environment.

      ![logon](environment.png)

  11. Select your Application **`MTA_Project_XXX`**.

      ![project](project.png)

  12. Check your result.

      ![project2](project2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

Login to your SAP Cloud Platform Cockpit and select your global account.
[ACCORDION-BEGIN [Step 10: ](Test yourself)]


[VALIDATE_1]
[ACCORDION-END]

---
