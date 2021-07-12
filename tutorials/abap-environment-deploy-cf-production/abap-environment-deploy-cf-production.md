---
auto_validation: true
title: Develop and Run SAP Fiori Application With SAP Business Application Studio
description: Develop and run your SAP Fiori application with SAP Business Application Studio
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-business-technology-platform, products>sap-business-application-studio ]
time: 25
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
- **Trial:** You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.
- **Licensed system:**
    - The business catalog `SAP_A4C_BC_DEV_UID_PC` (Development - UI Deployment) needs to be assigned to a business role of the developer user. For an existing ABAP systems, the business catalog needs to be added manually to the existing developer business role.
    - You need to be a member at the used global account​
    - You need to be an organization manager at the used Cloud Foundry subaccount
    - You need to be a security administrator at the used Cloud Foundry Subaccount​
    - The SAP Business Application Studio and the SAP BTP, ABAP environment instance should be under same subaccount.



## Details
### You will learn  
- How to assign role collections
- How to create dev spaces
- How to set up organization and space
- How to create list report object pages
- How to run SAP Fiori applications
- How to deploy applications
- How to check BSP library in Eclipse
- How to create IAM apps and business catalogs

---
[ACCORDION-BEGIN [Step 1: ](Assign role collection to user)]

  1. Login to [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/) and click **Enter Your Trial Account**.

      ![assign role collection](bas1.png)

  2. Select your subaccount **trial**.

      ![assign role collection](bas2.png)

  3. Click **Trust Configuration** and select **Default identity provider** to set up your trust.

      ![assign role collection](bas3.png)

      **HINT:** If you are using a licensed system, make sure you have the trust administrator role assigned to your user.

  4. Enter your e-mail address and click **Show Assignments**.

      ![assign role collection](bas5.png)

  5. Click **Assign Role Collection** .

      ![assign role collection](bas6.png)

  6. Select **`Business_Application_Studio_Developer`** and click **Assign Role Collection**.

      ![assign role collection](bas7.png)

  7. Check your result. Now your user should have the **`Business_Application_Studio_Developer`** role collection assigned.

      ![assign role collection](bas8.png)

      You are now able to develop on SAP Business Application Studio.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create dev space)]

  1.  Select **trial** > **Service Marketplace**. Search for **SAP Business Application Studio** and select it.

      ![dev](studio.png)

  2.  Select actions and click **Go to Application**.

      ![dev](studio212.png)

  3.  Check the privacy statement and click **OK**.

      ![dev](studio2.png)

  4. Now the SAP Business Application Studio has started. Click **Create Dev Space**.

      ![dev](studio3.png)

  5. Create a new dev space:
       - Name: **Fiori**
       - Type: **SAP Fiori**

       Click **Create Dev Space**.

     ![dev](studio4.png)

  6. When your status is **Running**, select your dev space **Fiori**.

      ![dev](studio5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set up organization and space)]

  1. Now you are in your **Fiori** dev space in SAP Business Application Studio.
     Select **Open Folder** to set your workspace.

      ![organization](studio6.png)

  2. Select **projects** and click **Open**.

      ![organization](studio7.png)

  3. Select **View > Find Command**.

      ![organization](neuyy.png)

  4. Search for **CF: Login to Cloud Foundry** and select it.

      ![organization](neu2.png)

  5. Select your API endpoint and press enter.

      ![organization](neu3.png)

  6. Enter the same e-mail address you set in your trial instance and press enter.
      ![organization](neu4.png)

  7. Enter your password and press enter.

      ![organization](neu5.png)

  7. Select your global account and press enter.

      ![organization](neu6.png)

  8. Select dev as your space and press enter.

      ![organization](neu7.png)

  9. Check your result. Now your organization and space have been set.

     ![organization](neumm.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create list report object page)]

  1. Select **View** > **Find Command**.

    ![object](view.png)

  2. Select **Fiori: Open Application Generator**.  

    ![object](neu9.png)

  3. Select **List Report Object Page** and click **Next >**.

    ![object](neu10.png)

  5. Configure data source, system and service:
     - Data source: **Connect to an SAP System**
     - System: **`ABAP Environment on SAP Business Technology Platform`**
     - ABAP Environment: **`default_abap-trial`**
     - Service: **`ZUI_C_TRAVEL_M_XXX(1) - OData v2`**

     ![object](neu11.png)

     Click **Next >**.

     A destination for the `abap-trial` service instance is generated automatically.

  6. Select your main entity **`TravelProcessor`** and click **Next >**.

    ![object](neu12.png)

  7. Configure project attributes:  
     - Name: **`ztravel_app_xxx`**
     - Title: **Travel App XXX**
     - Description: **A Fiori application.**
     - Add deployment configuration: Yes
     - Add FLP configuration: Yes
     - Configure advanced options: No

     Click **Next >**.

    ![object](neu13.png)

    **HINT:** Your **application name must** begin with a `z letter` and **must** be in **lowercase letters**.

  8. Configure deployment:

       - Target: ABAP
       - Is this an SAP Business Technology Platform system? Yes
       - Target System URL: `<your_abap_system_url>`
       - Name: `ztravel_app_xxx`
       - Package: `ztravel_app_xxx`
       - Transport Request: `<your_transport_request>`
       - Deployment description: deployment xxx

      !![app](neu14.png)

      Click **Next >**.

    >**HINT: If you want to copy your transport request, please do following:**  Open Eclipse, search your package **`ZTRAVEL_APP_XXX`** and open it. Open your transport organizer to see your transport request. Copy your transport request for later use. You can find your **transport request** underneath the **Modifiable** folder.
    >      ![deploy](deploy3.png)

  9. Configure Fiori Launchpad:

       - Semantic Object: `ztravel_app_xxx`
       - Action: display
       - Title: Travel App XXX

      !![app](neu15.png)

      Click **Finish**.

 10. Now all files have been generated.

      !![app](new4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run SAP Fiori application for data preview)]

  1. Press the run button on the left side and select the **`Start ztravel_app_xxx`** run button to start your SAP Fiori application.

      ![run](studio24.png)

      **HINT**: An alternative to run the application is to open the terminal and enter: `npm start`.

  2. Your default browser will open. Click **Go** to see your result.

      ![run](studio30.png)

  3. Check your result.

     ![run](studio31.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy your application)]

1. Click **Files**, right-click your project and select **Open in Terminal**.

    ![deploy](neuxx.png)

2. Enter **`npm run deploy`** to deploy your application. When prompted, check deployment configuration and press y. Open the URL at the end of the deployment log in browser to preview the application.

    ![deploy](deploy5.png)

    When the deployment is successful, you will get these two information back as a result: **UIAD details** and **deployment successful**.

>**HINT: If you want to update your SAP Fiori Launchpad configuration, then you can o following steps:**
> 1. Go back to SAP Business Application Studio, select projects, right-click your project **`ztravel_app_xxx`** and select **Open in Terminal**.

>![deploy](deploy.png)

>  2. To add Fiori Launchpad content use this command, enter **`npx fiori add flp-config`**.

>     Add following information:

>       - Semantic Object: **`ztravel_app_xxx`**
       - Action: display
       - Title: Travel App XXX
       - Subtitle (optional): press enter

>       ![deploy](deploy2.png)

> 3.  Open Eclipse, search your package **`ZTRAVEL_APP_XXX`** and open it. Open your transport organizer to see your transport request. Copy your transport request for later use. You can find your **transport request** underneath the **Modifiable** folder.

>      ![deploy](deploy3.png)

>  4. Go back to SAP Business Application Studio and open the terminal again. To add `deploy config` details, enter **`npx fiori add deploy-config`**.

>     Add following information:

>      - Please choose the target: ABAP
      - Is this an SAP Business Technology Platform system?: Y
      - Destination: press enter for default
      - Is this an S/4 Cloud system? N
      - Name: press enter for default
      - Package: **`ztravel_app_xxx`**
      - Transport Request: **`<your_transport_request>`**
      - Deployment description: `deployment xxx`

>      ![deploy](deploy4.png)

>      The `ui5-deploy.yaml` will be generated as part of this `deploy config` command.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check BSP library and SAP Fiori Launchpad app descriptor item in Eclipse)]

  1. Open Eclipse and check the **BSP library** and **SAP Fiori Launchpad app descriptor item folder** in your package **`ZTRAVEL_APP_XXX`**. If you are not able to see BSP applications and SAP Fiori Launchpad app description items, refresh your package `ZTRAVEL_APP_XXX` by pressing `F5`.

    ![library](library.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Create IAM App and business catalog)]

  1. In Eclipse right-click your package **`ZTRAVEL_APP_XXX`** and select **New** > **Other Repository Object**.

      ![iam](iam.png)

  2. Search for **IAM App**, select it and click **Next >**.

      ![iam](iam2.png)

  3. Create a new IAM App:
     - Name: **`ZTRAVEL_IAM_XXX`**
     - Description: IAM App

      ![iam](iam3.png)

      Click **Next >**.

  4. Click **Finish**.

      ![iam](iam4.png)

  5. Select **Services** and add a new one.

      ![iam](iam5.png)

  6. Select following:
      - Service Type: `OData IWSG`
      - Service Name: `ZUI_C_TRAVEL_M_XXX_0001`    

      ![iam](iam6.png)

      Click **OK**.

      **Save** and **activate** your IAM app.

  7. Right-click your package **`ZTRAVEL_APP_XXX`** and select  **New** > **Other Repository Object**.

      ![catalog](catalog.png)

  8. Search for **Business Catalog**, select it and click **Next >**.

      ![catalog](catalog2.png)

  9. Create a new business catalog:
     - Name: **`ZTRAVEL_BC_XXX`**
     - Description: Business catalog

      ![catalog](catalog3.png)

      Click **Next >**.

 10. Click **Finish**.

      ![catalog](catalog4.png)

 11. Select **Apps** and add a new one.

      ![catalog](catalog5.png)

 12. Create a new business catalog:
     - IAM App: `ZTRAVEL_IAM_XXX_EXT`
     - Name: `ZTRAVEL_BC_XXX_0001`

      ![catalog](catalog6.png)

      Click **Next >**.

 13. Click **Finish**.

       ![catalog](catalog7.png)

 14. Click **Publish Locally** to publish your business catalog.

       ![catalog](catalog8.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Run SAP Fiori application)]

  1. Go back to SAP Business Application Studio and deploy your changes. Therefore right-click your project **`ztravel_app_xxx`** and select **Open in Terminal**.

    ![url](url.png)


  2. Enter **`npm run deploy`**. When prompted, check deployment configuration and press y.

      ![url](url2.png)

  3. Press **`CTRL and click the following link`** to open the URL in a browser.

      ![url](url3.png)

  4. Log in to ABAP Trial.

      ![url](url4.png)

  5. Click **Go**.

      ![url](url5.png)

  6. Check your result.

      ![url](url6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=abap-environment-deploy-cf-production" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
