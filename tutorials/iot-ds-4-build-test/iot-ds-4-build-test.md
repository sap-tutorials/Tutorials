---
title: Build, Deploy and Test Notification and Decision Support Application
description: Build and deploy a multi-target application (MTA) project in SAP Web IDE, generate a test notification in SAP Leonardo IoT, and launch the Decision Support application.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-cloud-platform-portal, products>sap-leonardo-iot, products>sap-web-ide]
primary_tag: products>sap-cloud-platform-internet-of-things
---

## Prerequisites

## Details
### You will learn
  - How to build and deploy an MTA project
  - How to test Leonardo IoT Notification
  - How to test Decision Support application

If you make any changes to an MTA project, you'll need to build and deploy the project for the changes to take effect.

---

[ACCORDION-BEGIN [Step 1: ](Build MTA project)]

  1. In Web IDE, right click on the iot-ds project and click **Build** to start the build of the project.

    ![WebIDE Build](/images/shared/webide_build_0_1.png)

  2. When the build is completed, a notification will be displayed at the top right corner of the Web IDE.  

  3. You can also check the status of the build by viewing the logs in the Web IDE console.

    ![WebIDE Build](/images/shared/webide_build_1.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy MTA project)]

  1. You need to have the following Entitlements in your Cloud Foundry sub-account to **deploy** and **run** our application.  You can find more information on how to configure Entitlements in [Configure Entitlements and Quotas for Subaccounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html).

    Entitlement | Unit
    ------------ | -------------
    Portal|1 (or unlimited)
    HTML5 Application|2
    Application Runtime Memory| 1 Gib (Minimum)
    Destination|1

  2. To start a deployment, expand the `mta_archives` folder in your project, right click on the `mtar` file inside the folder and click **Deploy**.

    ![WebIDE Build](/images/shared/webide_build_3.png)

  3. Select the appropriate **Cloud Foundry API Endpoint**, **Organization** and **Space** for your deployment.  If you cannot find your selections, please check your Cloud Foundry settings in Web IDE preference.  For more information, please see Step 6 and Step 7 of [Prepare SAP Web IDE for Cloud Foundry Development](sapui5-webide-open-webide).

  4. Click **Deploy**.

    ![WebIDE Build](/images/shared/webide_build_4.png)

  5. When the deployment is completed, a notification will be displayed at the top right corner of the Web IDE.

  6. You can also check the status of the deployment by viewing the logs in the Web IDE console.

    ![WebIDE Build](/images/shared/webide_build_5.png)  

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Retrieve thing ID)]

  We will need to retrieve the Thing Id for the next Step. If you already have the Thing Id, please proceed to the next step.

  1. From Leonardo IoT Launchpad, select the Thing Modeler tile.

    ![Thing Modeler Tile 1](/images/shared/thing_modeler_tile.png)  

  2. Select the `greenhouse` package and select **Things** on the left panel.

  3. Click on the **Connectivity Information** icon on the top right corner.

    ![Thing Modeler 1](/images/shared/thing_modeler_1.png)  

  4. Copy the Thing Id.  We'll use it in the next step.

    ![Thing Id](/images/shared/thing_id.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Generate test notification)]

  1. From Leonardo IoT Launchpad, select the **Actions** tile.

    ![Actions Tile](/images/shared/launchpad_tile_actions.png)

  2. Select **Greenhouse Action**.

    ![Actions Tile](/images/shared/test_portal_1_1.png)

  3. Click **Test** button.

    ![Actions Tile](/images/shared/test_portal_2.png)

  4. Enter the **Thing Id** in the Test Action dialog. Click **Test**.

    ![Actions Tile](/images/shared/test_portal_3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check notification in Fiori Launchpad site)]

  1. To find the URL of the Fiori Launchpad Site, go to the SAP Cloud Platform Cockpit, navigate to the space of the sub-account where the MTA project is deployed.

  2. Navigate to the **Applications** screen.

  3. Click on `iot-ds_appRouter`.

    ![Find Portal Url 1](/images/shared/find_portal_url_1.png)

  4. You should see the route displayed under **Application Routes**.

    ![Find Portal Url 1](/images/shared/find_portal_url_2.png)

  5. Append `/cp.portal/site` at the end of the Application Route to create the URL of the Fiori Launchpad Site.

    For example:

      Application Route:
      mytenant-space1-approuter1.cfapps.eu10.hana.ondemand.com

      The Fiori Launchpad Site URL will be
      `https://mytenant-space1-approuter1.cfapps.eu10.hana.ondemand.com/cp.portal/site`

  6. Launch Fiori Launchpad Site in a browser.  You should see a Fiori notification alert.

    ![Find Portal Url 1](/images/shared/test_notif_1.png)

  7. Click on the alert icon and the Fiori Notification Panel will be opened.

    ![Find Portal Url 2](/images/shared/test_notif_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Launch application from Fiori notification)]

Please complete the tutorial [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui) before continuing with this step.

  * In Fiori Notification Panel, click on the `High Greenhouse Temperature Alert` notification.  If you do not see any notification in Panel, please follow in instructions in step 3 to generate a new notification.

    ![Find Portal Url 2](/images/shared/test_notif_2.png)

  * The Decision Support application will be launched.  

  * Summary of screen elements:
    * The application title ``Greenhouse Alert`` is configured in Step 1 of [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui).

    * The header title ``Greenhouse High Temperature Alert`` is configured in Step 4 of [Model the needed Decision Support based on IoT data](iot-ds-1-define-actions).

    * The Action Options ``Service Ticket Quick Create`` and ``Fiori Navigation`` are configured in Step 5 and Step 6 of [Model the needed Decision Support based on IoT data](iot-ds-1-define-actions).

        ![DS Runtime 1](/images/ds_runtime_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Initiate Quick Create option)]

  1. Click the option ``Service Ticket Quick Create``.

    ![QC Start 1](/images/qc_start_1.png)

  2. Enter a description in the field **Description**.

  3. Click ``Initiate Action``.

    ![QC Start 3](/images/qc_start_2.png)

  4. The Quick Create action is executed and the action history is saved in Execution Result.

    ![QC Start 3](/images/qc_start_3_1.png)

  5. Click **Details** to see additional information about the executed action.

    ![QC Start 4](/images/qc_start_4_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Initiate Fiori navigation option)]

  1. Click the option `Fiori Navigation`

  2. Click `Initiate Action`.

    ![Nav Start 1](/images/nav_start_2.png)

  3. A new browser tab is opened to the Home page of the Fiori Launchpad Site.

  4. Open the previous browser tab, a new action history is saved in Execution Result.

    ![Nav Start 2](/images/nav_start_1_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test with device simulator)]

  1. Start the Node.js simulator for greenhouse. This Node.js simulator was created in Step 6 of the tutorial [Create a Simple IoT Device Model](iot-express-2-create-device-model) .

  2. If the device simulator starts correctly, your console log should look like this:

    ![Test Sim 1](/images/dataok.png)

  3. (Optional) You can check the temperatures that are generated by the simulator in IoT Service Cockpit. To open IoT Service Cockpit, go to the Cloud Foundry space of your sub-account in SAP Cloud Platform Cockpit. In Service Instances, click the `Open Dashboard` icon in the **Actions** column of the `InternetOfThings` entry in the table.

    ![Test Sim 2](/images/check_device_data-1.png)

    Log in to the IoT Service Cockpit.  Select your tenant.

    ![Test Sim 2](/images/check_device_data-2.png)

    Go to the Devices section, select your device.

      ![Test Sim 3](/images/check_device_data-4.png)

    In the Data Visualization section, select the sensor type, temperature properties and Table mode. Click on the Refresh icon to refresh the data.

      ![Test Sim 4](/images/check_device_data-3.png)

  4. Launch Fiori Launchpad Site.

  5. When a generated temperature satisfies our Rule, you should see a new notification alert in Launchpad Site. Select the notification and launch the Decision Support application.


[DONE]
[ACCORDION-END]

---
