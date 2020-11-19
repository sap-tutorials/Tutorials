---
title: Build, Deploy and Test Notification and Decision Support Application
description: Build and deploy a multi-target application (MTA) project in SAP Web IDE, generate a test notification in SAP IoT, and launch the decision support application.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-cloud-platform-portal, products>sap-leonardo-iot, products>sap-web-ide, products>sap-cloud-platform-internet-of-things ]
primary_tag: topic>internet-of-things
---

## Details
### You will learn
  - How to build and deploy an MTA project
  - How to test SAP IoT notification
  - How to test decision support application

If you make any changes to an MTA project, you'll need to build and deploy the project for the changes to take effect.

---

[ACCORDION-BEGIN [Step 1: ](Build MTA project)]

  1. In SAP Web IDE, right click the iot-ds project and click **Build** to start the build of the project.

    ![Initiate project build](/images/shared/webide_build_0_1.png)

  2. When the build is completed, a notification will be displayed at the top right corner of the SAP Web IDE.  

  3. You can also check the status of the build by viewing the logs in the SAP Web IDE console.

    ![Project is built successfully](/images/shared/webide_build_1.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy MTA project)]

  1. You need to have the following entitlements in your Cloud Foundry sub-account to **deploy** and **run** our application.  You can find more information on how to configure entitlements in [Configure entitlements and Quotas for Subaccounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html).

    Entitlement | Unit
    ------------ | -------------
    Portal|1 (or unlimited)
    Application Runtime Memory| 1 Gib (Minimum)
    Destination|1

  2. To start a deployment, expand the `mta_archives` folder in your project, right click the `mtar` file inside the folder and click **Deploy to SAP Cloud Platform**.

    ![Deploy project to SAP Cloud Platform](/images/shared/webide_build_3.png)

  3. Select the appropriate **Cloud Foundry API Endpoint**, **Organization** and **Space** for your deployment.  If you cannot find your selections, please check your Cloud Foundry settings in SAP Web IDE preference.  For more information, please see step 6 and step 7 of [Prepare SAP Web IDE for Cloud Foundry Development](sapui5-webide-open-webide).

  4. Click **Deploy**.

    ![Enter SAP Cloud Platform information](/images/shared/webide_build_4.png)

  5. When the deployment is completed, a notification will be displayed at the top right corner of the SAP Web IDE.

  6. You can also check the status of the deployment by viewing the logs in the SAP Web IDE console.

    ![Project is deployed successfully](/images/shared/webide_build_5.png)  

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Retrieve thing id)]

  We will need to retrieve the thing id for the next step. If you already have the thing id, please proceed to the next step.

  1. From SAP IoT launchpad, select the **Thing Modeler** tile.

    ![Select Thing Modeler in SAP Fiori launchpad](/images/shared/thing_modeler_tile.png)  

  2. Select the `greenhouse` package and select **Things** on the left panel.

  3. Click the **Connectivity Information** icon on the top right corner.

    ![Select greenhouse package and click connectivity information](/images/shared/thing_modeler_1.png)  

  4. Copy the thing id.  We'll use it in the next step.

    ![Copy thing id](/images/shared/thing_id.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Generate test notification)]

  1. From SAP IoT launchpad, select the **Actions** tile.

    ![Select actions from SAP Fiori launchpad](/images/shared/launchpad_tile_actions.png)

  2. Select **Greenhouse Action**.

    ![Select greenhouse action](/images/shared/test_portal_1_1.png)

  3. Click **Test**.

    ![Click Test](/images/shared/test_portal_2.png)

  4. Enter the **Thing Id** in the **Test Action** dialog. Click **Test**.

    ![Enter thing id and click Test](/images/shared/test_portal_3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check notification in launchpad site)]

  1. To find the URL of the SAP Fiori launchpad site, go to the SAP Cloud Platform Cockpit, navigate to the space of the sub-account where the MTA project is deployed.

  2. Navigate to the **Applications** screen.

  3. Click `iot-ds_appRouter`.

    ![Find deployed application in SAP Cloud Platform cockpit](/images/shared/find_portal_url_1.png)

  4. You should see the route displayed under **Application Routes**.

    ![Find application route](/images/shared/find_portal_url_2.png)

  5. Append `/cp.portal/site` at the end of the application route to create the URL of the SAP Fiori launchpad site.

    >For example, if:
    >The application route is:
    >
    >**`mytenant-space1-approuter1.cfapps.eu10.hana.ondemand.com`**
    >
    > then the SAP Fiori launchpad site URL would be:
    >
    >**`https://mytenant-space1-approuter1.cfapps.eu10.hana.ondemand.com/cp.portal/site`**

  6. Launch the SAP Fiori launchpad site in a browser.  You should see a SAP Fiori notification alert.

    ![Notification alert displayed in SAP Fiori launchpad](/images/shared/test_notif_1.png)

  7. Click the alert icon and the SAP Fiori notification panel will be opened.

    ![Open notification](/images/shared/test_notif_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Launch application from SAP Fiori notification)]

>Please complete the tutorial [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui) before continuing with this step.

In SAP Fiori notification panel, click the `High Greenhouse Temperature Alert` notification.  If you do not see any notification in Panel, please follow in instructions in step 3 to generate a new notification.

![Notification shown in SAP Fiori launchpad](/images/shared/test_notif_2.png)

The decision support application will be launched.  

Here's a summary of screen elements:

  - The application title ``Greenhouse Alert`` is configured in step 1 of [Building the Decision Support UI in the Web IDE](iot-ds-3-create-ui).

  - The header title ``Greenhouse High Temperature Alert`` is configured in step 4 of [Model the needed Decision Support based on IoT data](iot-ds-1-define-actions).

  - The action options ``Service Ticket Quick Create`` and ``Fiori Navigation`` are configured in step 5 and step 6 of [Model the needed Decision Support based on IoT data](iot-ds-1-define-actions).

  ![Decision Support app runtime](/images/ds_runtime_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Initiate quick create option)]

  1. Click the option ``Service Ticket Quick Create``.

    ![Quick create possible action](/images/qc_start_1.png)

  2. Enter a description in the field **Description**.

  3. Click ``Initiate Action``.

    ![Initiate quick create possible action](/images/qc_start_2.png)

  4. The quick create action is executed and the action history is saved in execution result.

    ![Execution of possible action recorded in execution result](/images/qc_start_3_2.png)

  5. Click **Details** to see additional information about the executed action.

    ![Display additional information about executed action](/images/qc_start_4_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Initiate SAP Fiori navigation option)]

  1. Click the option `SAP Fiori Navigation`

  2. Click `Initiate Action`.

    ![Initiate SAP Fiori navigation possible action](/images/nav_start_2_1.png)

  3. A new browser tab is opened to the home page of the SAP Fiori launchpad site.

  4. Open the previous browser tab, a new action history is saved in execution result.

    ![Execution of possible action recorded in execution result](/images/nav_start_1_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test with device simulator)]

  1. Start the Node.js simulator for greenhouse. This Node.js simulator was created in step 6 of the tutorial [Create a Simple IoT Device Model](iot-express-2-create-device-model) .

  2. If the device simulator starts correctly, your console log should look like this:

    ![Console of a running device simulator ](/images/dataok.png)

  3. (Optional) You can check the temperatures that are generated by the simulator in IoT Service Cockpit. To open IoT Service Cockpit, go to the Cloud Foundry space of your sub-account in SAP Cloud Platform Cockpit. In **Service Instances**, click the `Open Dashboard` icon in the **Actions** column of the `InternetOfThings` entry in the table.

    ![IoT service instance in SAP Cloud Platform cockpit](/images/check_device_data-1.png)

    Log in to the IoT Service Cockpit.  Select your tenant.

      ![IoT service cockpit](/images/check_device_data-2.png)

    Go to the Devices section, select your device.

      ![Select greenhouse device](/images/check_device_data-4.png)

    In the **Data Visualization** section, select the sensor type, temperature properties and table mode. Click the refresh icon to refresh the data.

      ![Display data visualization of simulated data](/images/check_device_data-3.png)

  4. Launch the SAP Fiori launchpad site.

  5. When a generated temperature satisfies our rule, you should see a new notification alert in the launchpad site. Select the notification and launch the decision support application.


[DONE]
[ACCORDION-END]

---
