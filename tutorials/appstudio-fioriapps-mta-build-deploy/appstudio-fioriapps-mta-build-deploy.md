---
title: Build and Deploy Your SAP Fiori App to SAP Business Technology Platform
description: Build and deploy your SAP Fiori MTA project to your SAP BTP, Cloud Foundry environment.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-business-technology-platform, products>sap-fiori, topic>sapui5, products>sap-btp-cloud-foundry-environment]
primary_tag: products>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

## Prerequisites
- The SAP Fiori dev space is in status `RUNNING` and you opened it.
- You completed [Create an SAP Fiori App Using SAP Business Application Studio](appstudio-fioriapps-create).
- For the deployment step, additional prerequisites apply. You need to have the following available in the Cloud Foundry space to which you will log on (see [Add a New Entitlement to Your Subaccount](cp-cf-entitlements-add)):
    - Cloud Foundry Runtime: 1GB free
    - Destination: 1 free
    - HTML5 Applications: 1 free

## Details
### You will learn
  - How to build and deploy an application to SAP BTP, Cloud Foundry environment
  - How to configure Cloud Foundry settings in SAP Business Application Studio
  - How to run the deployed app from your space on SAP BTP, Cloud Foundry environment

After a period of idle time the dev space is automatically stopped. In order to re-start the dev space open the [dev space manager](https://triallink.eu10.trial.applicationstudio.cloud.sap/), click the **Play** icon, and click the dev space.

The period for idle time for Factory accounts is different than for trial accounts.

---

[ACCORDION-BEGIN [Step 1: ](Build the application)]

1. In the **Explorer** pane, right-click the `mta.yaml` file and select **Build MTA Project**.

    <br><br>!![build mta](AppStudio-Context-Menu-Build-MTA-.png)<br>

    >The build process creates a multitarget archive (`MTAR`) file in your project that packages all the project modules for deployment. You can find the `MTAR` file in the `FioriDemo/mta_archives` folder.

    !![terminal mbt build results](07-02-02-AppStudio-Terminal-MBT-Build-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set Cloud Foundry preferences)]

If you are not logged in to a Cloud Foundry space - Before you can deploy your new application, set your Cloud Foundry preferences.

1. In the menu bar, select **View | Find Command** to open the **command palette**.

    !![Command Palette-Login to CF](08-01-AppStudio-CF-Login-.png)

2. Select the command **CF: Login to cloud foundry**.

    >Type `cf` to filter commands.

    !![Command Palette-Login to CF](08-01-02-AppStudio-CF-Login-.png)

3. When prompted, provide your credentials, select the API endpoint, organization, and space for your project.

    >The Cloud Foundry organization and space appear in the status line at the bottom left part of the screen.

    !![Logged in to CF](AppStudio-CF-Login-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy the application)]

Deploy your application to SAP BTP, Cloud Foundry environment.

Right-click the `mtar` file and select **Deploy MTA Archive**.

!![deploy mtar](03-AppStudio-Fiori-Project-Deploy-.png)

>The application deployment to the space you are connected to starts and a notification appears. The deployment process takes a few minutes. You can see that the deployment is still in progress in the **Task: Deploy** console at the bottom right of your screen.

>When the deployment process is complete, a notification will temporarily appear at the bottom-right part of the screen.

>!![deploy success notification](AppStudio-Fiori-Project-Deploy-Success-Notification-.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access the application on SAP BTP, Cloud Foundry environment)]

Run the deployed app on SAP BTP. The steps below show you how to access your new application and run it. You will use CF commands in a terminal for this.

1. On the menu bar select **Terminal | New Terminal**.

    !![open new terminal tab](04-01-AppStudio-Open-Terminal-.png)

2. A new terminal tab is opened.

    !![new terminal tab opened](04-02-AppStudio-New-Terminal-Tab-Opened-.png)

    >The folder it is opened in is the specific project folder. You can use the following command in the terminal to verify it:
    ```Shell/Bash
    pwd
    ```
    !![pwd](04-02-AppStudio-PWD-.png)

3. Open the `mta.yaml` file, and locate the destination service instance name. You can find it in the **modules > requires** section or in the **resources** section. In this tutorial it should be `FioriDemo-destination-service`.

4. Execute the following command in the terminal to get the details of the deployed application and its URL:

    ```Shell/Bash
    cf html5-list -di FioriDemo-destination-service -u
    ```

    !![details of deployed app on CF](AppStudio-CF-HTML5-List-.png)

    <br><br>!![app url](AppStudio-CF-HTML5-List--.png)

    >To find out more about this command execute in the terminal:
    ```Shell/Bash
    cf help html5-list
    ```

5. As a temporary step you need to edit the URL. Copy the app's URL, and paste it in a new browser tab (do not click [ENTER]). Replace `cpp` with `launchpad`.

    !![app url](AppStudio-AppURL-in-New-Tab-.png)

    >You can use this URL in any browser to access your new application in your space on SAP BTP, Cloud Foundry environment.

    >Here we are using the [SAP Launchpad service](https://discovery-center.cloud.sap/serviceCatalog/launchpad-service) you configured in an earlier step. It enables organizations to establish a central point of access to SAP (e.g. SAP S/4HANA), custom-built, and third party applications and extensions, both on the cloud and on premise.

6. The app is running on SAP BTP, Cloud Foundry environment, accessing data from an on-premise backend.

    !![app running on cf](AppStudio-App-Running-on-CF-.png)

[VALIDATE_10]
[ACCORDION-END]

---

Congratulations!

With this, you have successfully completed the deployment of your SAP Fiori app to SAP BTP using SAP Business Application Studio.

In this tutorial, you used high productivity tools that are available out-of-the-box in SAP Business Applications Studio that make it easy to build and deploy applications as well as work in the Cloud Foundry environment.


---
