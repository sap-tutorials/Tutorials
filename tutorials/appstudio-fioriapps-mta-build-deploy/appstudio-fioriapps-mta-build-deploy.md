---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, software-product>sap-fiori, programming-tool>sapui5, software-product>sap-btp-cloud-foundry-environment]
primary_tag: software-product>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

# Build and Deploy Your SAP Fiori App to SAP Business Technology Platform
<!-- description --> Build and deploy your SAP Fiori MTA project to your SAP BTP, Cloud Foundry environment.

## Prerequisites
- The SAP Fiori dev space is in status `RUNNING` and you opened it.
- You completed [Create an SAP Fiori App Using SAP Business Application Studio](appstudio-fioriapps-create).
- For the deployment step, additional prerequisites apply. You need to have the following available in the Cloud Foundry space to which you will log on (see [Add a New Entitlement to Your Subaccount](cp-cf-entitlements-add)):
    - Cloud Foundry Runtime: 1GB free
    - Destination: 1 free
    - HTML5 Applications: 1 free

## You will learn
  - How to build and deploy an application to SAP BTP, Cloud Foundry environment
  - How to configure Cloud Foundry settings in SAP Business Application Studio
  - How to run the deployed app from your space on SAP BTP, Cloud Foundry environment

## Intro
After a period of idle time the dev space is automatically stopped. In order to re-start the dev space open the [dev space manager](https://triallink.eu10.trial.applicationstudio.cloud.sap/), click the **Play** icon, and click the dev space.

The period for idle time for Factory accounts is different than for trial accounts.

---

### Build the application


1. In the **Explorer** pane, navigate to **`webapp` > `view` > `node_modules`**. Right-click the `mta.yaml` file and select **Build MTA Project**.

    <br><br><!-- border -->![build mta](BAS-Build-1-.png)

    >The build process creates a multi-target archive (`MTAR`) file in your project that packages all the project modules for deployment. You can find the `MTAR` file in the `FioriDemo/mta_archives` folder.

    <!-- border -->![terminal mbt build results](BAS-Build-2-.png)


### Set Cloud Foundry preferences


If you are not logged in to a Cloud Foundry space - Before you can deploy your new application, set your Cloud Foundry preferences.

1. In the left side bar, select the **Hamburger icon | View | Command Palette…** to open the **command palette**.

    <!-- border -->![Command Palette-Login to CF](BAS-CF-Login-1-.png)    

2. Select the command **CF: Login to cloud foundry**.

    >Type `cf` to filter commands.

    <!-- border -->![Command Palette-Login to CF](BAS-CF-Login-2-.png)

3. A **Cloud Foundry Sign In** tab opens in SAP Business Application Studio. Select the API endpoint, provide your credentials (email and password), and click **Sign in**. If you don't know your API endpoint open the SAP Business Technology Platform Cockpit and navigate to your subaccount. Click on Overview on the left and you can see the URL of the API endpoint under "Cloud Foundry Environment".

    <!-- border -->![Cloud Foundry Login dialog](BAS-CF-Login-3-.png)

4. Select the Cloud Foundry organization, Cloud Foundry space, and click **Apply**.

    If the Cloud Foundry Target dropdowns are empty you need to change the default Cloud Foundry Endpoint to the one of your subaccount (see previous step). To obtain your subaccount’s Cloud Foundry Endpoint open the SAP Business Technology Platform Cockpit and navigate to your subaccount. Click on Overview on the left and you can see the URL of the API endpoint under “Cloud Foundry Environment”.

    <!-- border -->![Cloud Foundry Login dialog](BAS-CF-Login-4-.png)

    >**You have been logged in.** notification appears at the bottom-right of your screen.

    ><!-- border -->![Logged in to CF](BAS-CF-Login-5-.png)


### Deploy the application


Deploy your application to SAP BTP, Cloud Foundry environment.

1. In the **Explorer** pane under the folder `mta_archives`, right-click the `mtar` file and select **Deploy MTA Archive**.

    <!-- border -->![deploy mtar](BAS-Deploy-1-.png)

    >The application deployment to the space you are connected to starts and a notification appears. You can follow the deployment progress in the **Task: Deploy** console at the bottom of your screen.

3. Wait for the deployment to complete.

    >The deployment process takes a few minutes. When the deployment process is complete, the notifications **Process finished.** and **Terminal will be reused by tasks.** will appear at the bottom of the **Task: Deploy** console.

    ><!-- border -->![deploy success](BAS-Deploy-2-.png)

    If you have issues with the deployment, check out the [SAP Business Application Guided Answers](https://ga.support.sap.com/dtp/viewer/index.html#/tree/2827/actions/41344) for troubleshooting.


### Access the application on SAP BTP, Cloud Foundry environment


Run the deployed app on SAP BTP. The steps below show you how to access your new application and run it. You will use CF commands in a terminal for this.

1. In the left side bar, select the **Hamburger icon | Terminal | New Terminal**.

    <!-- border -->![open new terminal tab](BAS-Access-App-On-CF-1-.png)

2. A new terminal tab is opened.

    <!-- border -->![new terminal tab opened](BAS-Access-App-On-CF-2-.png)

    >The folder it is opened in is the specific project folder. You can use the following command in the terminal to verify it:
    ```Shell/Bash
    pwd
    ```
    <!-- border -->![pwd](BAS-Access-App-On-CF-3-.png)

3. Open the `mta.yaml` file, and locate the destination service instance name. You can find it in the **modules > requires** section or in the **resources** section. In this tutorial it should be `FioriDemo-destination-service`.

4. Execute the following command in the terminal to get the details of the deployed application and its URL:

    ```Shell/Bash
    cf html5-list -di FioriDemo-destination-service -u -rt launchpad
    ```

    <!-- border -->![details of deployed app on CF](BAS-Access-App-On-CF-4-.png)

    >To find out more about this command execute in the terminal:
    ```Shell/Bash
    cf help html5-list
    ```

5. Press [CTRL] and click the app's link in the terminal.

    <!-- border -->![app url](BAS-Access-App-On-CF-5-.png)

    >You can use this URL in any browser to access your new application in your space on SAP BTP, Cloud Foundry environment.

    >Here we are using the [SAP Launchpad service](https://discovery-center.cloud.sap/serviceCatalog/launchpad-service) you configured in an earlier step. It enables organizations to establish a central point of access to SAP (e.g. SAP S/4HANA), custom-built, and third party applications and extensions, both on the cloud and on premise.

6. The app is running on SAP BTP, Cloud Foundry environment, accessing data from an on-premise backend.

    <!-- border -->![app running on cf](BAS-Access-App-On-CF-6-.png)



---

Congratulations!

With this, you have successfully completed the deployment of your SAP Fiori app to SAP BTP using SAP Business Application Studio.

In this tutorial, you used high productivity tools that are available out-of-the-box in SAP Business Applications Studio that make it easy to build and deploy applications as well as work in the Cloud Foundry environment.


---
