---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>javascript, programming-tool>sapui5, programming-tool>html5, software-product>sap-business-technology-platform, software-product>sap-business-application-studio]
primary_tag: software-product>sap-btp--cloud-foundry-environment
author_name: Conrad Bernal
author_profile: https://github.com/cjbernal
---

# Deploy Your First SAPUI5 App
<!-- description --> Create, build, and deploy an MTA project with an integrated SAPUI5 module to SAP BTP, Cloud Foundry environment

## You will learn
  - How to create an SAPUI5 project
  - How to build a project for Cloud Foundry
  - How to deploy a project to Cloud Foundry

---

### Open the Fiori dev space


**Start** a dev space of kind *SAP Fiori* and **open** it by clicking its name.

<!-- border -->![selectDevSpace](./selectDevSpace.png)

> Have a look at [this tutorial](appstudio-devspace-fiori-create) if you are unsure how to get here or how to create a dev space.

### Connect to a Cloud Foundry endpoint

Make sure you are connected to a Cloud Foundry endpoint to which you will deploy the SAPUI5 application later.

1. Enter the Cloud Foundry endpoint you want to use. If your environment runs in the EU10-region, this is `https://api.cf.eu10.hana.ondemand.com`. To do so, use the `CF: Login to Cloud Foundry` command. From the menu, select **View** and then **Find Command**. Type `CF: Login` and then select `CF: Login to Cloud Foundry`. Select your endpoint or enter the value for your region and then enter your email and password to proceed.

    <!-- border -->![selectEndpoint](./selectEndpoint.png)

2. Next, you need to select the Cloud Foundry Organization and space you want use. You will see that you are connected to the endpoint once these prompts have been answered.

    <!-- border -->![connectedEndpoint](./connectedEndpoint.png)


### Create a new single-module-project
This step will guide you through the needed actions to create a project that contains a **single** SAPUI5 application. In case you want to create a project that contains multiple UI modules, please do not follow these instructions and rather create an empty MTA project to which you then add multiple UI modules.


1. Click on the link **Start from template** on the *Welcome* screen.

    <!-- border -->![newproject](./newproject.png)

2. Select **SAP Fiori freestyle SAPUI5 application** as the template category you want to use and click **Start**.

    <!-- border -->![fioriTemplate](./fioriTemplate.png)

3. Specify the application type **SAPUI5 freestyle** and the floor plan **SAPUI5 Application** and go to the **Next** screen.

    <!-- border -->![sapui5Template](./sapui5app.png)

4. Now you have the option to connect your SAPUI5 application to a data source. As we won't need a data source in this tutorial, select **None** and click **Next**.

    <!-- border -->![nodata](./nodata.png)

4. Keep the default view name and click **Next** .

    <!-- border -->![viewname](./viewname.png)

4.   Name of the module **`sapui5`**, use the application title  **`Tutorial`**, define the namespace **`sap.btp`**, and **Add deployment configuration**. Keep the default values for the other parameters and select **Next** to go to the next step.

    <!-- border -->![projectdetails](./projectdetails.png)

4.  Choose **Cloud Foundry** as the target runtime and select **None** for the destination and make sure that **Add application to managed application router** is selected. Then, press **Finish** to create the new project.

    <!-- border -->![finishProject](./finishProject.png)


4. Once you see the success message, click **File | Open Workspace...** to open a new dialog.

    <!-- border -->![openws](./openWs1.png)

    In there, select the generated folder **sapui5** and hit **open** to see the new project.

    <!-- border -->![openws](./openWs2.png)


### Build the application

Build (aka package) the project to a `mtar` archive to deploy it to Cloud Foundry.  

1. Right-click on the `mta.yaml` file and select **Build MTA** to trigger this process.

    <!-- border -->![build](./build.png)

3. Once the build is complete, you can see a message in the log. You can now find the generated `mtar` archive in the project tree under `mta_archives`.

    <!-- border -->![state](./buildsuccess.png)


### Deploy the archive to Cloud Foundry

Now that you created a `mtar` archive, you are all set to deploy the application.

1. Right-click on the `mtar` file and select **Deploy** and **Deploy MTA Archive**.

    <!-- border -->![deploy](./deploy.png)

2. Check the console output to make sure the process started.

3. You will see a success message and the URL of the app in the log once the deployment finished.

    <!-- border -->![success](./deploysuccess.png)

4.   You can see the URL of the deployed app when running `cf html5-list -di sap-btp-sapui5-dest-srv -u --runtime launchpad` in a new terminal session.

    <!-- border -->![cfapps](./cfhtml5.png)

    > Use the following command in case you use the Portal service
    >
    >  `cf html5-list -di sap-btp-sapui5-destination-service -u  --runtime cpp`


### Test to the application

1. **Open** the application in your browser. You might need to log in with your SAP ID (the same credentials you use for the SAP BTP Cockpit).
>There is temporarily a need to update the given URL to view the application. When pasting the URL into your browser, remove `cpp` from your URL and replace it with `launchpad`.


2. See that the sample application consists of a header and an empty page. So you should see something like this:

<!-- border -->![app](./app.png)


Enter the URL of your running application:

