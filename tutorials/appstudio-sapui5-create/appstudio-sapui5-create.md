---
title: Deploy Your First SAPUI5 App
description: Create, build, and deploy an MTA project with an integrated SAPUI5 module to SAP BTP, Cloud Foundry environment
auto_validation: true
time: 25
tags: [ tutorial>beginner, topic>javascript, topic>sapui5, topic>html5, products>sap-business-technology-platform, products>sap-business-application-studio]
primary_tag: products>sap-btp--cloud-foundry-environment
author_name: Conrad Bernal
author_profile: https://github.com/cjbernal
---

## Details
### You will learn
  - How to create an SAPUI5 project
  - How to build a project for Cloud Foundry
  - How to deploy a project to Cloud Foundry

---

[ACCORDION-BEGIN [Step : ](Open the Fiori dev space)]


**Start** a dev space of kind *SAP Fiori* and **open** it by clicking its name.

!![selectDevSpace](./selectDevSpace.png)

> Have a look at [this tutorial](appstudio-devspace-fiori-create) if you are unsure how to get here or how to create a dev space.


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Connect to a Cloud Foundry endpoint)]

Make sure you are connected to a Cloud Foundry endpoint to which you will deploy the SAPUI5 application later.

1. Click on the bottom-left corner of the screen to start the connection flow.

    !![notConnected](./notConnected.png)

2. Enter the Cloud Foundry endpoint you want to use. If your environment runs in the EU10-region, this is `https://api.cf.eu10.hana.ondemand.com`. Enter your email and your password to proceed.

    !![selectEndpoint](./selectEndpoint.png)

3. Next, you need to select the Cloud Foundry Organization and space you want use. You will see that you are connected to the endpoint once these prompts have been answered.

    !![connectedEndpoint](./connectedEndpoint.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Create a new single-module-project)]
This step will guide you through the needed actions to create a project that contains a **single** SAPUI5 application. In case you want to create a project that contains multiple UI modules, please do not follow these instructions and rather create an empty MTA project to which you then add multiple UI modules.


1. Click on the link **Start from template** on the *Welcome* screen.

    !![newproject](./newproject.png)

2. Select **SAP Fiori freestyle SAPUI5 application** as the template category you want to use and click **Start**.

    !![fioriTemplate](./fioriTemplate.png)

3. Specify the application type **SAPUI5 freestyle** and the floor plan **SAPUI5 Application** and go to the **Next** screen.

    !![sapui5Template](./sapui5app.png)

4. Now you have the option to connect your SAPUI5 application to a data source. As we won't need a data source in this tutorial, select **None** and click **Next**.

    !![nodata](./nodata.png)

4. Keep the default view name and click **Next** .

    !![viewname](./viewname.png)

4.   Name of the module **`sapui5`**, use the application title  **`Tutorial`**, define the namespace **`sap.btp`**, and **Add deployment configuration**. Keep the default values for the other parameters and select **Next** to go to the next step.

    !![projectdetails](./projectdetails.png)

4.  Choose **Cloud Foundry** as the target runtime and select **None** for the destination and make sure that **Add application to managed application router** is selected. Then, press **Finish** to create the new project.

    !![finishProject](./finishProject.png)


4. Once you see the success message, click **File | Open Workspace...** to open a new dialog.

    !![openws](./openWs1.png)

    In there, select the generated folder **sapui5** and hit **open** to see the new project.

    !![openws](./openWs2.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Build the application)]

Build (aka package) the project to a `mtar` archive to deploy it to Cloud Foundry.  

1. Right-click on the `mta.yaml` file and select **Build MTA** to trigger this process.

    !![build](./build.png)

3. Once the build is complete, you can see a message in the log. You can now find the generated `mtar` archive in the project tree under `mta_archives`.

    !![state](./buildsuccess.png)



[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Deploy the archive to Cloud Foundry)]

Now that you created a `mtar` archive, you are all set to deploy the application.

1. Right-click on the `mtar` file and select **Deploy** and **Deploy MTA Archive**.

    !![deploy](./deploy.png)

2. Check the console output to make sure the process started.

3. You will see a success message and the URL of the app in the log once the deployment finished.

    !![success](./deploysuccess.png)

4.   You can see the URL of the deployed app when running `cf html5-list -di sap-btp-sapui5-destination-service -u  --runtime launchpad` in a new terminal session.

    !![cfapps](./cfhtml5.png)

    > Use the following command in case you use the Portal service
    >
    >  `cf html5-list -di sap-btp-sapui5-destination-service -u  --runtime cpp`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Test to the application)]

1. **Open** the application in your browser. You might need to log in with your SAP ID (the same credentials you use for the SAP BTP Cockpit).


2. See that the sample application consists of a header and an empty page. So you should see something like this:

!![app](./app.png)


Enter the URL of your running application:

[VALIDATE_1]
[ACCORDION-END]
