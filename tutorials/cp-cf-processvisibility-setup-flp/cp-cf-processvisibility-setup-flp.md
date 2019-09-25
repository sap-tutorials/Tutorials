---
title: Make Available the Process Visibility Applications Using SAP Fiori Launchpad
description: Create tiles on SAP Fiori launchpad for  the applications of Process Visibility for building scenarios, tracking business processes, and monitoring events and scenarios.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-cloud-platform]
primary_tag: topic>cloud
author_name: Deeksha R
author_profile: https://github.com/Deeksha-R
---

### Prerequisites
  - You must have a trial account on SAP Cloud Platform.
  - You have created a service instance of Process Visibility and have noted the Process Visibility service instance name. For more information, see [Create a Service Instance of SAP Cloud Platform Process Visibility](cp-cf-processvisibility-setup-serviceinstance).

### Details
### You will learn
- How to configure your multi-target application to view Process Visibility tiles in SAP Fiori launchpad

The UIs of Process Visibility must be configured on SAP Web IDE Full-Stack. After the configuration, the default tiles of Process Visibility will be available on SAP Fiori launchpad. For more information, see [Consuming UI Applications on SAP Fiori Launchpad](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/27850a2be7834ccfa209a2a3aabd216e.html).

---

[ACCORDION-BEGIN [Step 1: ](Download MTA files from GitHub)]
1. Use the following link to access the MTA file from [GitHub] (https://github.com/SAP-samples/cloud-process-visibility/releases).

2. Choose the `FLPConfigForPVS.zip` file.

    ![import file](FLP-1.png)

>The `FLPConfigForPVS.zip` is downloaded in your local system.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import MTA app to SAP Web IDE)]
1. In your Web browser, open the [SAP Cloud Platform](http://cockpit.hanatrial.ondemand.com) cockpit.

2. Choose **Launch SAP Web IDE**.

    ![Web IDE](MTA-1.png)

2. Log into the **SAP Web IDE Full-Stack** and open the **Development** perspective.

    ![Development Perspective](FLP-step2-2.png)

3. Right-click the **Workspace** root folder, then choose **Import > File or Project**.

    ![import file](FLP-step2-3.png)

3. In the **Import** dialog, browse for the `FLPConfigForPVS.zip` file that you have downloaded in your local system.

    ![import file](FLP-step2-browse.png)

    Upon browsing the file, the other fields automatically get updated.

4. Choose **OK**.

    ![import file](FLP-step2-ok.png)

    >The MTA file is imported under the `Workspace` folder and the file structure is shown below. Ensure that have chosen **Show Hidden Files** to be able to view the file structure as shown.

    ![import file](FLP-step2-structure.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Modify mta.yaml (optional))]
If you have created a service instance with the name other than `pvservice`, you need to perform the following procedure.

1. Right-click the `mta.yaml` file and choose **Open Code Editor**.


    ![import file](FLP-step3-MTA1.png)

2. Add the name of your service instance in the `requires` section of the `approuter` module and the SAP Fiori launchpad site module. In this example, we have created a service instance called `pvservice`. Ensure that you provide the name of the service instance that you have created during [Create a Service Instance of SAP Cloud Platform Process Visibility](cp-cf-processvisibility-setup-serviceinstance).

    ![import file](FLP-step3-MTA2.png)

3. Add the name of your service instance in the `resources` section of the SAP Fiori launchpad site module. In this example, we have created a service instance called `pvservice`. Ensure that you provide the name of the service instance that you have created during [Create a Service Instance of SAP Cloud Platform Process Visibility](cp-cf-processvisibility-setup-serviceinstance).

    ![import file](FLP-step3-MTA3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build and deploy project)]

1. Right-click on the `FLPConfigForPVS` project and choose **Build > Build**.

    ![import file](FLP-step4-build.png)

2. After the build completes, navigate to the **`mta_archives` > `FLPConfigForPVS_0.0.1.mtar`** file and choose **Deploy > Deploy to SAP Cloud Platform**.

    ![import file](FLP-step4-deploy1.png)

3. In the **Log on to Cloud Foundry** dialog, enter the API endpoint and provide your Cloud Foundry credentials to fetch the environment details.

    ![log on](MTA-11.png)

    In the **Deploy to SAP Cloud Platform** dialog, choose **Deploy**.

    ![log on](MTA-12.png)

4. Open the job console at the end of the deployment process, and search for the App Router URL. It should appear in the console as follows:

    `Application <app name>-approuter has been created. Application URL is: https://<application URL>.`

    >To know more about accessing the applications using SAP Cloud Platform cockpit, refer to [Access Launchpad Runtime](https://help.sap.com/viewer/ad4b9f0b14b0458cad9bd27bf435637d/Cloud/en-US/4657960c8fab408eb84a575d267e1041.html).

5. Copy the URL to your browser to access the tiles on SAP Fiori launchpad. You can now see default tiles of Process Visibility on SAP Fiori launchpad.

    ![import file](FLP-step4-FLP3tiles.png)

[VALIDATE_1]
[ACCORDION-END]


---
