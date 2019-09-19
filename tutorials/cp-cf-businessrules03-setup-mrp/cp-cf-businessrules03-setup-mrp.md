---
title: Set Up the Manage Rules Project Application
description: Access the Manage Rules Project application to author rules by deploying a multi-target application using SAP Web IDE Full-Stack.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, products>sap-cloud-platform,products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-business-rules
---

## Details
### You will learn
  -  How to configure a multi-target application to access Manage Rules Project application

The Manage Rules Project application is web-based tool which is used to create projects where you can author and execute business rules. In this tutorial, you will learn how to access Manage Rules Project application by deploying a multi-target application in SAP Web-IDE Full Stack.

---

[ACCORDION-BEGIN [Step 1: ](Download multi-target application file)]

1. Use the following link to access the MTA file from [GitHub](https://).

2. Choose **`cf-businessruleseditor.zip`**.

    ![Download MTA file](MTA_0.png)

The **`cf-businessruleseditor.zip`** is downloaded to your file system.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import MTA to SAP Web IDE)]

1. Log onto [SAP Cloud Platform Cockpit](http://cockpit.hanatrial.ondemand.com).

2. Scroll down and choose **Launch SAP Web IDE**.

    ![Lauch Web IDE](MTA_1.png)

3. In SAP Web IDE Full-Stack, open the **Development** perspective.

    ![Development Perspective](MTA_2.png)

4. Right-click the **Workspace** root folder, then choose **Import > File or Project**.

    ![Import file or project](MTA_3.png)

5. In the **Import** dialog, browse for the **`businessruleseditor.zip`** file that you downloaded in your local system.

    ![Import dialog](MTA_4.png)

    Upon browsing the file, the other fields automatically get updated.

6. Choose **OK**.

    ![Click OK](MTA_5.png)

7. The MTA file is imported under the **Workspace** folder and the file structure is shown below. Ensure that have chosen **Show Hidden Files** to be able to view the file structure as shown.

    ![MTA folder structure](MTA_6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Modify mta.yaml file)]

If you have created a service instance with the name other than **`businessrules`**, you need to perform the following procedure.

1. Right-click the `mta.yaml` file and choose **Open MTA Editor**.

    ![Open MTA editor](MTA_7.png)

2. Under the **Resources** tab, add the name of your business rules service instance as shown:

    ![Resources](MTA_8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build and deploy project)]

1. Right-click on the **`businessruleseditor`** project and choose **Build > Build**.

    ![Build command](MTA_9.png)

2. After the build completes, navigate to the **`mta_archives`** > **`businessruleseditor_0.0.1.mtar`** file. Right-click **`businessruleseditor_0.0.1.mtar`** and choose **Deploy** > **Deploy to SAP Cloud Platform**.

    ![Deploy](MTA_10.png)

3. Enter the API endpoint and log onto Cloud Foundry to fetch the environment details.

    ![CF login](MTA_11.png)

    Choose **Deploy**.

    ![CF login](MTA_12.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Access Manage Rules Project Application)]

Open the job console at the end of the deployment process, and search for the application URL. It should appear in the console as follows:

```
Application "`<app name>_appRouter`" started and available at "`<application URL>`"
```

![MRP link](MTA_13.png)

> You can bookmark this link for later use.

[VALIDATE_1]
[ACCORDION-END]
