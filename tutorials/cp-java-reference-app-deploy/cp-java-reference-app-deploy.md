---
title: Build and Deploy a Java Application
description: Learn how to build and deploy your Java application on SAP Cloud Platform, Neo environment.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>java, products>sap-cloud-platform ]
time: 15
---

## Details
### You will learn  
- How to build the ESPM application on SAP Cloud Platform
- How to bind the application to a database
- How to add the retailer role

---

[ACCORDION-BEGIN [Step 1: ](Clone the Git repository)]
In this step you will create a git repository in the Eclipse IDE and copy the repository of the ESPM application.

1. Copy the repository URL.

    - In your browser, go to <https://github.com/SAP/cloud-espm-v2>.

    - Choose **Clone or Download**, and copy the URL. You can click ![Git-Clone-or-Download](Git_Clone.png) ( **Copy to clipboard** ).

2. Clone the Git repository in Eclipse.

    - In Eclipse, select **Window** > **Perspective** > **Open Perspective** > **Other**.

    - Select **Git** and click **OK** to open the **Git** perspective.

    - From the **Git Repositories** view on the left select **Clone a Git repository**.

        As you already copied the `cloud-espm-v2` Git repository URL to your clipboard, the URI, Host, Repository path and Protocol fields are automatically filled.

        ![Git_Repository_View](Clone_repo.PNG)

    - Choose **Next**.

    - Select the master branch and choose **Next**.

    - You will see the directory where the application will be stored. Choose **Finish**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Import and build the Maven project)]

The following steps describe how you can import and then update the Maven project (which fixes any Maven and POM dependency-related issues). Then you will build the Maven project in Eclipse.

1. Import and update the Maven project

    - In Eclipse, switch back to the **Java EE** perspective and go to **File > Import**.

    - From the Import box that appears select **Maven > Existing Maven projects** and choose **Next**.

    - Specify the root directory of the application (usually Git stores its information in your user folder, then a folder named "git" and then under a folder with the application name), then choose **Finish**.

    - To update the dependencies right click on the **`espm` project > Maven > Update Project** and choose **OK** from the box that appears.

        ![Update Maven](UpdateMaven.PNG)

2. Build the Maven project

    - Right click on the **`espm` project > Run As > Maven build...**

    - In the **Goals** field type in **`clean install`** and choose **Run**. In a couple of minutes the project will be build.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Deploy the application)]

The steps below outline the way in which you can deploy the ESPM Application on the SAP Cloud Platform.

1. Go to the [SAP Cloud Platform Cockpit](https://account.hanatrial.ondemand.com) and select the **Neo trial** button.

2. Go to **Applications** > **Java Applications** and click on **Deploy Application**.

3. Click on the **Browse** button next to the WAR File Location field and select the **git folder > `cloud-espm-v2` > deploy** and then select the `.war` file.

4. Change the Application Name to `espm` and select **Deploy**.

5. Once you receive the message that your application has been successfully deployed, click on the **Done** button.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Bind the application to a database)]

This step describes how to create a new Schema in the cockpit and bind it to the application.

1. Create a new schema

    - In the cockpit, select your account and choose **SAP HANA / SAP ASE** > **Databases & Schemas** from the navigation pane.

    - Select the New button and enter the following information in the pop-up window that appears, then click **Save**:

        |  :------------- | :-------------
        |  Database ID           | **`espm`**
        |  Database System           | **`HANA MDC (<trial>)`**
        |  Password    | The password for the database user. This is the one you chose when you created the **SAP HANA / SAP ASE** > **Databases & Schemas**

2. Bind the database to the application

    - Select your account and choose **Applications > Java Applications** from the Navigation pane.

    - Click on the name of the **`espm`** application.

    - From the Navigation pane select **Configuration > Data Source Bindings**.

    - Select the **New Binding** button and enter the following information in the pop up window, then select **Save**.

        |  :------------- | :-------------
        |  Data Source           | **`<default>`**
        |  DB/Schema ID           | **`espm`**
        |  Database System    | **`HANA (<shared>)`**

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add the Retailer role)]

You need to add the Retailer role in the cockpit in order to be able to access the application with the Retailer UI.

1. Assign a user to the Retailer role.

    - In the cockpit, click on the **`espm application`** and go to **Security > Roles**.

    - Select **Assign*** and add your User ID. It is important that you add you user ID without the "trial" part.

        ![Retailer Role](Retailer_Role2.png)

2. Start and access the application

    - Start the `ESPM` application by going to **Applications** > **Java Applications** and selecting the **`espm`** application. Then choose the **Start** button.

    - Once the application has been started in the cockpit you can access it by using the URL under the Application URLs field.

    - In order to access the application with the Retailer, UI you need to change the `webshop` part of the URL to `retailer`.

    You can also access the application through the following links:

    - eCommerce site: `https://espm<YourAccountName>.hanatrial.ondemand.com/espm-cloud-web/webshop`

    - Retailer UI: `https://espm<YourAccountName>.hanatrial.ondemand.com/espm-cloud-web/retailer`

>If you did not name the application `ESPM` during the deployment, you have to change the name part in the URL respectively.

[DONE]
[ACCORDION-END]


---
