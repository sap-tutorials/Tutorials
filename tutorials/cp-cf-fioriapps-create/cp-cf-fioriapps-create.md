---
author_name: Paola Laufer
author_profile: https://github.com/Paolaufer
title: Develop SAP SAPUI5 Application for SAP BTP on Cloud Foundry
description: Create an SAPUI5 application in SAP Web IDE Full-Stack and deploy it to your SAP BTP, Cloud Foundry environment.
auto_validation: true
time: 25
tags: [ tutorial>beginner, products>sap-fiori, tutorial>license ]
primary_tag: products>sap-web-ide
---

## Prerequisites
 - Make sure you have access to the trial version of SAP Web IDE Full-Stack.
 - To access Web IDE go through the [Prepare SAP Web IDE for Cloud Foundry Development](sapui5-webide-open-webide) tutorial.

## Details
### You will learn
  - How to create a new SAPUI5 application for SAP Business Technology Platform (BTP), Cloud Foundry environment
  - How to configure your Cloud Foundry settings in SAP Web IDE
  - How to build and deploy your application to Cloud Foundry

  Create, configure, build, and deploy a simple application on Cloud Foundry in SAP Web IDE Full-Stack.

---

[ACCORDION-BEGIN [Step 1: ](Create a new project)]

1. In SAP Web IDE Full-Stack, right-click your workspace choose **New > Project from Template**.

    ![Open template](step1-new-template.png)

2. In the template wizard that opens, in the **Environment** dropdown list, make sure that **Cloud Foundry** is selected and **Category** should be **Featured** otherwise the SAPUI5 Application tile will not appear.

    ![Select environment](step1-environment.png)


3. Scroll down and click the **SAPUI5 Application** tile and then click **Next**.

    ![Select template](step1-template-selection.png)

4. In the **Basic Information** screen, in the **Module Name** field, enter `FioriDemo`. In the **Namespace** field, enter `mynamespace` and then choose **Next**.

    ![Module name](step1-name.png)

5. In the **Template Customization** screen, accept the default values shown below and choose **Finish**.

    ![Finish](step1-finish.png)

A new MTA project called `mta_FioriDemo` containing the `FioriDemo` HTML5 module now appears in your SAP Web IDE workspace. When developing apps in the Cloud Foundry environment, you create a Multi-Target Application (MTA) file in SAP Web IDE. Each SAP Fiori app is developed as an SAPUI5 module of the MTA.

  ![View MTA file](step1-view-mta.png)

> You can alternatively choose the Multi-Target Application template which will create an MTA project structure and then add new modules to the project.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the layout editor)]

Now you need to open the layout editor in SAP Web IDE to easily make a few changes.

1. Choose **`FioriDemo` > `webapp` > `view`** and right-click the `View1.view.xml` file that you created in the wizard in the previous step.

2. Choose **Open Layout Editor**.

    ![Open layout editor](step2-right-click-view.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Use the layout editor)]

Now you will make some changes using the layout editor, with no need to do any coding.

1. In the layout editor, in the **Controls** pane, in the Search box on the top enter `Text` to filter the controls list. Select the **Text** control.

    ![Find text control](step3-text-control.png)

2. Drag the **Text** control and drop it on the **View** control in the canvas to the right.

    ![Drag and drop](step3-drag-drop.png)

3. Select the **Text** control, and in the **Properties** pane on the right, in the **Text** property, clear the default text and enter `SAP Fiori on Cloud Foundry`.

    ![Enter text](step3-enter-text.png)

Save your work by clicking either the **Save** or **Save All** icon located at the top of the workspace.

![Save work](step3-save.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set Cloud Foundry preferences)]

Now, before you can build and deploy your new application, check your Cloud Foundry preferences.

1. Open the **Preferences** perspective in SAP Web IDE by clicking the **Preferences** icon and then select **Cloud Foundry**.

    ![Open preferences](cf.png)

2. In the pane on the right, select the API endpoint, organization and space for your project.

    >If you are using a trial account, these values are automatically populated.

    ![Cloud Foundry preferences](step4-cf-config.png)

3. Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test your new application)]
Now you need  to run your new application to test it.

But first, check the project settings to make sure that Cloud Foundry is enabled for your project.

> By default, the target environment in your run configuration is set to Cloud Foundry.

1. In the workspace, right-click the `FioriDemo` folder, then choose **Run > Run Configurations**.

    ![Run configuration](step5-run-config.png)

2. In the **Run Configurations for `FioriDemo`** window that opens, click `+` and then select **Run as Web Application**.

    ![Cloud Foundry](step5-check-run-config.png)

3. Click on the **Configuration** and then select `indexhtml` as the **File Name** from the dropdown list.

    ![Select file name](step5-select-filename.png)

4. Click **Save and Run**.

    ![Save and Run](step5-save-run.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Build your application)]

Now you need to build your application.

In your workspace, right-click the **`mta_FioriDemo`** folder and choose **Build > Build**.

![Build](step6-build-app.png)

The build process creates a multi-target archive (`MTAR`) file in your workspace that packages all the project modules for deployment.

![MTAR file](step6-build-result.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Deploy your application)]

Now, you need to deploy your application to SAP BTP, Cloud Foundry environment.

In your workspace, locate and right-click the new `mta_FioriDemo_0.0.1.mtar` file in the `mta_archives` folder, and select **Deploy > Deploy to SAP BTP**.

![Deploy](step7-deploy.png)

The **Deploy to SAP BTP** dialog box opens. The fields are automatically populated. Click **Deploy**.

![Deploy dialog](step7-deploy-dialog.png)

The deployment process takes a few minutes. You can see that the deployment is still in progress in the status bar at the bottom right of your screen.

When the deployment process is complete, you should see the notification in the console at the bottom of your screen and also at the top right of the screen.

![Console](step7-console.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ]( Get the URL to access your application)]
Now you can access your deployed application in the SAP Business Technology Platform cockpit. The steps below show you how to create a URL that you can use to access your new application.

1. From the **Tools** menu  click **SAP BTP Cockpit**.

    ![Open cockpit](tools_cockpit.png)

2. Click **Home [Europe (Rot)-Trial]** at the top of the screen.

    ![Click Home Europe (Rot)-Trial](step8-home2.png)


3. Click **Enter your Trial Account**.

    ![Enter Trial Account](login_cockpit.png)



4. Click the **trial** subaccount box, assuming you are working on the trial version of SAP Web IDE. Otherwise, your subaccount will have a different name.

    ![Click subaccount](Step8-subaccount2.png)


5. Click **Spaces** in the side navigation panel and then click the number link to your Cloud Foundry spaces.

    ![Click space](Step8-Space2.png)

6. Click your space box to open it.

    ![Open space](step8-choose-space-box.png)

7. On your **Applications** page, you should see your new application in the list: `mta_FioriDemo_appRouter` and that it has a **Started** status. Click this link.

    ![Application list](step8-app-approuter.png)

8. A new page opens: **Application: `mta-FioriDemo_appRouter` - Overview.** Right-click the URL under **Application Routes** and save the URL in a text file such as in  **Notepad** or **Notes**.

    ![Get URL](step8-url.png)

9. In your text editor you need to add the following suffix to the URL that you saved in step 8: `/mynamespaceFioriDemo-1.0.0/index.html`

> The construct of the final URL is: `<URL_from_application_overview_page>/<project_name>-<application_version>/index.html`

You can use this URL in any browser to access your new application.

[VALIDATE_8]
[ACCORDION-END]



---
