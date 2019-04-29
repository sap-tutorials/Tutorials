---
title: Develop SAP Fiori Applications for SAP Cloud Platform on Cloud Foundry
description: Create an SAPUI5 application in SAP Web IDE Full-Stack and deploy it to your SAP Cloud Platform Cloud Foundry environment.
auto_validation: true
time: 25
tags: [ tutorial>beginner, products>sap-cloud-platform, products>sap-fiori ]
primary_tag: products>sap-web-ide
---

## Prerequisites
 - Make sure you have access to the trial version of SAP Web IDE Full-Stack. For more information, see [Getting Started with SAP Web IDE Full-Stack](https://developers.sap.com/tutorials/webide-innovation-beta.html).
 - Set up a trial Cloud Foundry account. Follow **steps 1-3** in this tutorial: [Getting started with Cloud Foundry](https://developers.sap.com/tutorials/hcp-cf-getting-started.html)

## Details
### You will learn
  - How to create a new SAPUI5 application for SAP Cloud Platform Cloud Foundry
  - How to configure your Cloud Foundry settings in SAP Web IDE
  - How to build and deploy your application to Cloud Foundry

  Create, configure, build, and deploy a simple application on Cloud Foundry in SAP Web IDE Full-Stack.

---

[ACCORDION-BEGIN [Step 1: ](Create a new project)]
In SAP Web IDE Full-Stack, right-click your workspace choose **New > Project from Template**.

In the template wizard that opens, in the **Environment** dropdown list, select **Cloud Foundry**. Then choose **Next**.

![New project](step1-project-selection.png)

In the **Basic Information** screen, in the **Module Name** field, enter `FioriDemo`. In the **Namespace** field, enter `mynamespace` and then choose **Next**.

![Module name](step1-name.png)

On the **Template Customization** screen, accept the default values shown below and choose **Finish**.

![Finish](step1-finish.png)

The new project now appears in your SAP Web IDE workspace.

![Workspace](step1-workspace.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the layout editor)]

Now you need to open the layout editor in SAP Web IDE to easily make a few changes.

Choose **`FioriDemo` > `webapp` > `view`** and right-click the `View1.view.xml` file that you created in the wizard in the previous step.

Choose **Open Layout Editor**.

![Open layout editor](step2-right-click-view.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Use the layout editor)]

Now you will make some changes using the layout editor, with no need to do any coding.

1. In the layout editor, in the **Controls** pane, in the Search box on the top enter `Text` to filter the controls list. Select the **Text** control.

    ![Find text control](step3-text-control.png)

2. Drag the **Text** control and drop it on the **View** control in the canvas to the right.

    ![Drag and drop](step3-drag-drop.png)

    The **View** control should now look like this.

    ![View](step3-view.png)

3. Select the **Text** control, and in the **Properties** pane on the right, in the **Text** property, clear the default text and enter `SAP Fiori on Cloud Foundry`.

    ![Enter text](step3-enter-text.png)

Save your work by clicking either the **Save** or **Save All** icon located at the top of the workspace.

![Save work](step3-save.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set Cloud Foundry preferences)]

Now, before you can build and deploy your new application, you need to set your Cloud Foundry preferences.

1. Open the **Preferences** perspective in SAP Web IDE by clicking the **Preferences** icon and then select **Cloud Foundry**.

    ![Open preferences](step4-preferences.png)

2. In the pane on the right, select the API endpoint, organization and space for your project.

    ![Cloud Foundry preferences](step4-cf-config.png)

3. Save your preferences.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test your new application)]
Now you need to run your new application to test it.

But first, check the project settings to make sure that Cloud Foundry is enabled for your project.

> By default, the target environment in your run configuration is set to Cloud Foundry.

1. In the workspace, right-click the `FioriDemo` folder, then choose **Run > Run Configurations**.

    ![Run configuration](step5-run-config.png)

2. In the **Run Configurations for `FioriDemo`** window that opens, on the left choose **Run `index.html`**. On the right you can see that under **Target Environment**, **Run on Cloud Foundry** is selected.

    ![Cloud Foundry](step5-check-run-config.png)

3. Choose **Cancel**.

In the workspace, right-click the `FioriDemo` folder, then select **Project > Project Settings**. Select **Cloud Foundry** and make sure that in the pane on the right that the API endpoint, organization, and space are the same as what you had configured in the **Preferences** perspective in the previous step.

To run the application:

1. Go back to the **Development** perspective, then in the workspace, select the `FioriDemo` folder.

2. Click on the green **Run** button in the upper toolbar.

    ![Run application](step5-run.png)

    A new tab opens in your browser and displays a preview of your application:

    ![Run result](step5-result.png)



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

Now, you need to deploy your application to SAP Cloud Platform, Cloud Foundry environment.

In your workspace, locate and right-click the new `mta_FioriDemo_0.0.1.mtar` file in the `mta_archives` folder, and select **Deploy > Deploy to SAP Cloud Platform**.

![Deploy](step7-deploy.png)

The **Deploy to SAP Cloud Platform** dialog box opens. Enter your credentials as required and choose **Deploy**.

![Deploy dialog](step7-deploy-dialog.png)

The deployment process takes a few minutes. You can see that the deployment is still in progress in the status bar at the bottom right of your screen:

![Deployment progress](step7-deploying.png)

When SAP Web IDE has finished the deployment process, you should see a notification in the console at the bottom of your screen:

![Console](step7-console.png)

Also, these notification messages should be displayed at the top right of the screen:

![Deploy notifications](step7-deploy-notifications.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create a URL for your new application)]
Now you can access your deployed application in the SAP Cloud Platform cockpit. The steps below show you how to create a URL that you can use to access your new application.

1. Open SAP Cloud Platform cockpit from the **Tools** menu in SAP Web IDE. Click the **Home** tab.

    ![Home tab](step8-home.png)

2. On the **Home** page of the SAP Cloud Platform cockpit, click **Cloud Foundry Trial**.

    ![Cloud Foundry trial](step8-cf-trial.png)

3. Click the **trial** subaccount box, assuming you are working on the trial version of SAP Web IDE. Otherwise, your subaccount will have a different name.

    ![Click "trial" subaccount](step8-subaccount.png)

4. Click the number link to your Cloud Foundry spaces.

    ![Click space](step8-choose-space.png)

5. Click your space box to open it.

    ![Open space](step8-choose-space-box.png)

6. On your applications page, you should see your new application in the list: `mta_FioriDemo_appRouter` and that it has a **Started** status. Click this link.

    ![Application list](step8-app-approuter.png)

7. A new page opens: **Application: `mta-FioriDemo_appRouter` - Overview.** Right-click the URL under **Application Routes** and save the URL in a text file such as in  **Notepad** or **Notes**.

    ![Get URL](step8-url.png)

8. In your text editor you need to add the following suffix to the URL that you saved in step 7: `/mynamespaceFioriDemo/index.html`

> For future reference, bear in mind that this is the construct of the final URL: `<URL_from_application_overview_page>/<mynamespace><project_name>/index.html`

You can now use this URL in any browser to access your new application.

[VALIDATE_8]
[ACCORDION-END]



---
