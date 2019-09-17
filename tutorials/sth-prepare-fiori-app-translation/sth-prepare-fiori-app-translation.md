---
title: Prepare an SAP Fiori App for Translation
description: Prepare an app based on an SAP Fiori reference app and prepare it for translation with SAP Translation Hub.
auto_validation: true
primary_tag: products>sap-translation-hub
tags: [  tutorial>beginner, products>sap-translation-hub, products>sap-cloud-platform, topic>sapui5 ]
time: 10
---

## Details
### You will learn  
  - How to prepare an SAP Fiori reference app so that you can translate it using SAP Translation Hub


---
[ACCORDION-BEGIN [Step 1: ](Open overview screen of cockpit)]
To go to the overview screen of the SAP Cloud Platform cockpit, choose your user ID at the top of the screen.

![Locate SAP Web IDE](sth-1.png)

[DONE]

[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Locate SAP Web IDE in the cockpit)]
Choose **Services** and locate the **SAP Web IDE** tile by searching for **`web`**. Then choose the tile.

![Locate SAP Web IDE](sth-2.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open SAP Web IDE)]

In the service description for SAP Web IDE, choose **Go to Service**.

![Open SAP Web IDE](sth-3.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a new project from a sample application)]

To get started with the app, ensure that you're on the **Home** tab (house icon) and choose **New Project from Sample Application**.

![Choose new project](sth-4.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Choose the Shop app)]

To add a project with the required files to your account, choose **Shop** and then **Next**.

![Choose shop app](sth-5.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Accept standard license conditions)]

Accept the standard license conditions by choosing **I agree** and then **Finish**.

![Accept conditions](sth-6.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Open the project settings)]

To be able to view the app in multiple languages and enable SAP Translation Hub to locate the text resources file, you need to enter some data in the project settings.
Select root folder of the application **sample.Shop** and open the menu with a right mouse click.
Select **Project - Project Settings**.

![Open the project settings](sth-7.png)

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Make project settings)]

In this step, you're going to specify the languages in which you want the app to be available (Chinese, French, and German).
In the `Project Settings` locate the **Languages tab** in section General and check that **English** is the default language.

![Make project settings](sth-8.png)

Scroll down in the Languages section to maintain the languages: **Chinese, French and German**.
 **Save and Close** the Project settings afterwards.

![Define project settings 2](sth-9.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create a run configuration)]

To test the application with mock data from a local system, you're going to need a special run configuration. To do this, right-click the root folder of your application (`sample.Shop`) and choose **Run | Run Configurations**.

![Run configurations](sth-10.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Choose Web application)]

Choose the **+** sign then **Web Application**.

![Web application](sth-10a.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Assign a run application file)]

Now you need to do the following:

- In the **File Name** field, choose the application file name `/webapp/test/flpSandboxMockServer.html`.
- Under **Preview Mode**, select **With Frame**.
- Under **Mock Data**, select **Run with mock data**.

Once you've done that, choose **OK**.

![Run application](sth-11.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Open the app in the SAP Fiori launchpad)]

Now you want to see what the application looks like by accessing it from an SAP Fiori launchpad. To do this, choose the green button shown below.

![Open with SAP Fiori Launchpad](sth-12.png)
> Depending on your browser settings, you might have to disable the pop-up blocker to proceed.

Choose the **Shop** tile.
![Open Shop app](sth-13.png)

To make things look more realistic, the app uses mock data.

![SAP Fiori app with mock data](sth-14.png)

[VALIDATE_1]

[ACCORDION-END]
