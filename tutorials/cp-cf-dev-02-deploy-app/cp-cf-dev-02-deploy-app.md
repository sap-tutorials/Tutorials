---
title: Deploy an App to SAP BTP, Cloud Foundry runtime
description: Use SAP BTP cockpit to deploy an app to the Cloud Foundry runtime, then explore what was deployed and created.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, products>sap-cloud-platform ]
time: 5
---

## Details
### You will learn
  - How to deploy an app to the SAP BTP, Cloud Foundry runtime using the cockpit
  - What is created when an app is deployed

This tutorial assumes you've completed the [Download and Prepare App for Cloud Foundry Deployment](https://developers.sap.com/tutorials/cp-cf-dev-01-prepare-app.html) tutorial, where you should have a folder containing a sample Node.js app with a deployment descriptor file. You're going to package the app and deploy it directly from your machine using a feature in the SAP BTP cockpit itself.

---
[ACCORDION-BEGIN [Step 1: ](Package the app up)]

To deploy your sample application on SAP BTP, create a zip file that contains the necessary files.

 1. Open the folder `cf-sample-app-nodejs-master` which contains your sample app.
 1. Select the entire contents of this folder -- all folders & files -- and add them to a new `hello-nodejs.zip` file.

![Create zip](Create-zip.png)

> Be sure not to include the containing folder in the zip file -- you must explicitly select only the contents of the `cf-sample-app-nodejs-master` folder and not that folder itself.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to your dev space on SAP BTP)]

In the SAP BTP cockpit, go to the Cloud Foundry **dev** space of your trial account.

Make sure that the **Applications** tab is selected in the navigation menu.

![Screenshot of applications page](Button-deploy-application.PNG)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy the sample application)]

 1. Choose **Deploy Application**.

    ![Deploy button](Button-deploy-application.PNG)

 1. Choose **Browse** to select the **`hello-nodejs.zip`** file you created earlier.

 1. Enable **Use Manifest**.

 1.  Choose **Browse** to select the **`manifest.yml`** file in your **`hello-nodejs`** folder.

    ![Deployment dialog](Deploy-dialog.PNG)

 1.  Choose **Deploy**.

The application is deployed to SAP BTP and starts automatically. It appears in the list of applications. The requested state is initially red, but turns green if the application starts without problem.

![Result of started application](Started-app2.PNG)

[VALIDATE_3]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Explore the deployed app)]

1. Choose the application from the list of applications by clicking on its name.

    The application overview opens.

1. In the **Application Routes** section, choose the URL to open the application you've just deployed.

    ![Application Routes section](Application-Route.PNG)

    A new tab opens that displays the app information.

    ![CF application information](App-CF.PNG)

At this stage, you have your sample app deployed and running in Cloud Foundry on SAP BTP. Well done!

[DONE]
[ACCORDION-END]

---
<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=cp-cf-dev-02-deploy-app&graphics=true" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>

