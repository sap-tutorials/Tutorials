---
title: Use an OData service created by Mobile Transaction Bridge to build a Mobile app
description: Using the MTB OData service we will build a Mobile App based on Mobile Development Kit client. The client is generated using wizards.
auto_validation: true
time: 20
primary_tag: products>sap-cloud-platform-mobile-services
tags: [ tutorial>beginner, topic>mobile, products>sap-cloud-platform, software-product-function>sap-cloud-platform-mobile-services ]
---

## Prerequisites
 - **Tutorial** : [Setting up the SAP Business Application Studio (BAS) environment](cp-mobile-bas-setup)

## Details
### You will learn
  - How to build a Mobile Development Kit Client app in the Business Application Studio, using the provided template.

### Introduction
Mobile Transaction Bridge, offers the end user full flexibility in the choice of the consumption vehicle for the OData service that has been published. One may build a Mobile app, a Web-App, deploy/publish the API endpoint from API Management Hub and so on. However, in order to remain true to the core of the tool, templates made available in Business Application Studio, aka. BAS (as well as WEBIDE) allow for creation of fully usable apps based on the Mobile Development Kit client (which is a metadata driven app with a bunch of cool features).
In this Tutorial we will primarily focus on using BAS and the related template.



[ACCORDION-BEGIN [Step 1: ](Invoking the template to create an MDK App)]

In the **File** menu, click on the **New Project from Template** link again.

!![MTB](08.png)

In the following screen, scroll down if you need to and click on the **MTB Project** tile and then the **Next** button.

!![MTB](13.png)

This will kick off the wizard which feeds basic elements the template requires to generate our app.

NOTE: For the purposes of this wizard, unless otherwise specified, please press the **NEXT** or **FINISH** buttons as required.

1. Starting with a **Project** name
    !![MTB](15.png)

2. Then the **Target** for our deployment. In this case, it is an Org called **DW-PM** and a Space called **MTB-Demo**. Your details **will** vary. The API details are picked up automatically and in our case i.e.. this tutorial, there is no need to edit this information.
    !![MTB](16.png)

 3. When in the **Service Name** section,
    1. Give a name of your choice in the **Service File Name** field. You can safely replace the text "Sample Service" as this is simply a placeholder.
    2. Choose **Mobile Service** from the drop down in the **OData Source** field.
    3. If you do that, a list of apps from your Mobile Services instance will be displayed.
        !![MTB](18.png)
    4. Choose the application you are using, in this case, it will be `com.sap.demo`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploying the MDK Application)]

In the Previous step, we got as far as using the template / wizard, to create our application. Now it is time to deploy this application.

Since we are using the MDK project, deployment is a very simple process.

1. Expand the project
2. Find the file named `Application.app`
    !![MTB](23.png)
3. Right click on the file and from the context menu click on **MDK Deploy**
    !![MTB](.\reimgs\06.png)
4. Doing so will give you a selection popup at the top of the main pane. In this case I am choosing Mobile Services since that is our starting point.
    !![MTB](25.png)

As promised, deployment is a breeze. When the app has successfully deployed, you will see a success message in the bottom right hand corner of the main pane.

!![MTB](.\reimgs\93.png)


[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Testing the app on a Mobile Device)]

The Mobile Development Kit Client is a metadata driven application, meaning the app container stays the same but all the content can be manipulated on the fly.

Given this nature of the app, we can onboard devices very simply by scanning a QR code.

1. Expand your **project** and find the **Application.app** file.
    !![MTB](23.png)
2. Clicking on this file will open it in the main pane.
3. Once open, find the **Application QR Code** label at the top of the file.

    !![MTB](27.png)

4. This will open a modal popup with a QR code. If you have to, close the console at the bottom of your screen by clicking on the 'x' for each open tab.

    !![MTB](28.png)

5. Now on your Mobile Device, install the Mobile Services Client app from the store of your choice.

    !![MTB](29.png)

6. The first run will bring you to a license and T&C page. Press **Agree** if you wish to continue.

    !![MTB](30.png)


7. The next screen provides a choice of working with a demo or point the app to a real app. This is of course our choice. Press the **Get Started** button.

    !![MTB](31.png)

8. If this is the first time you are using the app, the only option displayed is **QR Code Scan**. If you previously have configured you will be asked if you want to load the current app or scan a QR code for a new app. In our case now, we need to scan the QR code from our **Application.app** file. Once done, press the **Continue** button.

    !![MTB](32.png)

9. Next log in to your SAP Cloud Platform tenant.

    !![MTB](33.png)

10. You will be asked to maintain a passcode and given the choice of using biometric id.
Once you are done, the app update feature will ask if you want to update. Press the **OK** button.

    !![MTB](34.png)

11. Once the app is ready with the update, you will see a message at the bottom of your phone screen saying the service has been initialized. See the screenshot below as an example.
This is excellent, as we are now ready to use the application.

    !![MTB](35.png)

    Remember when we did the recording? We decided that we are going to search for a user.

12. Press the **Find** link in the app.

    !![MTB](36.png)

13. In the resulting fields, choose any of the fields to fill out; Here I will fill in the `**Lastname**` field. Now click on the **Execute** button.

    !![MTB](37.png)

14. And voila!

    !![MTB](38.png)

Here we have data from our backend ABAP system in a Mobile Application using the OData service we built using the Mobile Transaction Bridge.


[DONE]
[ACCORDION-END]


---
