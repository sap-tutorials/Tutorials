---
title: Configure a Mobile Services application to use a Mobile Transaction Bridge OData Service
description: Use the OData service you built in the previous tutorial with the Mobile Transaction Bridge and now prepare the necessary groundwork to take it further onto a Mobile Device with just a few clicks.
auto_validation: true
time: 20
tags: [tutorial>beginner, topic>mobile, products>sap-cloud-platform, software-product-function>sap-cloud-platform-mobile-services]
primary_tag: products>sap-cloud-platform-mobile-services
---

## Prerequisites

1. [Set up your ABAP for Mobile Transaction Bridge](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mtb/prerequisites.html)
2. **Tutorial** : [Liberate your ABAP with Mobile Transaction Bridge](cp-mobile-mtb-rec)

## Details
### You will learn
    - How to create & configure a Mobile Services Application to use the Mobile Transaction Bridge (MTB) recording as a data-source.

### Background
Previously, we have seen how we can configure the ABAP system to support Mobile Transaction Bridge and then we used the MTB to create and publish an OData Service. While you can use this service to build any type of modern front-end, we are going to see how to build a Mobile Application based on the Mobile Development Kit Client (MDK client) in just a few clicks.

---

[ACCORDION-BEGIN [Step 1: ](Create a Mobile Services App)]

1. Navigate to your Mobile Services Admin Cockpit.

    !![MTB](00.png)

2. In the left navigation pane, choose **Native / Hybrid** and thereafter, click on the **New** button in the top right hand corner of the main pane.

    !![MTB](01.png)

3. In the following modal popup, enter the details for the application you are building. Below are the values I am using but you are welcome to replace these with any appropriate values of your choice. Once done, click on the **Next** button.
    1. ID : com.sap.mtb.tut
    2. Name : Tutorial Example App
    3. Description : An app to demonstrate Mobile Services configuration in the MTB tutorial.
    4. Vendor : SAP
    5. License type : Standard
    6. XSUAA Service : Default Instance

      !![MTB](02.png)

  4. In the following modal popup, choose **Mobile Development Kit** from the **Assign Features for:** dropdown.
      1. Doing so, automatically selects features from the list. Generally, I deselect those that are not necessary **however** in this case,
          1. we **leave the defaults as they are** and
          2. **add a tick for Mobile Transaction Bridge OData** (See screenshot below).
      2. Once done, click on the **Finish** button.

      !![MTB](./rep_imgs/01.png)

  5. Press **OK** on the next popup. You may mark the checkbox if you wish. The app generation takes a few seconds

      !![MTB](./rep_imgs/02.png)
and once done, you will be presented with the app details screen.

      !![MTB](04.png)

We now have a mobile services app. Let us now configure it to point to the MTB service previously generated.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure app to use MTB as the data source)]

Now that we have the app let us connect it to our MTB service.

1. Navigate to the Mobile Services Admin Cockpit.
    !![MTB](00.png)
2. In the left navigation pane choose **Native/Hybrid**

3. In the main pane, choose the app you created in the previous step.
    !![MTB](05.png)

4. You should now be presented with the details page of the app.
5. Here click on the navigation link entitled **Mobile Connectivity** in the **Assigned Features** pane.

    !![MTB](06.png)

6. On the following page, we are going create a **New** destination by clicking on the **Create** icon.

    !![MTB](07.png)

7. In the modal popup that opens up once you click on the create icon, fill in the appropriate details. ( *See the screenshot at the bottom of this section.* )
    1. Destination Name : **Leave it as it is**
    2. the URL : This is the target system URL (Example : `_https://FQDN-of-ABAP-SYSTEM:port_number`).
        - *Note* - Usually, the ABAP system is located on-premise and requires a SAP Cloud Connector to be configured. We did this in a previous tutorial. Please retrieve the **Virtual** URL as configured in your cloud connector if you are using one. If your system is accessible on the internet, then simply provide the URL to your system.

    3. Use Cloud Connector : **Ticked**
        - Since I need to use one. You will choose as per the landscape you are working in.
        - You will be asked to specify the **Location ID** of your Cloud Connector. Only enter this if you have this configured in your Cloud Connector. Leave it empty if you do not have anything entered there.
    4. Use Mobile Transaction Bridge : **Ticked**
        1. Mobile Transaction Bridge Recording : **MTB-TUT** from the drop down.

        2. SAP Client : **your SAP Client number**
            - In my case, it is 001. Enter the value that is appropriate to your ABAP backend.

                !![MTB](08.png)

    5. Click the Next button to go to the Custom Headers step.
    6. Click Next again to go to the Annotations step.
    7. Click Next to set the SSO Mechanism for the connection
        - Here I will use **Basic Authentication** for the purposes of this tutorial.
            !![MTB](09.png)
    8. Once you have entered your credentials to access the ABAP backend system, click on the **Finish** button.

### Excellent! Now we have configured a data-source to match our app.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the Pre-Flight Checklist)]

Debugging applications which traverse multiple layers of landscape topology can be difficult, if not downright daunting. MTB offers a simple tool that enables the user to run an automated checklist that determines if the basic technical criteria are met. We call it the **Pre-Flight Checklist**

At the end of the previous step, when you clicked on the **Finish** button, the resulting page was the **Destination Details** page. In this page, at the top left hand corner of the main pane,

1. Click on the **Mobile Connectivity** navigation link in the breadcrumbs.
    !![MTB](10.png)

2. On the **Mobile Connectivity** overview page.
    1. Click on the **Pre-Flight Check** icon.
    !![MTB](11.png)

3. Click on **OK** in the resulting modal popup.
4. The results popup will show an overview of the test and
    1. clicking on the **Details** button will as expected provide more details (reasons for failure) on each of the tests.

Here is an example of some errors

  !![MTB](12.png)

And here is an example of a successful pre-flight check.

  !![MTB](./rep_imgs/03.png)

Resolve any issues before moving on to the next step, which is building the MDK Client.

### Congratulations!! You have successfully created and configured a Mobile Services app to use the OData service built using MTB.###

[DONE]
[ACCORDION-END]
