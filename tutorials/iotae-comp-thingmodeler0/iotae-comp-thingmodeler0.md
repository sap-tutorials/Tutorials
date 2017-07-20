---
title: Onboarding a computer as the IoT thing
description: Onboarding a computer in SAP IoT Application Enablement as the thing
primary_tag: topic>internet-of-things
tags: [  tutorial>beginner, topic>internet-of-things, products>sap-iot-application-enablement, products>sap-cloud-platform ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **System access:** You have a user in SAP IoT Explore tenant (in limited availability as of now)


## Next Steps
 - [Send the CPU usage data to SAP IoT Application Enablement](https://www.sap.com/developer/tutorials/iotae-comp-sendpy0.html)

## Details
### You will learn  
You will learn how to onboard your computer as the IoT thing for collecting sensors data from it.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Open SAP IoT Application Enablement Thing Modeler)]
You should have access to SAP IoT Explore tenant on SAP Cloud Platform. It is in limited availability as of now - primarily available to participants of SAP's face to face events.

Open https://sap-iotaeexplore.iot-sap.cfapps.eu10.hana.ondemand.com/launchpage/#Shell-home in the web browser.

Click on **IoT Thing Modeler** to scroll to a group of Thing Modeler applications.

![Thing Modeler](iotaecomptm0010.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](The `computeriotdevice` package)]
Open **Package Manager** application.

A package serves as a container for thing-related objects, such as thing types, things, property sets, properties, or event types.

The shared package `computeriotdevice` has been already created for you. You can search for it.

![The package](iotaecomptm0020.jpg)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Thing property sets in `computeriotdevice`)]
While in the Package Manager click on the **Open in Thing Properties Catalog** icon of the `computeriotdevice` package.

![Open in ](iotaecomptm0030.jpg)

The property sets have been already preconfigured for you. The package contains two property sets:
 1. `Default` of the type Basic Data - with one property `common_name`
 2. `resource_sensors` of the type Measured Data - with one property `cpu_usage`.

The property `cpu_usage` has
 - ___float___ data type,
 - ___percentage___ unit of measure,
 - and two thresholds: ___upper___ and ___uppermost___.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Thing type `generic_computer`)]
In the Thing Properties Catalog click on the **Thing Modeler**.

In the **Thing Types** pane you will see `generic_computer` thing type defined with:
 - Basic Data properties from the `Default` set,
 - Measured Values properties from the `resource_sensors` set.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add your computer as a new thing)]
From **Thing Type** overview for `generic_computer` click on the **New Thing**.

![New Thing](iotaecomptm0060.jpg)

Populate the name `computer_[userid]` and the description `[Name]'s computer` of the newly created thing.

>You can find the user name from https://sap-iotaeexplore.accounts400.ondemand.com/ui/protected/profilemanagement
>![Username](iotaecomptm0070.jpg)

Choose:
 - Authorization Group: `sap_iotaeexplore`
 - Select Provider: `iots`
 - Account: `a2667617c`

These values will create APIs for the device in SAP Cloud Platform account `a2667617c` using IoT Service for Neo Environment (`iots` provider).

![Definition](iotaecomptm0080.jpg)

Click **Save**. After a few seconds the new thing will be created and its technical device id and authorization token for the API calls will be displayed. Copy these values.

![technical ids](iotaecomptm0090.jpg)

This authorization token is displayed only once!



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure your new thing)]
You have created your first thing, which is a digital twin of the real device, like your computer in this case. Congratulations!

Now it's time for a few configuration activities.

Firstly, update the master data of the thing. Go to **Basic Data** of your thing. Expand `Default` category. Provide the common name, like computer's name, in the **Value** column of the property `common_name`.

>To find the computer name you can type command `hostname` in its terminal.
>![hostname](iotaecomptm0110.jpg)


Click **Save**

![Save master data](iotaecomptm0100.jpg)

Secondly, set the alert thresholds for CPU usage. Go to **Measured Values**. Expand `resource_sensors` category, then `cpu_usage` property.

Set `upper` value to `45`, and `uppermost` to `90`.

Optionally, you can scroll to **Image** and add a picture of your thing.

![Image of the thing](iotaecomptm0120.jpg)

[DONE]
[ACCORDION-END]

---

### Optional


[ACCORDION-BEGIN [Step 7: ](Review Thing Modeler user guide)]
You can find the official user guide at https://help.sap.com/viewer/p/SAP_IOT_APPLICATION_SERVICES > End-User Information > Thing Modeler Apps

[DONE]
[ACCORDION-END]


## Next Steps
 - [Send the CPU usage data to SAP IoT Application Enablement](https://www.sap.com/developer/tutorials/iotae-comp-sendpy0.html)
