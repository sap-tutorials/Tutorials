---
title: Build an IoT Condition Monitoring App in with Web IDE Full Stack in 5 Minutes
description: This tutorial shows you how to build a basic condition monitoring and sensor data visualization application with the Web IDE in 5 minutes.
primary_tag: topic>internet-of-things
tags: [  tutorial>beginner, topic>leonardo, topic>sapui5, products>sap-web-ide, products>sap-web-ide-plug-ins, products>sap-IoT-application-enablement, products>sap-cloud-platform  ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Configuration** You or someone else, that is an administrator in your global account, has walked through the following end-to-end configuration and onboarding guide: [SAP IoT Application Enablement: Onboarding - Setting up Your Account](https://help.sap.com/viewer/9dfedbe95cbe4a9f9a5ceddbef7f88e5/latest/en-US/c5b72d23880240dcb4b0d7b9523b065a.html).
 - **Tutorials:** [Create Thing Model]

## Details
### You will learn
- how the Web IDE can be used to build an application that leverages UI controls and data from IoT Application Enablement
- how you can use templates to speed up developing prototypes for certain use cases
- how you can use the sensor chart UI control to discover patterns in your initial sensor data

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Start the Web IDE Full-Stack)]
SAP Web IDE Full-Stack is a powerful, extensible, web-based integrated development tool that simplifies end-to-end application development for SAP Cloud Platform.

**Start the SAP Web IDE Full-Stack** using the URL provided by your team administrator. The URL would look something like this: `https://webidecp-aae9c2f7b.dispatcher.hana.ondemand.com/` only that the account id would be different. You would find it at  [http://hana.ondemand.com/](http://hana.ondemand.com/) where you should find your global account, your NEO sub-account and the Multi Stack Web IDE can be started under `Services`.

To login use the same email address and password you are using as within the thing modeler. The `home` screen looks like this:

![Web IDE MC](iotaecompappmc0010.jpg)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Enable the IoT Application Enablement feature)]
In SAP Web IDE go to **Preferences**, then **Features**. Find **IoT Application Enablement** feature and turn it on.

Make sure following features are turned on as well: **Layout Editor** and **Storyboard**.

![Web IDE MC - Features](iotaecompappmc0020.jpg)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Start the new project from the IoT template)]
Now go to **Development**. Right click on **Workspace** and choose the creation of a new project from a template (there are other ways to do this from the homepage for example).

![New](iotaecompappmc0030.jpg)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Create the app using wizard)]
Choose **IoT Application** from the **Internet of Things** category. Click **Next**
![Choose template](iotaecompappmc0040.png)

Type `greenhousemonitor` as a project name and your user id as the namespace. Click **Next**
![Names](iotaecompappmc0050.png)

On ___Data Source___ step select:
 -    `IoTAS-ADVANCEDLIST-THING-ODATA` or similar as a service
 -    all property sets for your greenhouse thing type in your package (search for `greenhouse`)

![Data Source](iotaecompappmc0060.png)

Click **Next** keeping all of the default settings for each of the page types until the wizard is finished. This has generated a multi-page IoT application for you.

SAP Web IDE will generate the code and will open the new application in the Code Editor. Now please open the application folder and select any of the subfolders to examine the generated source files:

![Code Editor](iotaecompappmc0080.png)


[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Run the application and explore your data)]

You can change the generated source code afterwards as you see fit. Right now we want to explore the data that we have collected with the application. So to see a preview of the application you can start it right from the Web IDE. To do so press on the green button (below Deploy menu in above picture) - this should bring up the application in a new tab:

![App](iotaecompappmc0090.png)

Depending on how many devices you have on-boarded you will see one or multiple greenhouses in the list on the left. Click on one to zoom in and once you have zoomed in to see only one click on it in the map and then click on `Analysis Page` (Feel free to explore the other options on how to navigate the application another time).

This will show this chart:

![Analysis](iotaecompappmc0100.png)

Now you can add measures other then humidity with the `Measured Values` button, change the time-frame with the slider at the bottom or with the buttons labeled `7 Days` and you can look for patterns in the data.

Below example shows 2 patterns - the light sensor in this case seems to jump between 2 extremes day vs night. Lets look at the second one: the humidity provides 140% values at times - these values are probably due to an error in the sensor.

![Patterns](iotaecompappmc0110.png)

You will discover that if you collect sensor data from the real world there will always be surprises, both in that you cannot see, what you thought you would find in the data and also that you see patterns in the data, that you did not expect to see.

For more support on in-depth topics on building an IoT Application please refer to [http://developer.sap.com/](http://developer.sap.com/) under `IoT`.

[ACCORDION-END]


---
