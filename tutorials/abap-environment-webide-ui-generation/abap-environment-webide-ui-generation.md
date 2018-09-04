---
title: Create a UI that Consumes an Exposed ABAP Service
description: Use SAP Web IDE Full-Stack to create a UI for an ABAP service in SAP Cloud Platform ABAP environment.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
---

## Details
### You will learn  
  - How to create a destination
  - How to create a UI with SAP Web IDE Full-Stack

In this tutorial, wherever `xxx` appears, use a number (e.g. `000`).

---

[ACCORDION-BEGIN [Step 1: ](Log into SAP Cloud Platform)]

Log into SAP Cloud Platform (Cockpit) as an administrator.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create destination)]

Switch to destinations and click **New Destination**.
![Create Destination](Picture1.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add destination data)]

Enter following data to your destination.

| ---------------------------- | ------------------------------------------------- |
|          **Name**            |            Name of your destination               |
|          **Type**            |                    `HTTP`                         |
|      **Description**         |            Description of your destination        |
|           **URL**            |        Service URL from communication Arrangement |
|       **Proxy Type**         |                  `Internet`                       |
|     **Authentication**       |            `BasicAuthentication`                  |
|    **User**                  |          Equals your communication user           |
| **Password**                 |        Password of your communication user        |

![Destination Data](Picture2.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Add additional properties)]

1. Enter additional properties by click on **New Property**.
2. Click on **Save**.
![additional properties](Picture3.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Check connection)]

Check the connection to be sure if you get a successful connection.
![Check Connection](Picture4.png)
![Check Connection](Picture5.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Search SAP Web IDE Full-Stack)]

Navigate to **Services**, search for SAP Web IDE Full-stack and select it.
![Web IDE](Picture6.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Go to service)]

Open **Go to Service**.
You will be navigated to SAP Web IDE.
![Go to Service](Picture7.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Create a new project from template)]

Select  **New Project from Template** to generate one.
![New Project](Picture8.png)
If you don´t see this welcome page then follow the alternative way: **File** > **New** > **Project from Template**.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Choose list report application)]

1. Choose  **List Report Application**.
2. Click  **Next** to create a new UI.
![List Report App](Picture9.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ](Enter basic information)]

1. Enter a name for your project, a title and the application component hierarchy, if needed.
2. Click **Next**.
![Basic Data](Picture10.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ](Choose a system to connect to the required service)]

1. Navigate to **Service URL**.
2. Choose your created destination as a service.
3. Then you must enter a relative URL of the service you want to explore. It is just a part of the URL that you entered in the destination for this service. The relevant part is `/sap/opu/odata/… `
4. Now the status of the service can be tested by clicking **Test** button.
5. Click on **Next**.
![Choose System](Picture11.png)
If you don´t see your created destination, you should restart your browser and log in again.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 12: ](Run the webapp)]

1. Run your application by clicking the **Run** button.
![Run the Webapp](Picture12.png)
2. Choose a file to run.
![Run the Webapp](Picture13.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 13: ](Open your application)]

Navigate to your application.
![Open your application](Picture14.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 14: ](Select fields to be displayed on UI)]

1. To set some filters click on settings icon.
![set Settings](Picture15.png)
2. Select fields that shall be displayed or select all and click **OK**.
![select fields](Picture16.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 15: ](Test your UI)]

By clicking on **GO** you can see the filtered data on UI.
![GO button](Picture17.png)
![UI view](Picture18.png)

[ACCORDION-END]
---
