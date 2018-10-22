---
auto_validation: true
title: Create a UI that Consumes an Exposed ABAP Service
description: Use SAP Web IDE Full-Stack to create a SAP Fiori Elements app on top of an OData service exposed in the SAP Cloud Platform ABAP Environment.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
---

## Details
### You will learn  
  - How to create a destination
  - How to create a UI with SAP Web IDE Full-Stack

In this tutorial, wherever `XXX` appears, use a number (e.g. `000`).

---

[ACCORDION-BEGIN [Step 1: ](Overview)]
You have an exposed ABAP service and you prepared a communication arrangement for creating Fiori Application via Web IDE Full-Stack.

![Create Destination](Picture23.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Log into SAP Cloud Platform)]
  1. Use the link `https://account.hanatrial.ondemand.com` to log on to cockpit.

      ![Login to Cockpit](Picture19.png)

  2. Login with the user `exp013-0##@teched.cloud.sap` provided on your handout.

      ![Login to Cockpit](Picture21.png)

  3. Navigate to **Neo Trial**.

      ![Global Accounts](Picture20.png)

  4. Navigate to **Connectivity** and then to **Destination**.

      ![destination](Picture22.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create destination)]
Switch to destinations and click **New Destination**.

![Create Destination](Picture1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add destination data)]

Enter following data to your destination.

| ---------------------------- | --------------------------------------------------------------- |
|          **Name**            |                   Name of your destination                      |
|          **Type**            |                           `HTTP`                                |
|      **Description**         |               Description of your destination                   |
|           **URL**            |        Service URL from communication arrangement without `-api`|
|       **Proxy Type**         |                          `Internet`                             |
|     **Authentication**       |                   `BasicAuthentication`                         |
|    **User**                  |                 Equals your communication user                  |
| **Password**                 |              Password of your communication user                |

> - Use `DEST_XXX` (where `XXX` is your group number) as name of your destination.
> - Remove `-api` from the URL.

![Destination Data](Picture2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add additional properties)]
1. Enter additional properties by clicking on **New Property**.

    | -------------------------- | -------------------------------|
    |      `WebIDEEnabled`       |            `true`              |
    |       `WebIDEUsage`        |     `odata_gen,odata_abap`     |

2. Click on **Save**.

![additional properties](Picture3.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check connection)]

Click on **Check Connection**.

![Check Connection](Picture4.png)

You can be sure if you get a successful connection.

![Check Connection](Picture5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Search SAP Web IDE Full-Stack)]
Navigate to **Services**, search for SAP Web IDE Full-stack and select it.

![Web IDE](Picture6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Go to service)]
Open **Go to Service**.
You will be navigated to SAP Web IDE.

![Go to Service](Picture7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create a new project from template)]
Select  **New Project from Template** to generate one.

![New Project](Picture8.png)

If you don´t see this welcome page then follow the alternative way: **File** > **New** > **Project from Template**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Choose list report application)]
  1. Choose  **List Report Application**.

  2. Click  **Next** to create a new UI.

      ![List Report App](Picture9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Enter basic information)]
  1. Enter a name for your project, a title and the application component hierarchy, if needed.
      - Project Name: `MYDEMOAPP_XXX`
      - Title: `MYDEMOAPP_XXX`

  2. Click **Next**.

      ![Basic Data](Picture10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Choose a system to connect to the required service)]
  1. Navigate to **Service URL**:

  2. Choose your created destination as a service.

  3. Then you must enter a relative URL of the service you want to explore. It is just a part of the URL that you entered in the destination for this service. The relevant part is `/sap/opu/odata/… `.

  4. Now the status of the service can be tested by clicking **Test** button.

  5. Click on **Next**.

      ![Choose System](Picture11.png)

If you don´t see your created destination, you should restart your browser and log in again.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Run the webapp)]
  1. Run your application by clicking the **Run** button.

      ![Run the Webapp](Picture12.png)

  2. Choose a file to run.

      ![Run the Webapp](Picture13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Open your application)]
Navigate to your application.

![Open your application](Picture14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Select fields to be displayed on UI)]
  1. To set some filters click on settings icon.

      ![set Settings](Picture15.png)

  2. Select fields that shall be displayed or select all and click **OK**.

      ![select fields](Picture16.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Test your UI)]
Click on **GO**.

![GO button](Picture17.png)

You can see the filtered data on UI.

![UI view](Picture18.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
