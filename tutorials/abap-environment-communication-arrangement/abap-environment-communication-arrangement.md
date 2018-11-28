---
auto_validation: true
title: Maintain a Communication Arrangement for an Exposed Service
description: Create a communication user and a communication system for an OData service in SAP Cloud Platform ABAP Environment.
primary_tag: product>sap-cloud-platform-abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
---


## Details
### You will learn  
- How to create a communication user
- How to create a communication system
- How to create a communication arrangement

In this tutorial, wherever `XXX` appears, use a number (e.g. `000`).

---

[ACCORDION-BEGIN [Step 1: ](Overview)]
In case you want to expose developed ABAP services for technical communication, you are expected to bundle them into a Communication Scenario. This Communication Scenario can then be consumed as a whole by different Communication Systems, represented by Communication Users. The combination of a Scenario, a System and a User is represented by a Communication Arrangement.

![click on New](Picture21.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Login to SAP Fiori Launchpad)]
  1. Open Eclipse and do right click on your system and navigate to **Properties**.

      ![Open Eclipse](Picture17.png)

  2. Navigate to **ABAP Development** and copy the **System URL**.

      ![System URL](Picture18.png)

  3. Copy this URL in a browser and change the URL like this:

      Add `-web` after `.abap` and `/ui` at the end of URL.
      `https://<your-system>.abap-web.eu10.hana.ondemand.com/ui`.

      ![Change URL](Picture20.png)

  7. Login with the user `exp013-0##@teched.cloud.sap` provided on your handout.

      ![Login to SAP Fiori Launchpad](Picture19.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create communication user)]
  1. Navigate to **Maintain Communication User**.

      ![Navigate to application](Picture3.png)

  2. Create a new user by clicking **New**.

      ![click on New](Picture4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Add communication user data)]
  1. Enter user name `TEST_XXX` (where `XXX` is your group number) and description for your user.

  2. Use **propose password** button since the password has to be 20 characters and save the generated password in a text file for the following exercises.

  3. Click **Create**.

      ![Add Communication User data](Picture5.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create communication system)]
  1. Navigate back to Home and then to **Communication System**.

      ![communication system app](Picture6.png)

  2. Create a new communication system by clicking **New**.

      ![click on New button](Picture7.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Add system ID)]
  1. Add a system ID and system name.
      - System ID: `WEBIDE_BASIC_XXX`
      - System Name: `WEBIDE_BASIC_XXX`

  2. Click on **Create**.

A new communication system will be registered.

![Add system ID](Picture8.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Add communication system data)]
  1. You can retrieve the system host name by right-clicking on your ABAP Cloud Project, selecting **Properties** from the context menu and clicking on **ABAP Development** in the hierarchy of properties. Copy the system URL and remove `https://` at its beginning.

      ![Add hostname](Picture18.png)

  2. Click  **+** button to add the created communication user for inbound communication. Inbound communication means that the here defined system is used from a communication partner to call from external into the SAP S/4HANA system.

  3. Click **Save**.

      ![Add data](Picture9.png)

      ![Add communication user](Picture10.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Create communication arrangement)]
  1. Navigate back to Home and then to **Communication Arrangement**.

      ![Communication Arrangement application](Picture11.png)

  2. Create a new communication arrangement by clicking **New**.

      ![click on new](Picture12.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Select scenario)]
Select your scenario and click on **Create**.

![Select Scenario](Picture13.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 10: ](Add communication arrangement data)]
  1. Add your communication system and select the communication user which you created in the first step in this tutorial.

  2. Copy this service URL / service interface from created communication arrangement, you will need it in next steps.

  3. Click **Save**.

      The URL from the communication arrangement should look as followed:

      `https://<your-system>-api.abap.eu10.hana.ondemand.com/sap/opu/odata/sap/Z_BIND_XXX` (where XXX is your group number)

      ![Enter Data](Picture14.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 11: ](Check the service URL)]
  1. Paste your service URL in a browser or in Postman.

  2. Remove `-api` from the link.

  3. Use the communication user and password.

  4. Check if you can connect to the service and there is no error.

      ![check Service](Picture15.png)

      ![check Service](Picture16.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

---
