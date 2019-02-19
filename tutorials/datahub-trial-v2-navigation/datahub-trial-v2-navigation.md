---
title: Navigate around SAP Data Hub, trial edition 2.4
description: Find your way around SAP Data Hub, trial edition 2.4.
auto_validation: true
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora  ]
time: 15
---

## Details
### You will learn  
  - How to find your way around SAP Data Hub, trial edition
  - How to troubleshoot problems
  - Please note here in this tutorial GCP refers to Google Cloud platform and AWS refers to Amazon Web Services.


---

[ACCORDION-BEGIN [Step 1: ](Access UIs via a web browser)]
The SAP Data Hub App Launchpad serves as central entry point to all user interfaces of SAP Data Hub. To access the SAP Data Hub App Launchpad in AWS or GCP you need go to the chapters 3.3 and 3.4 as described in the [**Getting Started with SAP Data Hub, trial edition**] (https://caldocs.hana.ondemand.com/caldocs/help/Getting_Started_Data_Hub24.pdf) guide.

Enter **DEFAULT** as the **Tenant**, `DATAHUB` as **Username** and the password which you have selected during system setup as **Password** to logon to the Launchpad. The system displays the **Application Launchpad** page.

![picture_01](./datahub-trial-v2-navigation_01.png)  

From the SAP Data Hub Application Launchpad, you can navigate to:

 - SAP Data Hub Modeler
 - SAP Vora Tools
 - SAP Data Hub System Management

Subsequently we will describe each of the user interfaces briefly.

>In a production environment you would, of course, access all user interfaces via fully-qualified domain names. SAP Data Hub, trial edition currently does not use such.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](SAP Data Hub Pipeline Modeler)]
The SAP Data Hub Modeler  allows you to create data-driven applications, so-called data pipelines as well as data Workflows.

>Use "default" as the tenant while logging in to Data Hub Pipeline Modeler

![picture_02](./datahub-trial-v2-navigation_02.png)  

Check the below screenshot and answer the question based on the same :

![picture_03](./datahub-trial-v2-navigation_03.png)

[VALIDATE_1]

>**Hint:** You can hover over the button icon to know more about it.



[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](SAP Vora Tools)]
The SAP Vora Tools provide you with a data modeling environment for creating and maintaining tables and views and can be accessed by going to Sap Data Hub App Launchpad as mentioned in Step 1.

![picture_04](./datahub-trial-v2-navigation_04.png)  

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](SAP Data Hub System Management)]
The SAP Data Hub System Management allows you to manage SAP Data Hub, including tenants, users and applications and can be accessed by going to SAP Data Hub App Launchpad as mentioned in Step 1.

![picture_05](./datahub-trial-v2-navigation_05.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Troubleshoot problems and show logs)]
To troubleshoot problems you can access the operating system of the virtual machines as well as the Kubernetes cluster underlying your solution instance. You also have access to `Grafana` and `Kibana` web users interfaces for monitoring and troubleshooting.

If you need to troubleshoot problems, please refer to the corresponding chapters of the [**Getting Started with SAP Data Hub, trial edition**] (https://caldocs.hana.ondemand.com/caldocs/help/Getting_Started_Data_Hub24.pdf) guide.

[DONE]

[ACCORDION-END]

---
