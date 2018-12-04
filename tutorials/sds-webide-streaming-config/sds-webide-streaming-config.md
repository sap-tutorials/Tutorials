---
title: Configure SAP HANA Streaming Analytics for Web IDE
description: Enable the Streaming Analycis plug-ins for web IDE
auto_validation: true
primary_tag:
tags: [  tutorial>beginner, products>sap-hana-streaming-analytics, products>sap-hana\,-express-edition, products>sap-web-ide, products>sap-web-ide-plug-ins ]
time: 15
---

## Prerequisites  
**Tutorials:**
  - [Installing SAP HANA Streaming Analytics for SAP HANA, Express Edition](https://www.sap.com/developer/tutorials/hxe-ua-installing-streaming.html)

## Details
### You will learn  
  - How to add a database to the Database Explorer in Web IDE
  - How to enable plug-ins for SAP HANA Streaming Analytics for Web IDE
  - How to create a Multi-Target Application

---

[ACCORDION-BEGIN [Step 1: ](Open Web IDE)]

Enter:

```
xs apps
```

Copy the URL beside the `webide` app.

![Check Applications](webide.PNG)

Paste the URL into a web browser and enter.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add database to the Database Explorer)]

Select the __Database Explorer__ in the left navigation bar and click __Add a database to the Database__ Explorer.

![Database Explorer](database-explorer.PNG)

Fill out the information for the database.

![Database Information](adding-database.PNG)
* Database Type: select __SAP HANA Database (Multitenant)__
* Host: enter the host name
* Identifier: select __Instance number__ and enter the instance number given to HANA (90 for SAP HANA Express)
* Database: select __System database__ or select __Tenant database__ and enter the name of the tenant database (HXE for SAP HANA Express)
* User: enter SYSTEM
* Password: enter SYSTEM user password
* Check the __Save user and password (stored in the SAP HANA secure store)__ box

Click OK to finish adding a database.

This is what it will look like if added successfully:

![Added Database](added-database.PNG)

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Enable the Streaming Analytics Plugins)]

Go to __Preferences__ and select __Features__ in the left navigation bar.

![Features](features.PNG)

Enable the __SAP Streaming Analytics__ and __SAP HANA Streaming Analytics Runtime Tool__.

![Streaming Analytics Feature](enable-hana-streaming-analytics.PNG)
![Streaming Runtime Feature](enable-hana-streaming-runtime.PNG)

Click SAVE to refresh and enable features.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Multi-Target Application Project)]

Go to __Development__, right-click __Workspace__ and select __New > Project from Template__

![Workspace](workspace-mta.PNG)

Select __Multi-Target Application Project__ and click Next.

![Multi Target Application](multi-target-application-project.PNG)

Enter a name for the MTA project and click Next.

![MTA Name](mta-name.PNG)

Select a Space from the drop-down menu and click Finish.

![MTA Space](mta-space.PNG)

This is what it will look like if added successfully.

 ![Created MTA](mta-created.PNG)

[DONE]

[ACCORDION-END]

---
