---
title: Configure a Space to Develop on a Tenant Database (XS Advanced)
description: Assign a tenant database to a space.
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, products>sap-web-ide ]
time: 15
---

## Prerequisites  
 - You have created a tenant database as explained on [this guide](https://www.sap.com/developer/tutorials/hxe-ua-dbfundamentals-tenantdb.html).
 - You have a space to assign the tenant database to or created one as explained on [this tutorial](https://www.sap.com/developer/tutorials/xsa-setup-new-space.html).
 - You have the proper administration rights.

## Details
### You will learn  
  - How to assign a tenant database to a space using the SAP HANA Service Broker in the Administration Cockpit

---

[ACCORDION-BEGIN [Step 1: ](Log in to the XS Advanced Administration)]

If you are using SAP HANA, express edition, go into `https://hxehost:51036` or `https://hxehost:39030` to access the `xsa-cockpit` app.

![Cockpit log in](39030_cockpit.png)

Click on the button to open and log in to the XSA Cockpit

![Cockpit log in](1.png)

>If you are not using SAP HANA, express edition, or you have changed the ports, you can find out the right URL using command `xs apps` on the Command Line Interface and look for the URL for application `xsa-cockpit`. You can access the CLI directly from an SSH console on the server or download it using the Download Manager. You can get the Download Manager after registering at the [Download site](https://www.sap.com/developer/topics/sap-hana-express.html)

&nbsp;

You need a user with administration rights, such as `XSA_ADMIN`. Log in to the Administration tool with it

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable the tenant DB)]

Click on **Enable**

![Cockpit click on logical setup](logical.png)

You will be prompted for the credentials to the tenant database

![Tenant database credentials](system.png)

After a couple of seconds you will see the database is enabled

![Tenant database credentials](enabled.png)

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Assign the tenant to the space)]

Click **Map**

![Logical Database](broker.png)

Select the space you want to map to the tenant database and click on **Map**

![Broker mapping](map1.png)

Select the tenant database as `default` and click **OK**

![Default Database](map2.png)

You can now use the space mapped to a tenant database in your Multi Target Applications.

[DONE]
[ACCORDION-END]
