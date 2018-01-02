---
title: XS Advanced development - Configure a Space to develop on a tenant database
description: Assign a tenant database to a space
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - You have created a tenant database as explained on [this guide](https://www.sap.com/developer/how-tos/2017/03/hxe-ua-dbfundamentals-tenantdb.html)
 - You have a space to assign the tenant database to or created one as explained on [this tutorial](https://www.sap.com/developer/tutorials/xsa-setup-new-space.html)
 - You have the proper administration rights

## Details
### You will learn  
This tutorial will show you how to assign a tenant database to a space using the SAP HANA Service Broker in the Administration Cockpit.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Log in to the XS Advanced Administration)]

The default URL for SAP HANA, express edition instances is `https://hxehost:51015`.

![Cockpit log in](1.png)

>If you are not using SAP HANA, express edition, or you have changed the ports, you can find out the right URL using command `xs apps` on the Command Line Interface and look for the URL for application `xsa-admin`. You can access the CLI directly from an SSH console on the server or download using the Download Manager. You can get the Download Manager after registering at the [Download site](https://www.sap.com/developer/topics/sap-hana-express.html)

&nbsp;

You need a user with administration rights, such as `XSA_ADMIN`. Log in to the Administration tool with it

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable the tenant DB)]

Click on **SAP HANA Logical Database Setup**

![Cockpit click on logical setup](logical.png)

You will see the available tenant databases. Make sure the one you want to assign is in status `Running` and then press **Enable**

![Tenant database](db.png)

You will be prompted for the credentials to the tenant database

![Tenant database credentials](system.png)

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Assign the tenant to the space)]

Go back to the **Home** screen. Click on the **SAP HANA Service Broker Configuration** tile.

![Logical Database](broker.png)

Select the space you want to map to the tenant database and click on **Map**

![Broker mapping](map1.png)

Select the tenant database as `default` and click **OK**

![Default Database](map2.png)

You will see a success message

![Default Database](map3.png)

You can now use the space in your Multi Target Applications.


[ACCORDION-END]
