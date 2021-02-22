---
title: Disable SAP HANA System Replication
description: Disable system replication.
auto_validation: true
primary_tag: products>sap-hana-dynamic-tiering
tags: [  tutorial>beginner, topic>big-data, products>sap-hana, products>sap-hana-dynamic-tiering, products>sap-hana-studio ]
---

## Prerequisites
 - **Proficiency:** Advanced
 - **Tutorials** Performing a Fail-back
 - **Credentials:** Have access to the SYSTEM user of  `SystemDB` and "`<SID>adm`" for a SSH session on the HANA hosts.

## Details
### You will learn
- How to disable system replication with SAP HANA Studio

### Time to Complete
**10 Min**

---

[ACCORDION-BEGIN [Step 1: ](Disabling System Replication)]

You can disable SAP HANA system replication for an SAP HANA system by first `unregistering` the secondary system and then disabling system replication on the primary system.

Before we begin to disable system replication, ensure that the Secondary System has been stopped, and that the Primary System is online.

First, we will `unregister` the secondary system.
In the Systems Panel, right-click the **Primary System** and choose **Configuration and Monitoring** > **Configure System Replication**.

![Configure System Replication](configure-system-replication.png)

In the pop-up dialog, select **`Unregister` secondary system** and then **Next**.

![Unregister Secondary System](unregister-secondary-system.png)

Select the **Secondary System Name**. Since Dynamic Tiering only support two-tier system replication, you can leave the default option. Click **Next**.

![Select Secondary System](select-secondary-system.png)

Review the configuration details and click **Finish**.

![Review configuration details](review-configuration-details.png)


Now, to disable system replication on the primary system, right-click the **Primary System** again, and choose **Configuration and Monitoring** > **Configure System Replication**.

![Configure System Replication](configure-system-replication.png)

Choose **Disable system replication** and click **Next**.

![Disable System Replication](disable-sys-repl.png)

Leave the **Ignore secondary system** unchecked, which will remove all secondary system's metadata, and click **Finish**.

![Ignore Secondary System](ignore-secondary-system.png)

System Replication has now been disabled.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
