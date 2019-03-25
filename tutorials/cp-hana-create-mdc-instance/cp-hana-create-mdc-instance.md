---
title: Setup your SAP HANA MDC instance
description: Learn how to setup your SAP HANA MDC instance on the SAP Cloud Platform as a "Database As A Service" (DBaaS) persistence layer.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ tutorial>beginner, products>sap-hana, products>sap-cloud-platform ]
---

This instance will be used as your persistence service that you can use with your application or services.

As each HANA MDC instance comes only with a ***System Account*** called SYSTEM, which shall be used only to execute "System" related activities, you will need to add a new user account depending on your tutorial track.

## Prerequisites
  - **Proficiency:** Beginner

## Details
### You will learn

- How to setup a SAP HANA MDC (Multi-Database Container) instance on the SAP Cloud Platform.

### Time to Complete
  **15 minutes**

[ACCORDION-BEGIN [Step 1: ](Access your SAP Cloud Platform account)]

Log into the <a href="https://account.hanatrial.ondemand.com/cockpit#/region/neo-eu1-trial/overview" target="new"><b>SAP Cloud Platform Cockpit Neo Trial</b></a> with your free trial account on **Europe (Rot) - Trial** and access ***Your Personal Developer Account***.

Click on your ***SAP Cloud Platform Account*** identifier (which ends with *trial* by default) as highlighted on the below screenshot.

![SAP Cloud Platform Cockpit](01a.png)

You are now in your ***SAP Cloud Platform developer*** account!

> If you are unclear with what is your SAP Cloud Platform account name, you can refer to the following blog entry: [SAP Cloud Platform login, user name, account id, name or display name: you are lost? Not anymore!](https://blogs.sap.com/2017/01/31/sap-hana-cloud-platform-trial-login-name-user-name-account-name-account-identifier-you-are-lost-not-anymore/)

![Your Personal Developer Account](02a.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create your HANA MDC instance)]

On the left side menu bar, you can navigate to **SAP HANA / SAP ASE** > **Databases & Schemas**.

![Databases & Schemas](03a.png)

Click on **New**.

Complete the form following details information:

Field Name           | Value
-------------------- | --------------
Database ID          | `mdc`
Database System      | Pick `HANA MDC (< trial >)` from the drop down
System User Password | `Welcome18Welcome18`

> ### **Note**:
>**You can choose other values for theses properties. However, the validation steps implemented in the tutorials will be based on the Database ID being `mdc`. So you will have to adjust your entries to validate your progress.**
>
>Also, the default SAP HANA password policy requires a 15 characters password that includes a digit.

&nbsp;

![New Database System](04a.png)

Click on **Create**

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Wait for the creation to finish)]

It will take about 7 to 10 minutes for the creation process to complete. So, now is a good time to get a coffee refill!

The refresh icon ![refresh](0-refresh.gif) will spin every time the page content is refreshed.

![Database Events](05a.png)

Once you see the ***Tenant DB creation finished successfully (created and configured)*** event appears in the list, you can proceed with the next steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Restart the database)]

Now that we have our HANA MDC instance, we will simply restart it to clear out all the caches and free all resources that were allocated during the creation process.

![Trial Database Overview](06a.png)

Click on **Stop**.

This page won't refresh automatically, so we will need to switch to the **Events** page.

On the left side menu, switch to **Events**.

Once you see the ***Database stopped successfully*** event appears in the list, switch back to **Overview** and click on **Start**.

> ### **Note**:
>**If you don't see your HANA MDC stopping right away in the Events log, it is probably because the initial backup is still pending.**
>
>The initial backup may take up to an hour to execute, and your HANA MDC will not restart until it is completed.
>
>Therefore you can proceed with the next steps. However, at some point, the backup will complete, and your instance will be shutdown, therefore you will need to start it.

&nbsp;

Switch again to **Events**.

Once you see the ***Database started successfully*** event appears in the list, you can proceed with the next steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Finalize the SYSTEM user account setup)]

When the SAP HANA MDC tenant is created, the SYSTEM account is not yet finalized with the system roles and privileges.

Theses system roles and privileges are added on the first connection.

Therefore, you need to connect to the **SAP HANA Cockpit** at least once using the HANA database `SYSTEM` user in order to finalize the system account setup and to add all the necessary roles.

> ### **Note:**
>**For your information: any SAP HANA MDC trial instances are shut down every 12 hours and in case a trial instance is not restarted in the next 14 days, it will be deleted.**

![Trial Database Overview](06a.png)

Click on **SAP HANA Cockpit**.

You will be prompted to login.

Enter `SYSTEM` as user name and the SYSTEM user password that was provided during the instance creation as password (e.g. `Welcome18Welcome18` if you followed suggestions in this tutorial).

Click on **Log On**.

![SAP HANA Cockpit Login](07a.png)

You will receive an information message stating that your ***HANA System Account*** is not authorized to access the ***SAP HANA Cockpit***.

![Information](08.png)

Click **OK**.

![Information](09.png)

Then click on **Continue** when the next message is displayed informing you that you've been successfully assigned the required database roles.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Extend the SYSTEM user)]

You are now in the **SAP HANA Cockpit**.

![SAP HANA Cockpit](10a.png)

In order to allow the **SYSTEM** user to run SQL statements, you will need to grant the Database Developer role.

>Never assign developer role to SYSTEM user in productive systems. Create separate developer users in the database.

Click on **Manage Roles and Users** tile. This will open the **SAP HANA Web-based Development Workbench: Security** perspective.

Under **Security**, expand **Users**, then click on **SYSTEM**.

Select the **Granted Roles** tab, then click on the ![plus](0-plus.png) icon, and add the following role:

  - `sap.hana.ide.roles::Developer`

Click on the ![save](0-save.png) button in the top menu bar

![SAP HANA Web-based Development Workbench](12a.png)

Now, look at the granted roles for the SYSTEM user and answer the question below. Then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]
