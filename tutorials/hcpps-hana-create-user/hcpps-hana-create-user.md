---
title: Setup a HANA user account
description: Configure a user account with the relevant roles and privileges to use the SAP Cloud for predictive services
primary_tag: products>sap-cloud-platform
tags: [ tutorial>beginner, products>sap-cloud-platform-predictive-service, products>sap-hana, products>sap-cloud-platform ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Create a HANA MDC instance](http://www.sap.com/developer/tutorials/hcpps-hana-create-mdc-instance.html)

## Next Steps
  - [Import a series of predictive demo dataset](http://www.sap.com/developer/tutorials/hcpps-hana-dataset-import.html)

## Details
### You will learn
  - How to create a new user account on your HANA MDC instance using the ***SAP HANA Cockpit***. Each Trial HANA MDC instance comes with a ***HANA System Account***. This user account shall be used only to execute "System" related activities.
  In the "real world", it is uncommon to use this user account to develop an "end-user" application.
  - What are the user roles and privileges required on HANA to use the SAP Cloud for predictive services. As the SAP Cloud for predictive services use the HANA Automated Predictive Library, it is required to configure the user roles and privileges accordingly.

### Time to Complete
  **5 minutes**

[ACCORDION-BEGIN [Step 1: ](Access the Security Console)]
From the ***SAP HANA Cockpit*** and connected with the ***HANA System Account***, click on **Manage Roles and Users**. This will open the ***SAP HANA Web-based Development Workbench***.

![SAP HANA Cockpit](01.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Extend the SYSTEM user)]

In order to allow the **SYSTEM** user to run any SQL statement, you will need to grant him the following role:

  - `sap.hana.ide.roles::Developer`

Under **Security**, expand **Users**, then double click on **SYSTEM**.

![SAP HANA Web-based Development Workbench](02.png)

Select the **Granted Roles** tab, then click on the ![plus](0-plus.png) icon, then add the roles listed above.

Click on the ![save](0-save.png) button in the top menu bar

Using the ![navigation](0-navigation.png) menu bar icon, select **Catalog**.

The following screen should appear:

![SAP HANA Web-based Development Workbench](03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create your user)]

Click on the ![SQL Console](0-opensqlconsole.png) button in the top menu bar, and paste the following SQL code:

```sql
DROP USER HCPPSTRIAL CASCADE;
CREATE USER HCPPSTRIAL PASSWORD Welcome17Welcome17 NO FORCE_FIRST_PASSWORD_CHANGE;
ALTER USER  HCPPSTRIAL DISABLE PASSWORD LIFETIME;

call _SYS_REPO.GRANT_ACTIVATED_ROLE ('sap.hana.ide.roles::Developer','HCPPSTRIAL');

GRANT AFL__SYS_AFL_APL_AREA_EXECUTE TO HCPPSTRIAL;
GRANT AFLPM_CREATOR_ERASER_EXECUTE TO HCPPSTRIAL;

GRANT "CREATE SCHEMA" TO HCPPSTRIAL;

GRANT REPO.READ on "public" TO HCPPSTRIAL;
GRANT REPO.MAINTAIN_IMPORTED_PACKAGES on "public" TO HCPPSTRIAL;
GRANT REPO.MAINTAIN_NATIVE_PACKAGES   on "public" TO HCPPSTRIAL;

GRANT REPO.EDIT_NATIVE_OBJECTS   on "public" TO HCPPSTRIAL;
GRANT REPO.EDIT_IMPORTED_OBJECTS on "public" TO HCPPSTRIAL;

GRANT REPO.ACTIVATE_NATIVE_OBJECTS   on "public" TO HCPPSTRIAL;
GRANT REPO.ACTIVATE_IMPORTED_OBJECTS on "public" TO HCPPSTRIAL;
```

**This script will delete any existing `HCPPSTRIAL` user including its related object and tables.**

Click on the ![Logout](0-run.png) **Run** button or press **F8**.

![SAP HANA Web-based Development Workbench Login](04.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify that your user is properly created)]

Click on the ![Logout](0-logout.png) **Logout** icon located in the top right corner of the screen.

![SAP HANA Web-based Development Workbench Login](05.png)

Now, you need to connect with your **`HCPPSTRIAL`** ***HANA User Account***.

Enter **`HCPPSTRIAL`** as ***Username*** user name and **`Welcome17Welcome17`** as ***Password***, click on **Logon**.

If you can login, then your ***HANA User Account*** is properly configured.

[DONE]
[ACCORDION-END]

## Next Steps
  - [Import a series of predictive demo dataset](http://www.sap.com/developer/tutorials/hcpps-hana-dataset-import.html)
