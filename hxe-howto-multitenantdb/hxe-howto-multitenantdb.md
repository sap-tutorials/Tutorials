---
title: Create a multi-tenant container database in `HANAExpress`
description: Provide simple instructions for a user to create a multitenant database in `HANAExpress`. SAP HANA systems with multitenant database containers can contain multiple tenant databases.
primary_tag: products>sap-hana\, express-edition
tags: [  tutorial>beginner, products>sap-hana\, express-edition ]
---

## Prerequisites  
 - Proficiency: beginner
 - Setup: `HANA, express edition` must be running.

## Next Steps
 - Go to the [`SAP HANA, express edition tutorials page`](http://www.sap.com/developer/topics/sap-hana-express.tutorials.html)

## How-To Details
Provides instruction on how to create a `multitenant` container database in `HANAExpress` and how to connect and verify the new database.

### Time to Complete
**5 Min**.

---

1. Connect to `HANA, express edition` using SYSTEM user and create a `multitenant` container database:

    % `hdbsql -i 00 -n localhost:30013 -u SYSTEM -p` `<SYSTEM user password>` `"CREATE DATABASE` `MTDB1` `SYSTEM USER PASSWORD` `<password>`"

    This will take about 1 min, result will be similar to:

    ![image 1](1.png)

2. Verify the new `multitenant` container database is accessible:

   % `hdbsql -i 00 -n localhost:30013 -u SYSTEM -p` `<SYSTEM user password>` `"select * from "PUBLIC"."M_DATABASES""`

   ![image 1](2.png)

3. Verify that you can connect to the database using a HANA client tool such as `HANA Studio`. To do so, select the `Add System` command in `HANA Studio`:

    ![image 1](4.png)

    ![image 1](5.png)

    View from HANA Plugin for Eclipse after adding "MTDB1":

    ![image 1](3.png)


## Next Steps
 - Go to the [`SAP HANA, express edition tutorials page`](http://www.sap.com/developer/topics/sap-hana-express.tutorials.html)
