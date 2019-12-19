---
title: Visualize a Graphical Query Plan
description: Generate and compare query plan for in-memory and extended tables.
auto_validation: true
primary_tag: products>sap-hana-dynamic-tiering
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana-dynamic-tiering, products>sap-hana-studio, topic>big-data, topic>sql ]
---
## Prerequisites
 - **Proficiency:** Intermediate
 - **System:** Access to an SAP HANA 2.0 system with the optional SAP HANA Dynamic Tiering component installed.
 - **Integrated Development Environment:** Basic working knowledge of SAP HANA Studio. E.g. You should be able to connect to your HANA instance from SAP HANA Studio and feel comfortable with executing simple queries.
 - **Tutorials:** [Create Tables and Import Data](https://developers.sap.com/tutorials/dt-create-schema-load-data-part3.html). This includes the `TPCH` user with all the permissions and the created tables with all the sample data loaded.

## Next Steps
 - **Tutorials:** [Compare Prepared and Executed Plan](https://developers.sap.com/tutorials/dt-query-processing-part2.html)

## Details
### You will learn
 - Understanding the outline of query optimization within SAP HANA.
 - Generating an Prepared graphical Query Plan.

### Time to Complete
**20 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Refresh Data)]
This tutorial assumes that you have completed all the prerequisites tutorials. You should have setup Dynamic Tiering, created a `TPCH` schema, created tables, and imported data into those tables. To ensure that the tutorial steps will run smoothly, we will start by refreshing the data in the tables used in this tutorial

In SAP HANA Studio, go to **SAP HANA Administration Console** perspective.

![Administration Console](admin.png)

In the **Systems** tab on the left hand side, right click on the system you are working with, and select **Open SQL Console**.

![SQL Console](sql-console.png)

Copy and run the script below in the SQL console to refresh the data. Make sure you replace each instance of "`<SID>`" in the script with your system's System Id. Verify everything executed successfully.

``` sql
TRUNCATE TABLE "TPCH"."LINEITEM_DT";
TRUNCATE TABLE "TPCH"."LINEITEM_CS";
TRUNCATE TABLE "TPCH"."PARTSUPP_CS";
TRUNCATE TABLE "TPCH"."PART_DT";
DELETE FROM "TPCH"."PART_CS";

IMPORT FROM CSV FILE '/hana/shared/<SID>/HDB00/work/TPCH_DATA/Part.csv' INTO TPCH.PART_CS
  WITH THREADS 4 BATCH 10000;

IMPORT FROM CSV FILE '/hana/shared/<SID>/HDB00/work/TPCH_DATA/LineItem.csv' INTO TPCH.LINEITEM_CS
  WITH THREADS 8 BATCH 10000;

IMPORT FROM CSV FILE '/hana/shared/<SID>/HDB00/work/TPCH_DATA/PartSupp.csv' INTO TPCH.PARTSUPP_CS
  WITH THREADS 4 BATCH 10000;

IMPORT FROM CSV FILE '/hana/shared/<SID>/HDB00/work/TPCH_DATA/Part.csv' INTO TPCH.PART_DT;

IMPORT FROM CSV FILE '/hana/shared/<SID>/HDB00/work/TPCH_DATA/LineItem.csv' INTO TPCH.LINEITEM_DT;
```

![Refresh Data](refresh-data.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Overview of Query Optimization)]
Query optimization within HANA is a multi-layered process. There are multiple query processing engines in HANA which include the Row Store, Column Store, Smart Data Access, and Dynamic Tiering engines. HANA parses the query as submitted and figures out which query processing engine to assign it to. Assuming a mix of Column Store and Dynamic Tiering tables, the query will be passed to the Column Store engine for further processing.

When Dynamic Tiering tables are identified, the portions of the query that involve the extended tables will be handed to the data federation layer. The data federation layer is shared between Smart Data Access and Dynamic Tiering. Smart Data Access is a HANA technology which enables remote tables to be accessed as if they are local tables in SAP HANA without copying the data into SAP HANA. Since Dynamic Tiering is an integrated component of SAP HANA, the data federation layer is able to implement specific optimization paths for queries against tables in the Dynamic Tiering store.

The federation layer generates SQL queries and ships them to the Dynamic Tiering engine to process the Dynamic Tiering portions of the query. The Dynamic Tiering engine parses the received SQL statements, optimizes the query, executes the generated plan and finally returns the results back to the core HANA node. It is important for the Dynamic Tiering engine to perform its own optimization as it understands the on disk structures that are used by the Dynamic Tiering engine within the HANA system.

>These results of Query Optimization are based on SAP HANA Dynamic Tiering 2.0 SPS00 Rev 00. You may see changes in the query plans with different versions of HANA and/or Dynamic Tiering.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Visualize Query for In-Memory Table)]
We will now start by generating a query plan for an in-memory column store table. Delete everything in the SQL console, then paste in the script below.

``` sql
SELECT
  L_RETURNFLAG,
  L_LINESTATUS,
  SUM(L_QUANTITY) AS sum_qty,
  SUM(L_EXTENDEDPRICE) AS sum_base_price,
  SUM(L_EXTENDEDPRICE * (1 - L_DISCOUNT)) AS sum_disc_price,
  SUM(L_EXTENDEDPRICE * (1 - L_DISCOUNT) * (1 + L_TAX)) AS sum_charge,
  AVG(L_QUANTITY) AS avg_qty,
  AVG(L_EXTENDEDPRICE) AS avg_price,
  AVG(L_DISCOUNT) AS avg_disc,
	COUNT(*) AS count_order
FROM
  TPCH.LINEITEM_CS
WHERE
  L_SHIPDATE <= '2018-10-01'
GROUP BY
  L_RETURNFLAG,
  L_LINESTATUS
ORDER BY
  L_RETURNFLAG,
  L_LINESTATUS;
```

![Script Copied](script-copied.png)

Right click inside the SQL console, then select **Visualize Plan** > **Prepare**. You can also press **Ctrl** + **Shift** + **V**. Since we are querying against the `LINEITEM_CS` table which was created as an in-memory column store table, we will also be generating a plan which is for an in-memory table.

![Prepare](prepare.png)

A dialogue will pop up prompting you to open the **`SAP HANA PlanViz`** perspective. Check **Remember my decision** to skip this step in the future. Then click **Yes** to open the `PlanViz` perspective.

![PlanViz](planviz.png)

You should now see the Prepared Query Plan for the column store statements. You can expand and view to see the entire Prepared Plan by clicking on the drop-down as highlighted in red above.

![Prepared CS Plan](estimated-cs.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Visualize Query for Extended Table)]
Now generate a query plan for an extended storage column store table. To do so, go back to the SQL console window you opened in the pervious section by clicking on the tab.

![Go Back To SQL Console](back-sql.png)

Change "**`TPCH.LINEITEM_CS`**" in the script to "**`TPCH.LINEITEM_DT`**". This changes the query to run against the extended table instead of the in-memory instance of the `LINEITEM` table.

![Change SQL script](change.png)

Right click inside the SQL console, then select **Visualize Plan** > **Prepare**. You can also press **Ctrl** + **Shift** + **V**.

![Prepare DT Plan](prepare.png)

You will now be able to view the Prepared Query Plan for the extended table.

![Prepared DT Plan](estimated-dt.png)

>NOTE: The "Prepare" plan option only generates an Prepared Plan. HANA does not send the Dynamic Tiering specific portions of the query to the Dynamic Tiering engine to generate an Prepared Plan. Rather, the Prepared Plan simply recognizes the Dynamic Tiering tables as existing in extended storage and shows that a "Remote Row Scan" is required in the Prepared Query Plan.

[DONE]

[ACCORDION-END]
