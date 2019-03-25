---
title: View Data Across Both In-Memory and Dynamic Tiering Tables Using a SQL View
description: Create and use a SQL view to query data from both in-memory and dynamic tiering tables.
auto_validation: true
primary_tag: products>sap-hana-dynamic-tiering
tags: [  tutorial>beginner, topic>sql, products>sap-hana, products>sap-hana-dynamic-tiering, products>sap-web-ide ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Migrate Records in Related Tables Using Stored Procedure](https://developers.sap.com/tutorials/hana-webide-dt-getting-started-5.html)

## Next Steps
 - **Tutorials:** [Multi-Store Tables](https://developers.sap.com/tutorials/hana-webide-dt-getting-started-7.html)

## Details
### You will learn  
 - How to view the combined data set for data partitioned between an in-memory and a dynamic tiering table instance.
 - How to create and use a SQL view.
 - How to query against a SQL view with conditions.

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Query from both in-memory and dynamic tiering tables)]
There are cases where you only need to access data either from the in-memory table instance (`ORDERS_CS`) or from the dynamic tiering table instance (`ORDERS_DT`). However you will also likely have use cases where you need to query the full data set across both table instances, which can be done with a union.
Run the script below in a SQL Console to query data from both in-memory and dynamic tiering tables using a `UNION`.

```SQL
SELECT * FROM "TPCH"."ORDERS_CS"
UNION ALL
SELECT * FROM "TPCH"."ORDERS_DT"
```
> Since we are explicitly managing the data set between `ORDERS_CS` and `ORDERS_DT` to ensure that data is not duplicated between the 2 tables, we can use the `UNION ALL` variation of the `UNION` clause, which is faster because it doesn't eliminate duplicate records in the combined result set.

![Union Query](assets/hana-webide-dt-getting-started-7-864f21b2.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create and query against a SQL view)]
Alternatively you can create an SQL view to query data from multiple tables and simplify the process. Run the script below to create a SQL view. Verify it ran successfully.

```SQL
CREATE VIEW "TPCH"."ORDERS_ALL_VIEW" AS
  SELECT * FROM "TPCH"."ORDERS_CS"
  UNION ALL
  SELECT * FROM "TPCH"."ORDERS_DT";
```

![Creating View](assets/hana-webide-dt-getting-started-7-b50ddb5a.png)

After successfully creating a view, you can query against the `"TPCH"."ORDERS_ALL_VIEW"` whenever you need to access the combined data set. For example, if you want to query order records that are between 6 months and 18 months old, you can execute the query below.

```SQL
SELECT "TPCH"."ORDERS_ALL_VIEW".* FROM "TPCH"."ORDERS_ALL_VIEW"
    WHERE "TPCH"."ORDERS_ALL_VIEW" ."O_ORDERDATE"
      BETWEEN ADD_YEARS(CURRENT_DATE, -1.5)
          AND ADD_YEARS(CURRENT_DATE, -0.5)
```

![Select View](assets/hana-webide-dt-getting-started-7-31c6f818.png)

[VALIDATE_1]

[ACCORDION-END]

