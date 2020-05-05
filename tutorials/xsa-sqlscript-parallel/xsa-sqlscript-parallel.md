---
title: Parallel Processing and Parameters
description: Leveraging SQLScript in Stored Procedures, User Defined Functions, and User Defined Libraries
author_name: Rich Heilman
author_profile: https://github.com/rich-heilman
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition   ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Creating Stored Procedures](https://developers.sap.com/tutorials/xsa-sqlscript-stored-proc.html)

## Next Steps
- [Intermediate Table Variables](https://developers.sap.com/tutorials/xsa-sqlscript-table-var.html)

## Details
### You will learn  
In this exercise we will modify the code of procedure `get_po_header_data`  so that it takes full advantage of the parallel processing within HANA by using table variables.

### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Edit Previous Procedure)]

Return to your procedure called `get_po_header_data`.

![Existing Procedure](1.png)

Define two tabular output parameters which will be used to explicitly pass the results of the SELECT statements to the caller.

![Define output](2.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Assign SELECT Statements)]

Next, assign SELECT statements to the output parameters as shown here.

![assign select](3.png)

The completed code should be similar to this.

```
PROCEDURE "get_po_header_data"(
    OUT EX_PO_CREATE_CNT TABLE(
       CREATE_CNT INTEGER,
      "HISTORY.CREATEDBY.EMPLOYEEID" NVARCHAR(10)),
    OUT EX_PO_CHANGE_CNT TABLE(
       CHANGE_CNT INTEGER,
       "HISTORY.CHANGEDBY.EMPLOYEEID"  NVARCHAR(10)) )  
   LANGUAGE SQLSCRIPT
   SQL SECURITY INVOKER
   --DEFAULT SCHEMA <default_schema_name>
   READS SQL DATA AS
BEGIN

ex_po_create_cnt = SELECT COUNT(*) AS CREATE_CNT, "HISTORY.CREATEDBY.EMPLOYEEID"
     FROM "PO.Header" WHERE PURCHASEORDERID IN (
                     SELECT PURCHASEORDERID
                          FROM "PO.Item"
          WHERE "PRODUCT.PRODUCTID" IS NOT NULL)
GROUP BY  "HISTORY.CREATEDBY.EMPLOYEEID";

ex_po_change_cnt = SELECT COUNT(*) AS CHANGE_CNT, "HISTORY.CHANGEDBY.EMPLOYEEID"
     FROM "PO.Header"  WHERE PURCHASEORDERID IN (
                     SELECT PURCHASEORDERID
                          FROM "PO.Item"
          WHERE "PRODUCT.PRODUCTID" IS NOT NULL)
GROUP BY  "HISTORY.CHANGEDBY.EMPLOYEEID";

END
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Save and Build)]

Save the procedure.

![Save Procedure](5.png)

Perform a build on the `hdb` module.

![Build Module](6.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the Procedure Again)]

Return to the Database Explorer page and run the procedure again.

![HRTT](7.png)

The CALL statement will be inserted into a new "SQL" tab. Click the **Run** button

![Run](9.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check Execution Time)]

Check the execution time again, you may notice that it is a bit faster this time. The reason is that these SQL statements are now executed in parallel.

![Check execution time](10.png)

[DONE]
[ACCORDION-END]
