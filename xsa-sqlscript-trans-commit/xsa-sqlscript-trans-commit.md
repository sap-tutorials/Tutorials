---
title: Using COMMIT Transactions
description: Leveraging SQLScript in Stored Procedures & User Defined Functions through the use of COMMIT
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition  ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Using Exception Handling](https://www.sap.com/developer/tutorials/xsa-sqlscript-trans-exception.html)

## Next Steps
- [Using Autonomous Transactions](https://www.sap.com/developer/tutorials/xsa-sqlscript-trans-autonomous.html)

## Details
### You will learn  
In this exercise will show the impact of a runtime error on DML statements and how to prevent it using COMMIT.

**Please note - This tutorial is based on SPS11**

### Time to Complete
**15 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Edit previous procedure)]

Return to the SQL tab and change the filter value for the first input parameter. Here you are adding a filter on a column which we know does not exists in hopes of causing an error and transaction rollback.

![sql tab](1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run the CALL statement)]

Select the entire CALL statement, and click **Run**

![call statement](2.png)

Of course we get the error "invalid column name".

![error](3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run log errors SELECT)]

Select the SELECT statement for `log.errors` again and click **Run** to check the table contents.  

![select statement](4.png)

You will notice that a new row was not inserted into the log table due to transaction rollback.

![sql tab](5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add COMMIT)]

Return to the procedure called `get_product_by_filter`. To avoid the deletion of the log entry in case of transaction rollback, we will use an explicit COMMIT.

![procedure editor](6.png)


Insert a DML statement  for the sake of showing the behavior of COMMIT. Insert this INSERT statement with BEGIN and END blocks after the DECLARE statements as shown.

![insert DML statement](7.png)

After the INSERT statement within the EXIT HANDLER, add a COMMIT statement.

![insert statement](8.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Save and build)]

Click **Save**.

![save](9.png)

Use what you have learned already and perform a build on your `hdb` module.

![HRTT](10.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Run the CALL statement)]

Then return to the HRTT page and make sure the input parameters are as shown and run the CALL statement again.

You will still get the error for invalid column. Select the SELECT statement for `log.errors` and click **Run** to execute it.

![select statement](11.png)

You will now notice that the new row has been inserted into the log table even though there was an error and a ROLLBACK was executed.

![rollback](12.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run log messages SELECT)]

Highlight the SELECT statement for `log.messages` and click **Run**.   

![run select](13.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Examine results)]

As you can see not only was the new record inserted into the `log.errors` table, but also "Chuck Norris" found its way into our `log.messages` table. The complete transaction will be committed, meaning any modification happened in this transaction will be persisted. A better solution for this are the autonomous transaction.

![inserted records](14.png)


[ACCORDION-END]




## Next Steps
- [Using Autonomous Transactions](https://www.sap.com/developer/tutorials/xsa-sqlscript-trans-autonomous.html)
