---
title: Exception Handling, COMMIT & Autonomous Transactions
description: Leveraging SQLScript in Stored Procedures & User Defined Functions
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana,-express-edition ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** [Using COMMIT Statement](http://go.sap.com/developer/tutorials/xsa-sqlscript-trans-commit.html)

## Next Steps
 - [SAP HANA XS Advanced Creating an HDI Module](http://go.sap.com/developer/tutorials/xsa-hdi-module.html)

## Details
### You will learn  
The autonomous transaction is independent from the main procedure transaction. Changes made and committed by an autonomous transaction can be stored in persistency regardless of `commit/rollback` of the main transaction. The end of the autonomous transaction block has an implicit commit.

### Time to Complete
**15 Min**.

---

1. Return to the procedure called `get_product_by_filter`.

	![procedure editor](1.png)
	
2. Remove the COMMIT statement, and instead wrap the INSERT statement with an AUTONOMOUS TRANSACTION block as shown.

	![insert statement](2.png)

3. Click "Save".

	![save](3.png)

4. Use what you have learned already and perform a build on your `hdb` module. Then return to the HRTT page and make sure the input parameters are as shown and run the CALL statement again.

	![HRTT](4.png)

5. You will still get the error for invalid column.  Select the SELECT statement for the `log.errors` table and click "Run" to execute it.

	![select statement](5.png)

6. You will now notice that a new row was entered into the `log.errors` table

	![new row](6.png)

7. Select the SELECT statement for the `log.messages` table and click "Run" to execute it. 

	![select statement](7.png)

8. Another “Chuck” record was not inserted.  “Chuck Norris’s” record was removed by the rollback (“Is that even possible…?”)  by using AUTONOMOUS TRANSACTION blocks, the code within is isolated from the rest of the mainline code and is treated as a separate transaction.

	![results](8.png)
	

## Next Steps
 - [SAP HANA XS Advanced Creating an HDI Module](http://go.sap.com/developer/tutorials/xsa-hdi-module.html)
