---
title: SQL Execute Immediate
description: Leveraging SQLScript in Stored Procedures & User Defined Functions
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition  ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Using Dynamic SQL vs Dynamic Filtering](http://www.sap.com/developer/tutorials/xsa-sqlscript-dynamic.html)

## Next Steps
- [Using APPLY FILTER](http://www.sap.com/developer/tutorials/xsa-sqlscript-applyfilter.html)

## Details
### You will learn  
In contrast to executing a string using EXEC, executing the string using EXECUTE IMMEDIATE returns a result set.
**Please note - This tutorial is based on SPS11**

### Time to Complete
**10 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Build your module)]

Switch to the procedure editor.

![procedure editor](1.png)

Replace the EXEC keyword with EXECUTE IMMEDIATE

![execute immediate](2.png)

3. Click "Save".

![save](3.png)

Use what you have learned already and perform a build on your `hdb` module.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run the call statement)]

Return to the HRTT page run the call statement again.

![HRTT](4.png)

You will notice the implicit result set is now returned to the console.  But you still cannot work further on this result set.

![result](5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Change the CALL statement)]

Now change the CALL statement again, this time insert the value for the input parameter as ' ' as shown here. Run the CALL statement again

![modify call statement](6.png)

You will notice the count is 10, which refers to all products except for `Laser printers`.

![count](7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Change CALL statement again)]

Now change the CALL statement. This time insert the value for the input parameter as 'OR 1 = 1' as shown here. Run the CALL statement again.

![modify call statement](8.png)

You will notice the count is now much higher, 106.  This illustrates the possibility of SQL injection. The always true `OR-condition` (1=1) will enforce that the complete `where-condition` will be evaluated to true for each record.

![new count](9.png)

[DONE]
[ACCORDION-END]



## Next Steps
- [Using APPLY FILTER](http://www.sap.com/developer/tutorials/xsa-sqlscript-applyfilter.html)
