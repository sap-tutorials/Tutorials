---
title: Debugging Stored Procedures
description: Leveraging SQLScript in Stored Procedures & User Defined Functions
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition ]
---
## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Creating Table User Defined Functions](http://www.sap.com/developer/tutorials/xsa-sqlscript-table-user.html)

## Next Steps
- [Anonymous Blocks](http://www.sap.com/developer/tutorials/xsa-sqlscript-anonymous.html)

## Details
### You will learn  
In the following exercise we will show how to debug a procedure using the SQLScript debugger. This includes setting breakpoints, evaluating expressions and intermediate results.
**Please note - This tutorial is based on SPS11**

### Time to Complete
**10 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Start the debugger)]

Go to the HRTT page and open the procedure called `dev602.procedures::get_po_header_data`.

![HRTT](1.png)

From the menu, click SQL Debugger, then Debug Settings.

![SQL debugger](2.png)

For the service name, click the drop down box.

![service name](3.png)

Choose your container associated with your user id.  Click "OK".

![container](4.png)

Click the "Connect" button.

![connect](5.png)

Click "Apply", then "Close".

![apply](6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set breakpoints)]

Set breakpoints at the lines shown here by simply clicking on the line number.

![breakpoints](7.png)

Click **Invoke Procedure**.

![invoke](8.png)

A new SQL tab will be opened.  Click on the **Run** button.

![SQL tab](9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](View the debugger pane)]

Execution of the procedure has started, and a debugger pane is now visible which is displayed on the right. You can see all of the variables and parameters for this procedure.  You might notice that this pane is currently not showing the intermediate table variables at this point.

![execute procedure](10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use step over)]

Click the **Step Over** button.

![step over](11.png)

You will notice that execution has continued to the next statement. In the debugger pane, a new local variable has been added. This is because it is not defined explicitly will be implicitly declared at runtime during first usage.

![next step](12.png)

To see the data for this intermediate table variable, right click on it and choose "Display Content".

![display content](13.png)

A new window is then opened showing the data in the table. Review the data and close the window by clicking the "X" in the upper right hand corner.

![review data](14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](View expression editor)]

Click the **Toggle Expression Editor** button.

![toggle expression editor](15.png)

In the yellow box that appears below, enter a SELECT statement as shown here and hit **Enter**.

![select statement](16.png)

You will notice the expression is then added to the "Expressions" section above.

![expressions](17.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View expressions)]

Right click on the expression and choose **Display Content**.

![display content](18.png)

Review the results and close the window by clicking the "X".

![review results](19.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check output)]

Once again click the **Step Over** button. Notice the next intermediate table variables is also added.  You can review the data in this table as well.

![step over](20.png)

Continue to step through the code and when execution stops at the END statement of the procedure, display the contents of the output parameter the same way you did for the intermediate table variables. Finally, close the window by clicking the "X".

![continue steps](21.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Resume execution)]

Click the **Resume** button.

![resume](22.png)

Execution of the procedure is now completed. Return to the SQL tab and check the results.

![results](23.png)

[DONE]
[ACCORDION-END]



## Next Steps
- [Anonymous Blocks](http://www.sap.com/developer/tutorials/xsa-sqlscript-anonymous.html)
