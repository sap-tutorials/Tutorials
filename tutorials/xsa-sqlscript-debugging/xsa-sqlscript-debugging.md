---
title: Debugging Stored Procedures
description: Leveraging SQLScript in stored procedures, user-defined functions, and user-defined libraries.
author_name: Rich Heilman
author_profile: https://github.com/rich-heilman
primary_tag: products>sap-hana
tags: [  tutorial>intermediate, topic>sql, products>sap-hana, products>sap-hana\,-express-edition   ]
time: 10
---

## Prerequisites  
- This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
- **Tutorials:** [Anonymous Blocks](xsa-sqlscript-anonymous)

## Details
### You will learn  
- How to debug a procedure using the SQLScript debugger

The debugging shown in this tutorial includes setting breakpoints, evaluating expressions and intermediate results.

---

[ACCORDION-BEGIN [Step 1: ](Start debugger)]

Go to the Database Explorer page and right-click on the procedure called `get_po_header_data` and choose **Open for Debugging**.

![DBX](1.png)

Make sure the configuration is set as shown here and click **OK**.

![apply](6.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set breakpoints)]

Set breakpoints at the lines shown here by simply clicking on the line number.

![breakpoints](7.png)

Right click on the procedure and choose **Generate Call Statement**

![invoke](8.png)

A new SQL tab will be opened.  Click on the **Run** button.

![SQL tab](9.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](View debugger pane)]

Execution of the procedure has started, and a debugger pane is now visible which is displayed on the right. You can see all of the variables and parameters for this procedure.  You might notice that this pane is currently not showing the intermediate table variables at this point.

![execute procedure](10.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use step over)]

Click the **Step Over** button.

![step over](11.png)

You will notice that execution has continued to the next statement. In the debugger pane, a new local variable has been added. This is because it is not defined explicitly and will be implicitly declared at runtime during first usage.

![next step](12.png)

To see the data for this intermediate table variable, select it and click **Display Content**

![display content](13.png)

A new window is then opened showing the data in the table. Review the data and close the window by clicking **Close**

![review data](14.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](View expression editor)]

Click the **Expression** tab and click **+** button.

![toggle expression editor](15.png)

In the following window, enter a SELECT statement as shown here and hit **Add**.

![select statement](16.png)

You will notice the expression is then added to the "Expressions" section.

![expressions](17.png)


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](View expressions)]

Select the expression and click  **Display Content**.

![display content](18.png)

Review the results and close the window.

![review results](19.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check output)]

Once again click the **Step Over** button. Notice the next intermediate table variables is also added.  You can review the data in this table as well.

![step over](20.png)

Continue to step through the code and when execution stops at the END statement of the procedure, display the contents of the output parameter the same way you did for the intermediate table variables. Finally, close the window by clicking the **Close**.

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
