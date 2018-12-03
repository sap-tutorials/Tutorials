---
title: Create and run an ABAP application
description: Create and run an ABAP application based on tables from the sample EPM data model.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development ]
time: 10
---

## Prerequisites  
[Create an ABAP project in Eclipse](https://developers.sap.com/tutorials/abap-create-project.html)

## Details
### You will learn  
In this tutorial you will learn how to create and run an ABAP application based on tables of the SAP NetWeaver Demo sample EPM Model (details in the SAP Community [The SAP NetWeaver Enterprise Procurement Model – An Introduction](https://archive.sap.com/documents/docs/DOC-31458). So you can reiterate the tutorial on any SAP NetWeaver 7.5 system.  


---

[ACCORDION-BEGIN [Step 1: ](Create New ABAP Program)]
In the toolbar, select the **New** icon, then choose **ABAP Program...**:

![Image depicting create new](abap03-1a.png)

A wizard appears to create a new ABAP Program. Enter **`z_invoice_items_euro`** in the name field. Enter a meaningful text in the **description field**. Click **Finish** to create the report.
Afterwards an editor will be opened which shows the empty report.

![new ABAP program](abap-03-2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create ABAP local class)]
Type `lcl` and press `Ctrl+Space` to get code completion proposals. Select the code template for the insertion of a local class by double-clicking on `lcl - Local class` or press `Enter` if the right entry is already selected.

![local class value help](abap-03-3.png)

Adjust the name of the local class to `lcl_main` using inline editing.

![local class](abap-03-4.png)


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create CREATE method)]
Position the cursor in the class definition statement and press **Ctrl+1** to open the Quick Fix menu. Double-click on **Generate factory method create** to create a static factory method.

![create method](abap-03-5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Implement RUN method)]
a. Add the definition of an additional instance method **run** by entering **`methods run.`** in the public section.
b. Then position the cursor on the method name and press **Ctrl+1** to open the Quick-Fix menu. Double-click on **Add implementation for run**.

![run method](abap-03-6.png)

Implement a simple **WRITE** statement in the method **run**.

![write statement](abap-03-7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Make ABAP program runnable)]
a. Add a **START-OF-SELECTION event** to your report
b. Create an instance of the local class `lcl_main`.
c. Call the **run** method.
That is, enter the following code:

```ABAP
START-OF-SELECTION.
    lcl_main=>create( )->run( ).
```

![create run statement](abap-03-8.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Save and Activate ABAP Program)]
Save and activate your program by clicking on the **Save** and **Activate** buttons in the toolbar.

![save and activate program](abap-03-9.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Run ABAP Program)]
Press **F8** to run your program. An embedded SAP GUI will open inside of an Eclipse editor. After checking the output of the report click on the **Close** icon   to close the tab.

![run program](abap-03-10.png)


[ACCORDION-END]
