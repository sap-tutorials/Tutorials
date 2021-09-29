---
title: Explore the ABAP Repository
description: Open an ABAP package containing ABAP development objects and find out more about them.
auto_validation: true
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development ]
time: 15
---

## Prerequisites  
 - You have a running ABAP application server (AS). This tutorial was created in the AS ABAP developer edition, 752, download version. For more information, see [SAP Developer Community: Trials and Downloads](https://www.sap.com/developer/trials-downloads.html)
 - **Tutorial**: [Create an ABAP project](abap-create-project)
 - **Tutorial (Recommended)**: [Exploring the ABAP Dictionary](abap-dev-learn-ddic)


## Details
### You will learn  
  - How to find development objects in the ABAP Repository

---

[ACCORDION-BEGIN [Step 1: ](Add a package to Favorites)]
You will start by opening a package with various ABAP Repository object types.

1. In ABAP Development Tools, expand the project that you created, by choosing the arrow on the left:

    ![Image depicting step1-expand-project](step1-expand-project.png)

2. Select **Favorite Packages**, then choose **Add Package…** from the context menu:

    ![Image depicting step1b-add-fave-package](step1b-add-fave-package.png)

3. Enter **`SABAP`** in the search field, then choose the package **`SABAPDEMOS`** from the matching items box:

    ![Image depicting step1c-search-sabap](step1c-search-sabap.png)

The package is added to your favorites:

![Image depicting step1c-package-added](step1c-package-added.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Expand the package)]
Expand the package SABAPDEMOS again by choosing the arrow, then expanding the Source Code Library folder:

![Image depicting step2-expand-package](step2-expand-package.png)

You will see a list of folders of objects in alphabetical order. For the moment you will focus on just 3:

- Executable **programs** also known as **reports**
- **Classes** (part of the object-oriented ABAP Objects)
- **Function modules** (now obsolete - see below)

You will now explore how these concepts fit together.

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Expand the programs folder)]
1. Expand the **Programs** folder, scroll down and choose (double-click) the program **`DEMO_CREATE_STRUCTURED_DATA`**:

    ![Image depicting step3-expand-programs](step3-expand-programs.png)

2. Alternatively, choose **Open development object (`Alt+Shift+A`)** from the toolbar and choose the program **`DEMO_CREATE_STRUCTURED_DATA`**:

    ![Image depicting step3b-open-dev-object](step3b-open-dev-object.png)
    .
    ![Image depicting step3c-open-dev-object2](step3c-open-dev-object2.png)

3. The program opens in a text-based editor:

    ![Image depicting step3d-program-text-ed](step3d-program-text-ed.png)

4. Click on the editor and choose **Link to Editor** from the Project Explorer toolbar.

    ![step5c-link-with-editor](step5c-link-with-editor.png)

    The relevant folders and packages will now open, showing the program in its package in the Project Explorer. This works for any Repository object.

5. The outline also appears:

    ![Image depicting step3e-program-outline](step3e-program-outline.png)

6. Also, if you choose the Properties tab, you can see the package and application component:

    ![Image depicting step3f-program-properties](step3f-program-properties.png)

In the Outline, you can see that the program contains a local class, **Demo**, with a method, **Main**, and, crucially, an event keyword **`START-OF-SELECTION`**. If you look in the main editor, you will see that when the associated event is triggered by the ABAP runtime environment, the program calls the method `Main` of the class `Demo`:

![Image depicting step3g-start-of-selection](step3g-start-of-selection.png)

Without a processing block such as this, the program would not do anything. Also note, if you are completely new to ABAP, but experienced in other languages, that every ABAP statement ends with a period (.).
For more information, see [ABAP Keyword Documentation: START-OF-SELECTION](https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abapstart-of-selection.htm).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run your program)]
Now run your program by choosing **Run (`F8`)** from the toolbar (ignore the other options for now):

![Image depicting step4-run-program](step4-run-program.png)

An ABAP console appears. Change the name of the database table, or number of rows if you want, then choose **Enter**:

![Image depicting step4b-console](step4b-console.png)

The console displays 10 rows of the table in a new tab:

![Image depicting step4c-table-in-console](step4c-table-in-console.png)

Close the console.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open a global class)]
You will now explore ABAP global classes:

1. Back in the main text editor for the program, scroll down till you find the class **`CL_DEMO_INPUT`**.

    ![Image depicting step5-choose-class](step5-choose-class.png)

2. This is a global class. Select it and choose **Navigate (`F3`)** from the context menu. The definition appears in a text-based editor. (The outline also appears):

    ![Image depicting step5b-global-class](step5b-global-class.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Find the class in the Project Explorer)]
If you looked in the Project Explorer for the class **`CL_DEMO_INPUT`**, you will not find it in the package **`SABAPDEMOS`**. If you look in the Properties tab, you will see that **`CL_DEMO_INPUT`** belongs to another package. You can display it in the hierarchy by right-clicking in the text editor and choosing **Show In > Project Explorer**:

![Image depicting step6-show-in-proj-explorer](step6-show-in-proj-explorer.png)

The class appears in its package, **`SABAP_DEMOS_INPUT`**:

![Image depicting step6b-class-in-package](step6b-class-in-package.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Open a function module)]
Finally you will look at **function modules**.

Function modules are managed in function groups and provide two main benefits:

  - They allow you to encapsulate and reuse global functions in an ABAP system.

  - They are also a very powerful feature, in that SAP delivers many built-in function modules – both general such as date and time, or remote system information, and module specific, such as HCM or FI. See these two lists for more details:     
    - [Useful function modules](https://wiki.scn.sap.com/wiki/display/ABAP/Useful+ABAP+Function+Modules)
    - [Commonly-used function modules](https://wiki.scn.sap.com/wiki/display/ABAP/List+of+Commonly+Used+Function+Modules)

>Function modules are now obsolete for new development. You should now use ABAP Objects for modularization. However, you should be familiar with them, since you may need to maintain them or use an existing function module. For more information on obsolete ABAP language elements, see the [ABAP Keyword Documentation: Function Modules](https://help.sap.com/doc/abapdocu_752_index_htm/7.52/en-US/abapfunction.htm).

Now you will inspect a function module:

1. In the Project Explorer, in the package **`SABAPDEMOS`**, expand **Source Code Library > Function Groups**, then expand the group **`DEMO_SPFLI` > Function Modules**. You will see 2 function modules:

    ![Image depicting step7-find-fm](step7-find-fm.png)

2. Choose (double-click on) **`READ_SPFLI_INTO_TABLE`**. The function module opens in a text-based editor:

    ![Image depicting step7b-fm-in-editor](step7b-fm-in-editor.png)

3. Now run your function module by choosing **Run (`F8`)** from the toolbar (ignore the other options for now).

4. The initial screen should look like this. Choose **Run (`F8`)** again.

    ![Image depicting step7-test-fm-1](step7-test-fm-1.png)

5. Double-click on the text **4 entries**.

The output should look roughly like this.

![Image depicting step7c-test-fm-output](step7c-test-fm-output.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

And that's it. You should now be familiar with the most important ABAP Repository objects and the relationship between them.
You can now work through the tutorial mission [Get Started with ABAP Development](mission.abap-dev-get-started).

---
