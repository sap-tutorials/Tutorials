---
title: Inspect Your Class in the ABAP Debugger and Feed Reader
description: Work with ABAP classes in the ABAP Debugger in (ADT) for both a console application and SAP Fiori application.
auto_validation: true
time: 20
tags: [ tutorial>beginner, topic>abap-development, products>sap-cloud-platform, tutorial>license]
primary_tag: products>sap-cloud-platform--abap-environment
---

##Prerequisites
- You have done one of the following:
    - **Tutorial**: [Test the Connection to the Remote System](abap-environment-test-rfc), step 10 - for steps 1-10 of this tutorial
    - **Tutorial**: [Get Data from a Remote System Using a Custom Entity](abap-environment-rfc-custom-entity)

## Details
### You will learn
  - How to debug an ABAP Console application in ABAP Development Tools (ADT)
  - How to debug an ABAP class that provides the business logic for a SAP Fiori application


This tutorial will get you familiar with the relevant tools, whether you are an ABAP newbie, experienced in SAPUI5 development, or an ABAP developer who is new to ADT / SAP Cloud Platform, ABAP Environment.

For more information, see:

- [SAP Help Portal: SAP Cloud Platform (Concepts): ABAP Debugger](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/4ec365a66e391014adc9fffe4e204223.html)
- [SAP Help Portal: SAP Cloud Platform (Tasks): Debugging ABAP Code](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/4ec33a996e391014adc9fffe4e204223.html)

---

[ACCORDION-BEGIN [Step 1: ](Duplicate your class)]
1. Select your class, `ZCL_OUTPUT_TEST_XXX` and choose **Duplicate** from the context menu.

    This is clearly not standard practice. However, you are working with the ABAP Debugger for the first time, and also adding some dummy code to your class.

2. Enter a name, e.g. `ZCL_OUTPUT_TEST_DEBUG_XXX` and choose **Finish**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Change ABAP Debugger setting to user)]

1. In the Project Explorer, select your project and choose **Properties** from the context menu.

    ![Image depicting step2-properties](step2-properties.png)

2. Choose  **ABAP Development > Debug**.

3. Change the setting **Breakpoint activation ...** to **User** and enter your logon user.

    ![Image depicting step2a-debug-user](step2a-debug-user.png)

4. Choose **Apply and Close**.
Your class, which includes an RFC request, that is an external request to a different system / project. This enables you to debug the class.

Note that breakpoints in the ABAP Development Tools (ADT) are by default external user breakpoints. For more information, see: [Breakpoints - Characteristics](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/4ec121276e391014adc9fffe4e204223.html?q=external)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add dummy code and breakpoints)]
1. Right at the start of the method, add some simple code, e.g.

    ```ABAP
    IF 0 = 1.
    ENDIF.
    ```

2. At the statement `IF 0 = 1.`, set a breakpoint by double-clicking the ruler.

    ![Link text step3a-add-bp](step3a-add-bp.png)

3. Repeat this - add the same code, then add a breakpoint - right at the end of the code, just before `ENDTRY`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run your application)]
1. Run your application in the console by choosing **`F9`**.

2. As soon as the first breakpoint is reached, a pop-up window suggests that you switch to the **Debug** perspective. Choose **Switch**.

    ![Image depicting step5-switch-perspective](step5-switch-perspective.png)

The Debugger perspective opens.

![Image depicting step5b-debugger](step5b-debugger.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add variable to list)]
1. Switch to the **Variables** tab (to the right of the Class Editor).

2. Add the system field **`SY-TABIX`** to the list, by clicking: **` < Enter Variable > `**

    This field is filled by the runtime system. You can then use them in programs to query the system status.

    ![Link text step5a-sy-tabix](step5a-sy-tabix.png)

    For a list of system variables, see: [ABAP System Fields](https://help.sap.com/doc/saphelp_nw70ehp3/7.03.19/en-US/7b/fb96c8882811d295a90000e8353423/frameset.htm)

3. Expand **Locals**.

4. Select the internal table **`lt_product`** and choose **Show in Table View** from the context menu.

    ![Link text step5c-table-view](step5c-table-view.png)

The internal table appears in a new tab in the bottom panel.

![Link text step5d-table-view](step5d-table-view.png)    

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Step through the program)]
1. Switch to the Class Editor. You have 4 options for stepping through the program.

    - Step Into (`F5`)	Execute the next single ABAP instruction in the program in the debugger. Step into a called procedure.

    - Step Over (`F6`)	Execute the next ABAP statement. If the next step is a procedure call, run the entire procedure.

    - Step Return (`F7`)	Run until the current procedure returns to its caller or until the program ends.

    - Run to Line (`Shift F8`)	Run to the statement on which the cursor is positioned.

    ![Link text step6a-step-functions](step6a-step-functions.png)


2. Step through the first few lines of the program line by line using **`F5`** - **UNTIL** you get to the statement `DATA(lo_rfc_dest) = cl_rfc_destination_provider=>create_by_cloud_destination...`.

3. Since this statement calls a system class, which you **do not** want to debug it. Execute these two `DATA` statements using **`F6`**.

4. When you get to `CALL FUNCTION`, **STOP**.
You cannot execute this remote function in the Debugger, but you cannot debug it. If you choose `F5`, the Debugger will hang. You will have to terminate the Debugger session.
Therefore, simply execute this using `F7`.

Look at the **Table View**. `lt_product` is filled with the data from the BAPI.

![Link text step7a-filled-itab](step7a-filled-itab.png)

However, in the **Variables View**, `ls_product` is still empty. `SY-TABIX` = 25, the total rows in the table imported.

![Link text step6c-structure-empty](step6c-structure-empty.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Step through LOOP statement)]
1. Proceed until you get to the statement `LOOP AT lt_product INTO ls_product.`.

2. Step through the `LOOP...ENDLOOP.` using `F5`. Note that the variable `SY-TABIX` starts at **1**, then increments by 1 for each loop pass.

3. Exit the Debugger by choosing **Terminate** from the main tool bar.

    ![Link text step7c-terminate](step7c-terminate.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Set breakpoint for an ABAP statement)]
To jump straight from your first breakpoint to the `CASE` statement:

1. Choose **Run > ABAP Breakpoints > Add Statement Breakpoint...**

    ![Link text step8-add-statement-bp](step8-add-statement-bp.png)

2. Enter `CASE`, keep the setting **Soft Breakpoint** (so that you debug `CASE ` statements only in your own class, not the whole stack), then choose **OK**.

    ![Link text step8b-bp-for-case-statement](step8b-bp-for-case-statement.png)

3. Run the Debugger again by choosing `F9`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Set watchpoint for variable with a condition)]
You may want to stop, not at a specific statement, but when a variable hits a specific value.
To do this, run the Debugger again and proceed as follows:


1. For clarity, you may wish to deactivate your `CASE` statement breakpoint in the **Breakpoints View**.

    ![Link text step9a-deactivate-case-bp](step9a-deactivate-case-bp.png)

2. In the **Class Editor**, select the field `ls_product-suppliername` and choose **Set `Watchpoint`** from the context menu.

    ![Link text step9b-select-field](step9b-select-field.png)

3. In the **Breakpoints View**, choose the `watchpoint` and enter the condition **`LS_PRODUCT-SUPPLIERNAME = 'AVANTEL'`**. Do not forget the single quotation marks.

    ![Link text step9e-watchpoint-avantel.png](step9e-watchpoint-avantel.png)  

4. If you switch to the **Variables View**, you can monitor the values of the variable as you step through the loop.

    ![Link text step9d-watchpoint-in variables-view](step9d-watchpoint-in variables-view.png)

5. When the Debugger hits the correct row in the table, it stops.

    ![Link text step9f-avantel-in-variables-view](step9f-avantel-in-variables-view.png)

6. When you are satisfied, terminate the Debugger.

You can define a wide range of complex conditions for breakpoints and `watchpoint`. For more information, see [SAP Help Portal: SAP Cloud Platform: ABAP Development User Guide: Adding Conditions to Breakpoints](https://help.sap.com/viewer/5371047f1273405bb46725a417f95433/Cloud/en-US/162f24582b7540d2b1dc05a87b4874da.html)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Set watchpoint for  variable with condition for table row index)]
You can also specify a specific value for a different variable.

1. Start the Debugger again. Unlike a breakpoint, a `watchpoint` lasts only for the current Debugger session.

2. In the **Class Editor**, select `ls_product-suppliername` and choose **Set `Watchpoint`** again.

    ![Link text step9b-select-field](step9b-select-field.png)

3. In the **Breakpoints View**, choose the `watchpoint` again. This time, enter the condition `SY-TABIX = 4`.

    ![Link text step9c-watchpoint-sy-tabix](step9c-watchpoint-sy-tabix.png)

4. When the Debugger hits the correct row in the table, it stops.

    ![Link text step9g-table-view-pears](step9g-table-view-pears.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Open ABAP Debugger from Fiori Elements Preview)]
The class that you created previously (in [Get Data from a Remote System Using a Custom Entity](abap-environment-rfc-custom-entity)) is not displayed in the ABAP Console. However, you can start the ABAP Debugger for it as follows:

1. Again, duplicate the class, in this case `zcl_product_via_rfc_xxx`.

2. Open your custom entity, `zce_product_xxx`.

3. Change the name of the implementing class to, e.g. `ZCL_PRODUCT_DEBUG_XXX` (upper case); then **Save and Activate ( Ctrl+S, Ctrl+F3 )** the custom entity.

    ```CDS

    @ObjectModel.query.implementedBy: 'ABAP:ZCL_PRODUCT_DEBUG'

    ```

4. Open your service binding, `ZSB_PRODUCT_XXX` and choose **Preview**.

    ![Image depicting step14-preview](step14-preview.png)

5. Log in using your ABAP Environment user and password.

    The SAP Fiori elements preview then appears.

6. Display the data by choosing **Go**.

    ![Image depicting step14b-preview-with-data](step14b-preview-with-data.png)

You can also debug your application, displayed in the SAP Fiori elements preview, in the browser. This is  beyond the scope of this tutorial, but for more information, see:

- [SAPUI5: UI Development Toolkit for HTML5: `Walkthrough` : Debugging](https://help.sap.com/doc/saphelp_uiaddon20/2.05/en-US/c9/b0f8cca852443f9b8d3bf8ba5626ab/frameset.htm)
- [Browser Debugging for ABAP Developers](https://help.sap.com/doc/saphelp_uiaddon20/2.05/en-US/c9/b0f8cca852443f9b8d3bf8ba5626ab/frameset.htm)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]

---
