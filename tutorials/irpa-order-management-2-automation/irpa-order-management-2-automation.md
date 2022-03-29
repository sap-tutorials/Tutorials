---
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
title: Build an Automation for Orders Management
description: Build an automation to collect order details for all the orders and print them in an Excel file.
keywords: RPA
auto_validation: true
time: 30
tags: [ tutorial>beginner, software-product>sap-intelligent-robotic-process-automation]
primary_tag: software-product>sap-intelligent-robotic-process-automation
---

## Prerequisites
- [Capture Orders Management Web Application](irpa-orders-management-1-capture)

## Details
### You will learn
  - How to create an automation in Cloud Studio
  - How to use Excel activities in an automation
  - How to set the columns for Order Number and Status in the Excel workbook
  - How to get the order number and order status from the [Browse Orders application](https://openui5.hana.ondemand.com/test-resources/sap/m/demokit/orderbrowser/webapp/test/mockServer.html)
  - How to close all open applications

---

[ACCORDION-BEGIN [Step 1: ](Check dependencies)]
1. Check the dependencies by choosing the **Menu** &nbsp; !![Menu](menu.png) in the Project Explorer.

    >Alternatively, you can choose the **manage the project properties** icon ![manage project properties icon](manage-project-properties.png).

2. Select **Dependencies** from the dropdown list.

    !![select-dependencies](select-dependencies.png)

3. Choose **Manage Dependencies** icon.

    !![manage-dependencies](manage-dependencies.png).

4. From the **Project Properties** popup screen, choose **Dependencies** to manage your project dependencies.

    !![check-dependencies](check-dependencies.png)

By default, SDK **`irpa_core`** and SDK **`irpa_excel`** are automatically added to the project on creation of automation.

Now that you checked the dependencies, you can create your automation.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create the automation artifact)]
1. Choose !![Plus](plus.png), then hover over **Create** and select **Automation** from the artifact dropdown.

    !![choose-automation](choose-automation.png)

2. From the **Configure agent version** popup, select the **Desktop Agent version** installed on your machine and choose **Confirm**.

    !![configure-desktop-agent](configure-desktop-agent.png)

3. In the **Create Automation** popup, enter the following information:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | **`Collect Order Details`**
    |  Description    | **`An automation to collect order number and status`**

    The **Identifier** field value is populated automatically.

4. Choose **Create**.

    !![Create Automation](create-automation.png)

    The automation workflow opens in a new tab in Cloud Studio.

    The **automation designer** opens and you can drag and drop activities, data types, controls, applications and other automations from the side panel into the workflow.

    !![Workflow](workflow.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add Excel activities)]
1. Search for the **Open Excel Instance** activity from the list of tools available in the **Automation Information** panel.

    !![search-open-excel-instance](search-open-excel-instance.png)

2. Drag-and-drop the activity in the workflow, where they become a step in the automation.

    Each step has its associated properties.

    !![open-excel-instance-workflow](open-excel-instance-workflow.png)

3. Search for the **Add Workbook** activity and drag-and-drop it below the Open Excel Instance activity.

4. Drag-and-drop a **Set Values (Cells)** and select the activity to open its properties to make the following changes:

    - Step name : **Set Order Number**

    - Range Definition: **`A1`**

    - Values: **`Order Number`**

    !![Set order number](set-order-number.png)

5. Similarly, drag-and-drop a **Set Values (Cells)** activity and make the following changes to its properties:

    - Step name : **Set Order Status**

    - Range Definition: **`B1`**

    - Values: **`Order Status`**

6. Save your work.

    After completing the above steps, your automation should look like this.

    !![automation-1](automation-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add application to automation)]
1. From the **Tools** panel, expand the **Screens** node and drag-and-drop the previously captured screen **Browse Orders** into the workflow.

      !![Browse Orders](browse-orders.png)

2. Choose **Browse Orders** and choose **Define Screen Activities** to view the activities related to **screen**.

    !![Define Screen activities](DefinescreenActivities.png)

    Drag-and-drop the **Set Navigator** activity into the **Browse Orders** screen and click the step to set the following properties:

    -	Target : **Browse Orders**

    -	Navigator : **`Chrome`**

    !![Set Navigator](set-navigator.png)

    >Setting the navigator to Chrome will ensure that it opens using the Chrome browser if the application does not open with another navigator.

3. To allow the web page to fully load, drag-and-drop the **Wait (Screen)** activity inside the application. Set the **Target** as `Browse Orders`.

    !![Wait activity](wait-activity.png)

    The automations looks like:

    !![Step4output](Step4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Define screen activities)]
1.  Choose **Browse Orders** in the workflow and from the **Parameters** panel, choose **Define Screen Activities**.

    !![Define Screen Activities](define-screen-activities.png)

2. A screen preview is shown. The elements declared appear in a green box.

    !![Screen preview](screen-preview.png)

3. From the list of activities, search for **Get Element** activity. Drag-and-drop the activity in the screen preview on **Order 7991**. The box will turn from green to blue.  

    > ### What's going on?
    Here, you are defining activities on the screen elements. The **Order 7991** has been declared earlier as Order Number. The element is a Collection, thus, any activity applied to this element will be automatically applied to other instances of the same element. So, a **For** loop will be generated automatically in the workflow.

    !![Get element Order number](get-element-order-number.png)

4. Select **Get Element** activity to edit the following properties.

    -	Step Name : **Get Order Number**

    - Target : `currentMember`

    -	Output Parameters : `orderNumber`

5. From the **Tools** panel, search **number**. Select the `Number` data type within the **Data** node.

6. Drag-and-drop the **Number** data type within the **For Each** loop below the Get Order Number activity.

    !![create number variable](create-number-variable.png)

7. Select and edit the activity **Create Number Variable**.

8. Choose **Open the Expression editor** to open the **Expression Editor**.  

      !![expression editor icon](expression-editor-icon1.png)

9. In the **Expression Editor**, enter this text below and click **Save Expression**:

    ```Expression
    Step7.index + 3
    ```

      !![edit-expression](edit-expression.png)

10. Rename the **Output Parameters** as `myExcelRow`. The step will now look like this:

      !![create-number-variable-final](create-number-variable-final.png)

11. Drag-and-drop **Set Values (Cells)** activity in the **For** loop below Create Number Variable activity.

    Edit the properties as shown below. Use the **Expression Editor** to edit **`rangeDefinition`**.

    !![set-order-number-for-loop](set-order-number1.png)

12. Repeat Steps 4-11 for the element **Order Status** using the following information:

    |  Step           | Step Name           | Input Parameters        | Output Parameters
    |  :------------- | :------------------- |
    |  Get Element    | **`Get Order Status`** | target : **`currentMember`** | **`orderStatus`**
    |  Create Number Variable           | **`Create Number Variable - 2`**  | value: **`Step11.index + 3`**  | **`myExcelRow`**
    |  Set Values (Cells)    | **`Set orderStatus`**  | range Definition : **`"B" + Step13.myExcelRow`**  values: **`orderStatus`** |   |

    !![automation-3](automation-3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Close and release application)]
You will now close the Browse Orders application, saving the worksheet and releasing the Excel instance.

1. Search for **Close Screen** activity and drag and drop it in the **Browse Orders** application outside the **For** loop.

    !![close-screen](close-application.png)

2. Search for **Save as Workbook**, drag-and-drop into the workflow outside the Browse Orders application.

3. In the activity's properties, provide an appropriate **`filePath`** (for example, `C:\some Path\myOrdersExcel.xlsx`) to store the Excel file.

    The resulting Excel is stored in this path:

    !![save-as-workbook](save-as-workbook.png)

4. At last, drag-and-drop the **Release Excel Instance** activity into the workflow.

5. Choose **Save**.

You have completed building the automation and it should look like this.

  !![complete-automation](complete-automation.png)

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test the automation)]
You will now test the automation. Result of this automation should be the saved Excel file with all the order numbers and their order status.

To test the automation, choose !![test icon](Test.png).

  !![Test](TestAutomation.png)

Here's the Excel:

  !![result](result.png)

Congratulations! You have successfully completed the tutorial for automating Order Management.

[DONE]
[ACCORDION-END]
---
