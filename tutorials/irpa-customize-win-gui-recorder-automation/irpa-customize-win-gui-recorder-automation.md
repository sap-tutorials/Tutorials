---
parser: v2
author_name: Ilyes Yamoun
author_profile: https://github.com/shielddz
keywords: RPA
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-cloud-sdk]
primary_tag: software-product>sap-intelligent-robotic-process-automation
---

# Customize automation generated through Recorder
<!-- description --> Customize Sales Orders Creation (SAP Win GUI) Bot which is generated using Recorder

## Prerequisites
  - [Create automation using Recorder](irpa-win-gui-recorder)

## You will learn
  - How to customize a **Recorder** generated Automation

---
### Customize Application

Open the Cloud Studio project where you recorded the application using Recorder.

<!-- border -->![Generated automation](step2-resultant-automation.png)

1.  Select the **Sales order Recorder** application in the Cloud Studio project.
2.  Select the screen containing the order details.
3.  Select the field containing *Material*.
4.  Rename the material field.
5.  Select **Remove** the **Id** **Criteria**.
6.  Select **Name** to add it as a **Criteria**.
7.  Select **Is a collection** <!-- border -->![Is a Collection icon](step2-is-a-collection-icon.PNG).

    > **Is a collection** is used to declare a field as a column containing multiple fields rather than just one field.

8. Repeat the steps 3-7 for the remaining two fields ( *Order Quantity Field* and *Storage location Field* ).

    <!-- border -->![Modify application fields](step2-application-modification.png)

The application now recognizes *Material*, *Order Quantity* and *Storage Location* as columns and not as fields.

  <!-- border -->![Resultant Fields](step2-result.png)



### Customize Automation


1. Create an Excel file with the order details.

    <!-- border -->![Excel File](step3-excel-file.png)

    > This data is used to create the **Sales order** in SAP ERP. Change this data according to your **SAP ERP** system.

2. Select **Sales Order Recorder Automation** in the project.

3.  **Adding the required activities to the automation**.

    - Add **Open Excel Instance** Activity before the **Start Application** activity.

    - Add **Close Excel Instance** Activity before the **Terminate Application** activity.

    - Add **Excel Cloud Link** activity after the **Open Excel Instance** activity.

4.  **Configure Excel Cloud Link activity**.

    -  Select the activity.

    -  Select **Edit Activity**.

    -  Browse and select the Excel file.

    <!-- border -->![Configure ECL](step3-excel-cloud-link-configure.png)

    The file is loaded to the activity.

    -  Put the path of the file in the field.

    -  Select **+ From Excel Data** to create the Order details **Data Type**.

    <!-- border -->![Configure ECL PART2](step3-excel-cloud-link-configure2.png)

    A pop-up window opens.

    - Set a name and a description for the **Data Type**.

    <!-- border -->![Order Data Type](step3-order-data-type.png)

    - To add all the rows from the Excel file, you require adding a **For each** activity.

    > The **For each** activity is used for *looping* through Data.

5.  **Looping through data**.

    In the automation, go to the **Create Standard Order: Overview** *screen* and add a **For each** activity just above the fields that need to be included in the repetitive process ( *Material*, *Order Quantity* and *Storage Location* ).

    <!-- border -->![For each Activity](step3-for-each-activity.png)

    Drag and drop the three **Set Element - X** activities inside the **For each** activity.

    - Select **For each** activity.

    - Select **returned Values** option on **Set looping list**.

    > **returned Values** references the data returned from the **Excel Cloud link** activity.

    <!-- border -->![Setup For each Activity](step3-setup-for-each-activity.png)

    Map the excel data of the current row to the corresponding field.

    -  Select the first field (Corresponding to *Material*).

    -  Set the value as **current Member** and click **currentMember.material** on the drop down menu that shows up.

        > **current Member** references the excel row used in the iteration of the **For each** loop.

        <!-- border -->![Set material Value](step3-set-element-material.png)

    -  Select **Open the target Editor**.

    -  Select **Material Field**.

    -  Set **Index of the Element** to **index**.

    -  Select confirm.

        <!-- border -->![Set Target index](step3-set-element-material-2.png)

    Some fields are stored as numbers in excel (such as **Order Quantity** and **Storage Location**). Convert them to strings (succession of characters).

    -  Select the second field (Corresponding to *Order Quantity*).

    -  Select **Open expression editor**.

        <!-- border -->![Set orderQuantity](step3-set-order-quantity.png)

    -   Enter **Step27.currentMember.orderQuantity.toString()**.

    > *Step27* references the **For each** **activity**.

      <!-- border -->![Set orderQuantity Value](step3-set-order-quantity-value.png)

    -  Select **Open the target Editor**.

    -  Select **Order Quantity Field**.

    -  Set **Index of the Element** to index.

    -  Choose **Confirm**.

        > Repeat the same steps for **Storage Location** field as for the **Order Quantity** field in the previous steps.

        <!-- border -->![Set orderQuantity Value](step3-set-order-quantity-2.png)



### Create Environment Variables


**Environment variables** allow you to reuse certain information for a given environment.

You use **environment variables** to pass parameters to automations. You can create environment variables in the **Cloud Studio** for which you can later set values across specified environments.

Create two **environment variables** (Password and Excel file path).

1.  Select **Manage this project properties**.

2.  Select **Environment Variables**.

3.  Select **Create**.

    <!-- border -->![Create Environment Variable 1](step4-create-env-var-1.png)

4.  Fill in the *password* **environment variable**'s **Identifier**, **Description** (optional) and **Type** then Select **Create**.

5.  Repeat step 4 for *excel_ file_ path* **environment variable**.

    > Type of environment variables depends on the use case. It can be a **String**, **Password**, **Number**, **Date**, etc.. .
    >
    > **environment variables**' Identifier can't contain spaces. Replace spaces with *underscores*.

    |  Identifier      | Description (Optional)               | Type
    |  :-------------- | :----------------------------------- | :-----------
    |  password        | SAP Logon password                   | **Password**
    |    excel_ file_ path | Excel file Path for Excel Cloud Link | **String**

    The two **environment variables** are created.

    <!-- border -->![Create Environment Variable 2](step4-create-env-var-result.png)



### Use Environment Variables in Automation

To use the created **environment variables** in the automation go to the activities corresponding them.

1.  Select **Set Element** of password activity in the automation.

2.  Search and select the *password* **environment variable**.

    > **Environment Variables** have the letter **E** on the left side to distinguish them. <!-- border -->![Distinguish Environment Variables](step4-E.png)

    <!-- border -->![Use password Environment Variable](step4-password-variable.png)

3.  Select the **Excel Cloud Link** activity in the **automation**.

4.  Select **Edit Activity**.

5.  Search and select the *excel_ file_ path* **environment variable**.

    <!-- border -->![Use excel_ file_ path Environment Variable](step4-file-variable.png)

    Both of the **environment variables** were assigned to their correct value fields.


### Test your Automation


Choose **Test** and you will be prompted to put in values for the **environment variables**.

<!-- border -->![Test Application](step4-test-prompt.png)

The process operates as follows:

1.  SAP ERP system is opened, enters the credentials and navigates to Sales order Transaction.

2.  The Excel file which is stored on your local machine containing Sales order details is opened.

3.  The **Desktop Agent** fills all the details in the screen.

4.  Information is validated to move to the next detected screen.

5.  These steps are repeated for all the screens that were captured.

6.  The Sales order is created successfully with data in the Excel.

---
