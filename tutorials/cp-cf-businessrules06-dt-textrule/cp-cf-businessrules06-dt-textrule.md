---
title: Create Decision Table and Text Rules
description: Create and activate the decision tables and text-based rules to create the decision logic.
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>cloud, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: products>sap-business-rules-service
author_name: Vandana Vasudevan
author_profile: https://github.com/VandanaVasudevan
---

## Details
### You will learn
  - How to create decision table rules
  - How to create text rules

A decision table rule is the collection of rule expressions in tabular format and sequentially executes each rule in the table.

A text rule is a decision logic in simple if-then format. The **If** condition of the text rule is evaluated by the system once the rule is triggered. Based on the result of the condition, the rule performs a set of operations.

In this tutorial, we will create the decision logic required to assign equipment for the new hire, both as text rule as well as decision table.

---

[ACCORDION-BEGIN [Step 1: ](Create decision table rules)]

1. Navigate to the **Rules** > **Local Rules** tab, then choose +.

    ![Rules tab](create_dt_1.png)

2. In the **New Rule** prompt, provide the following details:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | **`DetermineEquipmentRules`**
    |  Type          | **`Decision Table`**

    Then choose **Create**.

    ![New Rule Window](create_dt_2.png)

3. In the **New Rule** page, provide the following details and then choose **Settings** icon.

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Label           | **`DetermineEquipmentRules`**
    |  Description     | **`Rule to determine equipment for the new hire based on the country and job title of the new hire.`**

    ![New Rule Page](create_dt_3.png)

    The decision table settings window opens.

4. In the decision table settings window, select the **Hit Policy** as **First Match**. With **Hit Policy** as **First Match**, the decision table returns the first row that matches the condition.

    ![Hit Policy](create_dt_4.png)

5. Press **CTRL** + **SPACE** to load the autosuggestion list, and then choose **Employee** under **Vocabulary** section.

    ![Autosuggestion list](create_dt_5.png)

6. Choose the attribute **`countryofCompany`** from the **Vocabulary** section and then in the **Label** field enter **Country or Region**.

    ![Attribute in autosuggestion list](create_dt_6.png)

7. From the **Fixed Operator** dropdown list, select **None**.

    ![Fixed operator](create_dt_7.png)

8. Similarly, create the following condition expressions:

    |  Condition Expressions     |Label | Fixed Operator
    |  :------------- |:------------ | :-------------
    |  **`Employee.company`**   | **`Company`** | **`None`**
    |  **`Employee.jobTitle`**   | **`Job Title`** | **`None`**
    |  **`Employee.isFullTimeEmployee`**   | **`Full time/Part time`** | **`None`**

    > The label is displayed in the decision table column header.

    Choose **`EquipmentInfo`** as the **Result** data object from the dropdown list and then choose **Apply**.

    ![DT settings](create_dt_8.png)

    A decision table gets created with the condition columns and result columns as shown:

    ![Empty Decision Table](empty_dt.png)

9. In the **`Employee.countryofCompany`** column, press **CTRL** + **SPACE**. Under **Comparison Operators**, select **is equal**.

    ![Comparison Operators](create_dt_9.png)

10. Under **Fixed Value** section, select the value help icon to select the value.

    ![Value help](create_dt_10.png)

    Select **USA** from the value help.

    ![Value help1](create_dt_11.png)

11. In the **`Employee.company`** column provide the following value using the autosuggestion list:

    `='ACE_USA'`

    ![Company details](create_dt_12.png)

12. In the subsequent cells, provide the following values:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  **`Employee.jobTitle`**   | **`MATCHES '.*'`**
    |  **`Employee.isFullTimeEmployee`**   | **`= true OR Employee.isFullTimeEmployee = false`**
    |  **`Currency`**   | **`EUR`**
    |  **`ProductID`**   | **`106743`**
    |  **`Equipment Type`**   | **`Audio and Video`**
    |  **`Equipment Price`**   | **`35.96`**
    |  **`Product Desc`**   | **`Lovely Sound 5.1`**

    ![Company details](create_dt_13.png)

13. Select the first row, then choose **Add Row** > **Insert First**.

    ![Company details](create_dt_14.png)

14. Enter the following values in the condition and result columns of the new row:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  **`Employee.countryofCompany`**   | **`= 'USA'`**
    |  **`Employee.company`**   | **`MATCHES '.*'`**
    |  **`Employee.jobTitle`**   | **`MATCHES '.*'`**
    |  **`Employee.isFullTimeEmployee`**   | **`= true OR Employee.isFullTimeEmployee = false`**
    |  **`Currency`**   | **`EUR`**
    |  **`ProductID`**   | **`106875`**
    |  **`Equipment Type`**   | **`Notebook`**
    |  **`Equipment Price`**   | **`956.00`**
    |  **`Product Desc`**   | **`Notebook Basic 15`**

    Then, choose **Activate**.

    ![Company details](create_dt_15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create text rules)]

1. Navigate to **Rules** > **Local Rules** tab, then choose +.

    ![Create Text Rule](create_text_rule1.png)

2. In the **New Rule** window, enter the following values and then choose **Create**.

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | **`EquipmentApprovalRules`**
    |  Type          | **`Text Rule`**
    |  Mode    | **`Advanced`**

    ![New text rule](create_text_rule2.png)

3. In the **New Rule** page, enter the following details:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Label          | **`EquipmentApprovalRules`**
    |  Description   | **`Rules to determine if the approval is needed for the equipment list or not`**

    In the **Text Rule** section, choose the **Settings** icon.

    ![Text Rule details](create_text_rule3.png)

4. In the **Text Rule Settings** window, select **`EquipmentApproval`** as the **Result** and then choose **Apply**.

    ![Text Rule Result](create_text_rule4.png)

5. In the **If** section, press **CTRL**+**SPACE** and then provide condition expression using the autosuggestion list:

    `Equipment.TotalAmount > 800`

    ![Text Rule if condition](create_text_rule5.png)

     In the **Then** section, provide the following value:

     `'true'`

     Then, choose **Add Else** to an **Else** section.

     ![Else condition](create_text_rule6.png)

6. In this **Else** section, enter the following value:

    `'false'`

    ![Activate text rule](create_text_rule7.png)

    Then, choose **Activate**.

    ![Activate text rule](create_text_rule8.png)

    Navigate to the **Rules** > **Local Rules** tab to view the list of rules as shown:

    ![Some alternative text](create_rules.png)


[VALIDATE_1]

[ACCORDION-END]
