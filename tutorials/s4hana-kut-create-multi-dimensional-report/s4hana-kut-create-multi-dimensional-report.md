---
title: Create a Multi-Dimensional Report from an Analytical Query
description: Use the SAP released CDS View that fulfills your business needs to create an analytical report.
auto_validation: true
primary_tag: products>sap-s-4hana
tags: [  tutorial>intermediate, topic>abap-development, products>sap-s-4hana ]
time: 30
---

## Details
### You will learn  
  - How to create a multi-dimensional report from a CDS query

---

[ACCORDION-BEGIN [Step 1: ](Execute app Custom Analytical Queries)]

Go back to the SAP Fiori launchpad home, select and execute from the group **Query Design** > **Custom Analytical Queries**.
![Title screen](report_aq_01.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Search and customize relevant query)]

Search for the query **`ZC_APSEMTAGSTATISTICAL`**
![Relevant query](report_aq_02.png)

Copy the query  **`ZC_APSEMTAGSTATISTICAL`** by checking it and clicking **Copy**.
![Query Report](report_aq_03.png)

In the **Copy Source Query** window, provide the new query name:
**`APSEMTAGSTATISTIC_XXXX`**  
>Replace  XXXX with your initial (e.g., **`APSEMTAGSTATISTIC_1111`**).

Click **OK**.

![Extension of the query](report_aq_04.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Explore query and choose your fields)]

Explore the details of the Tab General:

  - Label
  - Name
  - Data Source

![Tab General](report_aq_05.png)

Explore **Field Selection** tab, including the available fields and selected fields.

![Tab Field Selection](report_aq_06.png)

Explore the details of the **Display** tab, including the display fields and properties.

![Tab Field Selection](report_aq_07.png)

Explore the details of the **Filter** tab, including the filter fields and filters.

![Tab Filter](report_aq_08.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Activate and publish query)]

Go back to the **General** tab. Click **Publish** to activate and publish your query with the name **`ZZ1_APSEMTAGSTATISTIC_XXXX`** where you must replace **`XXXX`** with your initials.

![Activation and publishing](report_aq_09.png)

After publishing, the status will change from **Draft** to **Publish**. You will have a similar picture to this one:

![Publishing status](report_aq_10.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Preview content of query)]

Go back to the SAP Fiori launchpad home, select and execute from the group Query Design the App View Browser and show the content of your query **`ZZ1_APSEMTAGSTATISTIC_XXXX`**  by maintaining the fields on the **Prompts** screen with following values:

|  Field Name                | Value
|  :-------------------------| :-------------
|  To-Period                 | `7`
|  Category                  | `ATC01`
|  Fin Statement Version     | `L000`
|  Fiscal Year               | `2018`
|  Controlling Area          | `A000`

![Content Preview one](report_aq_10_01.png)

![Content Preview two](report_aq_10_02.png)

![Content Preview three](report_aq_10_03.png)

![Content Preview four](report_aq_10_04.png)

![Content Preview fIVE](report_aq_10_05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Explore content of  query)]

1. Drill-down by segment.

    ![Drill-down by Segment](report_aq_11.png)

2. Drill-down by cost center.

    ![Drill-down by Cost Center](report_aq_12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create custom key figure for report)]

1. Go back to the SAP Fiori launchpad home, select and execute from the group **Query Design** the app Custom Analytical Queries. In the search field, enter `ZZ1_APSEMTAGSTATISTIC_XXXX`, which is the name of the view you have created previously, and then click the search button.

    ![Custom key figure01](report_aq_12_01.png)

2. Check the view **`ZZ1_APSEMTAGSTATISTIC_XXX`** and afterwards click the arrow button to display the details of the query, and afterwards click **Edit**.

    ![Custom key figure02](report_aq_12_02.png)

3. Move to the **Display** tab, click **Add** and double-click **Add Calculated Measure**. You will see a screen similar to the one below:

    ![Custom key figure03](report_aq_12_03.png)

4. In the screen **New Calculated Measure**, maintain the fields as follow:

    |  Field Name                | Value
    |  :-------------------------| :-------------
    |  Label                     | `Net Profit Amt by Empl in Glob Crcyxxxx`

    > The field Name will be filled automatically by the system !

    Afterwards, click **OK**.

    ![Custom key figure04](report_aq_12_04.png)

5. A new field labelled `Net Profit Amt by Empl in Glob Crcyxxxx` appears on the right of the screen, showing the properties of the newly created field.

    ![Custom key figure05](report_aq_12_05.png)

6. On the properties panel of the newly created field, click **Edit** to open the expression editor in order to insert the formula for the calculation of the value of the key figure `Net Profit Amt by Empl in Glob Crcyxxxx`.

    ![Custom key figure06](report_aq_12_06.png)

7. In the expression editor, enter the following expression for the calculation as depicted in the following pictures:

    ![Custom key figure07](report_aq_12_07.png)

    ![Custom key figure08](report_aq_12_08.png)

    ![Custom key figure09](report_aq_12_09.png)

    ![Custom key figure10](report_aq_12_10.png)

    Afterwards, click **OK**.

    ![Custom key figure11](report_aq_12_11.png)

13. Finally, you will have a screen similar to the one below. After exploring this, click the **Save Draft** to save all your work in this step.

    ![Custom key figure12](report_aq_12_12.png)

14. After you save you work, click **Publish** to make it available for use. Doing so will change the status of the query from draft to publish. Afterwards, click **Preview** to see your new key figure field.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create launchpad tile for report)]

1. Click the **Action** icon.

    ![Action icon](report_aq_13.png)

2. Click **Save as Tile**.

    ![Save as icon](report_aq_14.png)

3. In the **Save Tile** window, maintain the fields as follows:

    - **Title:** Actual Cost and Headcount XXXX Report
    - **Subtitle:** Filter by Cost Center
    - **Group:** My Home
    - **Save title:** `report_aq_15.png`

4. Afterwards click on the OK button

    ![Save tile OK](report_aq_16.png)

5. Go to SAP Fiori launchpad home page, and navigate to the group **My Home**, and
make sure that you see a tile labeled with **Actual Cost and Headcount XXXX Report**.

    ![Tile in Fiori Launchpad](report_aq_17.png)


[DONE]
[ACCORDION-END]
