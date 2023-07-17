---
parser: v2
auto_validation: true
primary_tag: programming-tool>abap-development
tags: [  tutorial>beginner, programming-tool>abap-development, programming-tool>abap-extensibility ]
time: 15
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

# Create Custom Analytical Queries With Calculated Measures
<!-- description --> Create a Custom Analytical Queries with Calculated Measures.

## Prerequisites  
The Business user requires the Role `SAP_CA_BC_ANA_AQD_PC` to access the Query Designer.
Furthermore `ABAP Custom Analytical Queries` as a tutorial reveals another Prerequisite.

## You will learn  
This tutorial teaches you the creation of a Custom Analytical Query and the addition of a Calculated Measures. You will also be able to edit your calculated Measures and define an Exception Aggregation.

## Intro
The Application Custom Analytical Queries characterizes the usage of reporting and analysis.
Raw data from business documents are getting converted into a meaningful grid.
By using Custom Analytical Queries you do not need to understand the technical details.


---
### Open Custom Analytical Queries Application

Start the **Custom Analytical Queries** Application by selecting the corresponding tile in the SAP Fiori Launchpad.

![start Custom CDS View App](FLP.png)


### Select your Analytical Query

To edit your analytical query you have to search for it and **select** it.
Furthermore you have to click on the **Edit** button to continue.

![start Custom CDS View App](select.png)


### Add Calculated Measures

Now you have to switch to the **Display** Tab to add a calculated measure.

![start Custom CDS View App](calculated-measures.png)


### Add Further Calculated Measures

Press the **Add** button to add further calculated measures like:

-    **Gross Profit**
-    **Gross Revenue**
-     and **Counter**.

![Select Data Source](further-measures.png)


### Edit Calculated Measures

Select the measure **`margin`** and click on **Edit** to open the expression editor in a `popup` window.

![Select Data Source](calculated-measures2.png)


### Choose Measures

You can choose different measures, supported functions and you have content assistance by pressing **`Ctrl`** + **space**.

Select **`"Gross Profit" / "Gross Revenue"`** as an Expression.

![Select Data Source](calculated-measures3.png)


### Result of Measures


Result:
The new expression for the calculated measure is shown in the text field.

![Select Data Source](calculated-measures4.png)


### Define Exception Aggregation

For a calculated measure, you can define exception aggregation. Check **Exception Aggregation** and press the value help to add a function. The **`popup`** shows you the supported functions. One or more dimensions can be added to the exception aggregation via the **+** button.

Select following:

-      `COUNT` as `Function`
-      `Controlling Area` as `Dimension`
-       and `Cost Object` as `a further Dimension`.

![Select Data Source](calculated-measures5.png)


### Display Hierarchy

If a field supports hierarchies, then the hierarchy checkbox is shown in the properties.
**`Checkmark` the hierarchy checkbox** to enable displaying hierarchy for `CostCenter`.
Furthermore select values for `Controlling Area` and `Cost Center Hierarchy`.
![Select Data Source](costcenter.png)


### Save the Draft and Show the Preview

**Click on Save Draft** to save the query and **click** afterwards on **Preview** to test the query.

![Select Data Source](save.png)


### Publish the Query

**Click on publish** to publish the query. Published queries are available to be consumed by Key Performance Indicators and Reports.  

After publishing the status will be updated to published on the top right corner.

![Select Data Source](publish.png)


### Test yourself



