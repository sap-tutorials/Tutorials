---
title: Build a Dashboard in SAP Analytics Cloud
description: Build a HR Employee Dashboard in SAP Analytics Cloud using live data from the SAP HANA Service
auto_validation: true
author_name: Priyanka Brodie
author_profile: https://github.com/pbrodie
time: 30
tags: [ tutorial>beginner, products>sap-cloud-platform,  tutorial>license]
primary_tag: products>sap-analytics-cloud
---

## Prerequisites
 - You have created the [prerequisite calculation views](haas-dm-create-cube-calculation-view)
 - You have deployed the SAP HANA Analytics Adapter [to consume Calculation Views from SAP Analytics Cloud](https://blogs.sap.com/2019/04/24/connecting-the-sap-hana-service-on-cloud-foundry-to-sap-analytics-cloud-the-lazy-approach-pt1/)

## Details
### You will learn
  - How to build a HR dashboard using live data from Calculation Views in SAP Cloud Platform, SAP HANA Service
  - How to create each metric displayed in the dashboard
  - General styling tips and tricks

The HR Employee Dashboard displays insightful metrics and charts corresponding to employee data which can be leveraged by HR for reporting needs and to pinpoint where there is room for improvement in an organization.

![New project from template](1.png)

The left-side panel of the HR Employee Dashboard contains 6 metrics: Headcount, Employees Hired in CY, Total Salary, Average Salary, Average Satisfaction Index and Average Evaluation Rating.

![New project from template](2.png)

You will learn how to create some of these metrics to replicate this dashboard.

---

[ACCORDION-BEGIN [Step 1: ](Indicator: Headcount Metric)]

The `Headcount` metric displays the number of employees in the organization.

![New project from template](3.png)

Click on the **Insert Chart** icon found on the ribbon at the top of the page.

![New project from template](4.png)

The below tile will appear on your canvas. You can drag and drop this tile anywhere on your canvas.

![New project from template](5.png)

There is an field `ID` in the sample data that you are using to build this dashboard. You can count the number of unique records in the data to determine the headcount. In order to do this, you will create a calculation.

In the Builder panel on the right-side of the screen, click on **+ Add Measure**.

![New project from template](6.png)

Choose the **+ Create Calculation** option.

![New project from template](7.png)

In the dropdown menu in the Calculation Editor, choose the **Aggregation** option.

![New project from template](8.png)

In the **Name** field, type in 'Headcount'. Select COUNT DIMENSIONS for the **Operation** field and select ID for the **Aggregation Dimensions** field. Press **OK**.

![New project from template](9.png)

Change the chart structure from a bar/column graph to a numeric point.

![New project from template](10.png)

Click on the three dots of your newly created Headcount measure and select the **Format** option.

![New project from template](11.png)

Change the number of decimal places to 0 and then press **OK**.

![New project from template](12.png)

You can change the styling of this chart by clicking on the three dots that appear when the chart is selected. Click on the **Show/Hide** option to determine whether you want to show or hide the chart details, subtitle, primary value labels, etc.

![New project from template](13.png)

Click on the **Edit Styling** option to change the styling of the chart such as the background color, font size, font color, etc.

![New project from template](14.png)

In this dashboard, all of the metrics in the left panel have a light blue font color for the primary value text (which is the value of the metric, in this case 80,000) and have a white font color for the name of the metric (in this example, the name of the metric is Headcount).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Indicator: Employees Hired in current year)]

The `Employees Hired in CY` metric displays the number of employees the organization has hired in the current year.

![New project from template](15.png)

Click on the **Insert Chart** icon found on the ribbon at the top of the page.

![New project from template](16.png)

The below tile will appear on your canvas. You can drag and drop this tile anywhere on your canvas.

![New project from template](17.png)

In order to display the number of employees hired in the current year, we will need to use the Headcount measure created in the previous step and apply a current year filter to it.

First, click on **+ Add Measure** and select the Headcount measure.

![New project from template](18.png)

Next, click on **+ Add Filters**.

![New project from template](19.png)

Choose the **Start Year (Member)** option.

![New project from template](20.png)

Select the current year in the **Set Filters for Start Year** window. Press **OK**.

![New project from template](21.png)

Change the chart structure from a bar/column graph to a numeric point.

![New project from template](22.png)

Edit the styling of the chart as desired.

The indicators for Total Salary, Average Salary, Average Satisfaction Index and Average Evaluation Rating can be created repeating the above steps.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Donut Chart: Employee Gender Split Chart)]

The `Employee Gender Split` chart displays the number of employees identifying themselves as male versus employees identifying themselves as female in the company.

![New project from template](39.png)

Click on the **Insert Chart** icon found on the ribbon at the top of the page.

![New project from template](40.png)

You will use the `Headcount` calculation that you previously created in this chart.

In the **Builder** panel on the right-side of the screen, click on **+ Add Measure**.

![New project from template](42.png)

Select `Headcount` as the measure.

![New project from template](43.png)

Under the **Color** field, click on **+ Add Dimension/Measure**. Select `Gender`. This will automatically display the number of male and female employees in the company.

![New project from template](44.png)

You can change the color palette of the chart as desired. In this example, blue was chosen to represent males and pink was chosen to represent females. Click on **+ Create New Palette** if you want to customize the colors in the chart.

![New project from template](45.png)

In this example, a custom color palette has been chosen for the `Gender` dimension. If you want to make this color palette consistent in the dashboard when the `Gender` dimension is used in a chart, you can sync the colors for the `Gender` field. This will ensure that every chart that showcases related indicators will have the same color palette.

To do this, click on the three dots next to Gender under the Color field, click on the **Color Sync** option and click on **Sync Colors**.

![New project from template](46.png)

Essentially, the Color Sync option ensures that when the color palette for one chart in a story is changed, all the other charts that have the same color dimension or member will also be updated. This applies to charts that have a single dimension or measure for Color.

Change the chart structure from a bar/column graph to a donut chart.

![New project from template](47.png)

You can double click on the title of the chart to edit it.

![New project from template](48.png)

You can edit various aspects of the chart by clicking on the three dots that appear when the chart is selected and then choosing one of the available options.

![New project from template](49.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Column Chart: Salary per Gender Chart)]

The `Salary per Gender` chart displays the salary distribution per gender.

> Note: Although the salaries have been anonymized for data privacy, the distribution remains similar to the original dataset.

![New project from template](50.png)

Click on the **Insert Chart** icon found on the ribbon at the top of the page.

![New project from template](51.png)

In the Builder panel on the right-side of the screen, click on **+ Add Measure**.

![New project from template](53.png)

Since we are comparing the salary of males versus females, add Salary as the measure.

![New project from template](54.png)

In the Builder panel on the right-side of the screen, click on **+ Add Dimension**.

![New project from template](55.png)

Add Gender as the dimension.

![New project from template](56.png)

In order to style the chart to display one color for males and another color for females, click on **+ Add Dimension/Measure** under the Color field. Select Gender.

![New project from template](57.png)

Change the chart orientation from horizontal to vertical.

![New project from template](58.png)

You can edit various aspects of the chart by clicking on the three dots that appear when the chart is selected and then choosing one of the available options.

![New project from template](59.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Stacked Bar Chart: Gender Breakdown by Region Chart)]

The `Gender Breakdown by Region` chart displays the total headcount by gender and region.

![New project from template](60.png)

Click on the **Insert Chart** icon found on the ribbon at the top of the page.

![New project from template](61.png)

Change the chart structure to **Stacked Bar/Column**.

![New project from template](63.png)

Since we want to display the gender breakdown by region, put Region as a dimension.

![New project from template](64.png)

We will need to create two measures for this chart; one measure to count the number of females in the company and the other measure to count the number of males.

In the Builder panel on the right-side of the screen, click on **+ Add Measure**.

![New project from template](65.png)

Click on **+ Create Calculation**.

![New project from template](66.png)

The following calculation counts the number of female employees. Configure the fields as follows:

|  Field Name     | Value
|  :------------- | :-------------
|  Name           | `f`
|  Operation           | `SUM`
|  Measure   | `Headcount`
|  Aggregation Dimension            | `Gender`
|  Use conditional aggregation            | Checked
|  Dimension            | `Gender`
|  Values for Input Controls            | `f`


 Choose **Have Measure values for Conditions** option is chosen for the **Aggregate when aggregation dimensions** field.

 ![New project from template](67.png)

Press **OK**.

Create the same calculation as above, but for male employees.

![New project from template](68.png)

Select the newly created f measure and click on the three dots that appear. Click on **Format**.

![New project from template](69.png)

Change the number of decimal places to 0. Press **OK**.

![New project from template](70.png)

Execute the same steps for the newly created m measure to ensure that it has 0 decimal places.

Change the color palette of this chart (specifically the colors that represent males and females) to correspond with the color palette of the other charts in the dashboard.  

![New project from template](71.png)

You can change the title of the chart by double clicking on the title. You can also format the chart by clicking on the three dots and choosing the **Edit Styling** option.

![New project from template](72.png)

You can repeat similar steps to create a chart for the indicator `Salary per Region`.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Bar Chart: Evaluation Rating per Region Chart)]

The `Evaluation Rating per Region` chart displays the average evaluation rating of each region.

![New project from template](83.png)

Click on the **Insert Chart** icon found on the ribbon at the top of the page.

![New project from template](84.png)

Make the chart structure a numeric point.

![New project from template](86.png)

Select Evaluation Rating as the measure.

![New project from template](87.png)

Select the chart and click on the three dots. Click on the **Add Trellis** option.

![New project from template](88.png)

Click on **+ Add Dimension** under the Trellis field.

![New project from template](89.png)

Select the Region dimension.

![New project from template](90.png)

Edit the styling of the chart. In this example, the Chart Details and Primary Value Labels are hidden. The color of the evaluation ratings in the chart have also been edited.

![New project from template](91.png)

You can repeat these steps to display the `Satisfaction Index per Region`.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Bar Chart: Salary per Start Year Chart)]

The `Salary per Start Year` chart displays the average salary of employees each year.

![New project from template](93.png)

Click on the Insert Chart icon found on the ribbon at the top of the page.

![New project from template](94.png)

Select Salary as the measure.

![New project from template](96.png)

Select Start Year as the dimension.

![New project from template](97.png)

Change the size and styling of the chart. In this example, the size of the chart was increased and the color of the bars was modified.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Scatter Plot: Salary versus Satisfaction Index Chart)]

The `Salary versus Satisfaction Index` chart is a scatter plot that displays the salary of employees on one axis and the corresponding satisfaction index on the other axis.

![New project from template](98.png)

Click on the Insert Chart icon found on the ribbon at the top of the page.

![New project from template](99.png)

Change the chart structure to a scatter plot.

![New project from template](101.png)

Choose the Satisfaction Index measure for the X-Axis.

![New project from template](102.png)

Select the Salary measure for the Y-Axis.

![New project from template](103.png)

Choose ID for the dimension.

![New project from template](104.png)

At this current state, the chart only shows partial results because there are too many data points. To resolve this issue, add a filter for the Start Year and narrow the results down to the current year. Under Filters, click on **+ Add Filters**.

![New project from template](105.png)

 Click on the **Start Year (Member)** option.

![New project from template](106.png)

Select the current year and press **OK**.

![New project from template](107.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Input Controls: Start Year)]

The `Start Year Input Control` allows users to filter the entire dashboard by specific start years.

![New project from template](109.png)

Click on the **Insert Input Control** icon found on the ribbon at the top of the page.

![New project from template](110.png)

Click on **Dimensions**.

![New project from template](111.png)

Click on the Start Year dimension and then select **Filter by Member**.

![New project from template](112.png)

Select the **All Members** checkbox. Press **OK**.

![New project from template](113.png)

When the dashboard is first viewed, the metrics and charts will encompass data for all the start years. Users can then choose to filter by specific start years if desired.  

You can repeat the same steps to create an input control for the dimension `T-LEVEL`.

[VALIDATE_1]
[ACCORDION-END]

---
