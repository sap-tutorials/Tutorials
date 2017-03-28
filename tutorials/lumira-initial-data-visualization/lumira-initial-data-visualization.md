---
title: Basics of data visualization in SAP BusinessObjects Lumira
description: Basics of visual data discovery
primary_tag: products>sap-lumira
tags: [  tutorial>beginner, products>sap-lumira ]
---
## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Initial data exploration in SAP BusinessObjects Lumira](http://www.sap.com/developer/tutorials/lumira-initial-data-exploration.html)

## Next Steps
- Time dimension and data drilling (coming soon)

## Details
### You will learn  
The basics of data visualization: creating charts suggested by the system or manually, doing time-series analysis and predictions

### Time to Complete
**10 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Switch to Visualize room)] ￼

Switch to **Visualize** room.

By default it displays **Chart Builder** in **visualization tools**, and **Vertical Orientation** of **Measures and Dimensions** pane.

![Layout of Visualize room in SAP BusinessObjects Lumira](Lum03-01a.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](View column chart)] ￼

For now you will focus on manual data discovery using visualizations.

Start with simple **Column Chart**, which you should select from **Chart Picker**. Next, drag `Sales Revenue` from Measures and drop it on **Chart Canvas**. Then drag `Lines` from Dimensions to the Chart Canvas as well.

> You could as well drop these attributes to **Shelves**.

You should get a column chart like this.

![Simple column chart in SAP BusinessObjects Lumira](Lum03-02.png)

A human eye can immediately spot that the best selling line of products was `Sweat-T-Shirts`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Sort products)] ￼

Let's sort lines of products by sales revenue on the chart.

For that just move the mouse over `Sales Revenue` in the Shelves, then click on **Settings** icon that appears, and choose **Sort Descending**.

![Sorting on the chart in SAP BusinessObjects lumira](Lum03-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Choose colors)] ￼

You can change predefined palette of colors used in a chart by clicking on **Settings** icon in the canvas, and selecting **Choose Colors...**.

![Choose colors for charts in SAP BusinessObjects Lumira](Lum03-04.png)

SAP BusinessObjects Lumira offers a long list of predefined palettes, which can be modified or new palettes can be defined by you.

![List of palettes in SAP BusinessObjects Lumira](Lum03-05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create new visualization)] ￼

Click on the **+** in the visualization gallery to create a new visualization. The previous one - the column chart - is remembered in the gallery.

![A new visualization is created in SAP BusinessObjects Lumira](Lum03-06.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add a Customer Satisfaction measure)] ￼

For this newly created visualization choose **Numeric Point** chart and add `Customer Satisfaction` as a measure.

![Numeric Point chart in SAP BusinessObjects Lumira](Lum03-07.png)

> Numeric Point chart is often used at the later stage in data stories, as you will see in corresponding tutorials.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Display average score)] ￼

Summarized customer satisfaction scores do not make sense. You need to display an average score.

To do that click on **Options** icon next to `Customer Satisfaction` measure and from **Change Aggregation** choose **Average**.

You can see now that the average Customer Satisfaction score across all countries, products and years is 0.88.

![Average Customer Satisfaction displayed in SAP BusinessObjects Lumira](Lum03-08.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Change to percentage format)] ￼

Now you should change the way Customer Satisfaction displayed to the percentage format.

To do that click on **Options** icon next to `Customer Satisfaction` measure and choose **Display Formatting...**

Now change value format to **Percentage** and **Number of decimals** to `1`. Accept changes.

![Display options for measures in SAP BusinessObjects Lumira](Lum03-09.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Save)] ￼

Save the document.

[DONE]
[ACCORDION-END]



## Next Steps
- Time dimension and data drilling (coming soon)
