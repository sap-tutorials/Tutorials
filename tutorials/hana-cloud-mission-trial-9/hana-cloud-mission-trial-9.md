---
parser: v2
author_name: Christopher Kollhed
author_profile: https://github.com/chriskollhed
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-hana-database, software-product>sap-business-application-studio]
primary_tag: software-product>sap-hana-cloud
---

# Create a Calculation View
<!-- description --> Learn how to create your own calculation views in SAP HANA Cloud, SAP HANA database with SAP Business Application Studio using Join and Rank nodes. 

## Prerequisites
- You have access to [SAP HANA Cloud trial](hana-cloud-mission-trial-2) or [SAP HANA Cloud free tier](hana-cloud-mission-trial-2-ft), or a production environment of SAP HANA Cloud, SAP HANA database
- You have completed the tutorial to [provision an instance of SAP HANA Cloud, SAP HANA database](hana-cloud-mission-trial-3)
- You have completed the tutorial to [import the sample data needed for this mission](hana-cloud-mission-trial-5)
- You have [set up a development project in SAP Business Application Studio and connected it to your database](hana-cloud-mission-trial-8)


## You will learn
- How to create a calculation view in SAP Business Application Studio
- How to use join nodes
- How to use rank nodes
- How to preview the output



## Intro
>
> ![Alex Banner](banner-alex.png)
>
> Reminder: This tutorial is part of a mission, in which you will help Alex, the CEO of Best Run Travel, to answer a concrete business question with SAP HANA Cloud, SAP HANA database.
>
> *Alex needs to know the top 5 partners of their agency and wants to find out the days with maximum booking of each partner.*

In this tutorial, you will learn how to create a calculation in SAP Business Application Studio, in which you will join tables and rank results to get Alex the business insights they need. 

---

### Create the calculation view


1.	Within your project in the SAP Business Application Studio, click on **View** from the side menu. Then click on **Command Palette**. Alternatively, use `Ctrl+Shift+P` to access it.

2.	Type **SAP HANA: Create HANA database artifact** and press `Enter` or click on the right option.

3. Under **Path**, change the path so the calculation view is created in the `src` folder of your project.	

4. You will see a form appear on the right-side of the screen. Select **Calculation View** as your artifact type.

5.	Type a name for your calculation view, such as `calculationView`.

6.	Leave the rest of the options as they are. Finally, click on **Create**.

    ![Create calculation view](create-calc-view.png)



### Create a join node


1.	The calculation view will open automatically upon creation.

2. In this example, start with a join node to join two tables. Click on the join icon on the sidebar of the editor and then click anywhere on the canvas.

    ![Join Node](ss-01-join-node.png)

3.	The join node appears. Next to the node, click on the plus icon to add the tables.

    ![Join Plus icon](ss-02-join-plus-icon.png)

4.	On the pop-up, start by selecting the user-provided service on the **Services** drop-down list.

    ![Select user-provided service](add-user-provided-service.png)

5. Search for the `SAGENCYDATA` table, which we created in a previous tutorial.

    > If you want to see all objects available via the connection service, enter `**` in the search field.


6.	We can find the top 5 partners for Best Run Travel by joining the `SAGENCY` table with the `STRAVELAG` table. Add the `STRAVELAG` table to the join node.

7.	Once both objects are selected, click on **Create Synonym**.

    ![Create synonym](create-synonym.png)

8.	Click on **Finish** without selecting any other options.

9.	In your file explorer, a new file will appear ending with `.hdbsynonym`. In this file, your synonyms are defined and stored. Go back to the calculation view editor and you should see the two tables in the join node.

    ![Join Node tables added](ss-03-join-node-tables-added.png)





### Define the mapping of the join node


1.	To properly join the two tables, you need to define how they relate to each other. This is done by editing the join node.

2.	Double click the join node to open the settings.

3.	Under **Join Definition**, click on the column `AGENCYNUM` from one of the tables and drag and drop it on top of the same column from the second table. This determines the key column.

    ![Key mapping](ss-04-key-mapping.gif)

4.	Click on the **Mapping** tab. Here you can select which columns will be part of the output. Select the columns `AGENCYNUM`, `NUMBOOKING`, and `NAME` by double clicking on them. You can see they are added to the output section on the right.

    ![Mapping](ss-05-mapping.png)

5.	Close the join settings by clicking on the `X` icon at the top right corner.

6.	Now **connect** the join node to the aggregation node above it. Just click on the arrow icon of the join node and drag and drop it on the aggregation node.

    ![Connect Join to Aggregation](ss-06-connect-join-to-aggregation.gif)




### Add a rank node


1.	Since we want to see the top 5 results from this join, we will add a **Rank** node next. Click on the rank icon (![Rank](icon-rank.png)) then click **on the link** between Join node and Aggregation node. This will add a Rank node in between them.

    >To make it easier to view the nodes, you can click on the **Auto Layout** icon (![Auto Layout](icon-auto-layout.png)) to rearrange the canvas.


2.	Next, double click the Rank node to open the settings.

3.	Under **Mapping**, make sure all 3 columns are included in the output.

    ![Mapping output](output-column.png)

4.	Click on **Definition**. Choose the **Aggregation Function** as `Rank`.

5.	Set the **Result Set Direction** as `Top`. This will order the results descending from highest to lowest.

6.	Set the **Result Set Type** as `Absolute`. This setting determines the unit of values given out by the rank. You could, for example, also select `Percentage` here to get the top 10% of results.

7.	On the **Target Value**, type `5`. This will determine the number of values given out as a result.

8.	The **Offset** should be `0`. Offset determines a number of values that are skipped in the result, for example, with an `Offset = 1` the first value of the rank result would not be reported.

9.	Then click on **Sort Column** to expand this area.

10.	Click on the plus icon to add a **Sort Setting**. Select the column `NUMBOOKINGS` and the direction as **Descending**.

    ![Edit Rank node](ss-08-edit-rank-node.png)

11.	Now close the Rank node panel and double click on the **Aggregation** node.

12.	Under **Mapping**, make sure all columns are selected as part of the output. If a column is not mapped to the output, double click it to add it.

    ![Mapping Aggregation](ss-09-mapping-aggregation.png)




### Deploy the calculation view


1.	Now deploy the calculation view. In the SAP HANA Project panel next to the calculation view name or on the top right corner of the screen, click on the deploy icon (![Deploy](icon-deploy.png)). This will deploy the calculation view. Once this is successfully completed, it's time to check the output so far.

2.	To access the data preview, click on the HDI container icon (![Container](icon-container.png)) next to the name of the project. This will open a new tab with the SAP HANA database explorer.

3.	On the list of databases, you will now see the HDI container that represents your calculation view. Expand the catalog of that HDI container, then click on **Column Views**.

4.	Next, click on the name of your calculation view on the panel below the catalog and click on **Open Data**.

    ![DBX CV Preview](open-col-view.png)

5.	Then, click on **Raw Data** to see the output of this calculation view so far.

6.	This shows you the top 5 partners of Best Run Travel.

    ![DBX Result preview](raw-data.png)

> You can also preview the results of your calculation view directly in the calculation view editor in SAP Business Application Studio. Right-click on the aggregation node and select **Data Preview**. This will open the data preview inside the calculation view editor.
>
> ![Data Preview in BAS](ss-12-data-preview-in-BAS.png)




### Add a third table to the view


Now that we know the top 5 partners, we need to next find out on which days the top 5 travel agencies have the most bookings. To achieve this, we will add the table `SAGBOOKDAYS` to our view.

1.	Continue working on the same calculation view.

2.	We will join the output of our rank node to the table `SAGBOOKDAYS`, which we previously created. Add a join node **between** the rank node and the aggregation node.

    >Remember, you can use the **Auto Layout** icon (![Auto Layout](icon-auto-layout.png)) to keep the canvas tidy.

3.	Since the Join node is connected to **Rank 1**, its output is already added to the join node. So, you only need to add the `SAGBOOKDAYS` table by clicking on the plus icon. Follow the steps you previously took to add a table and create a synonym.

    ![Create synonym for SAGBOOKDAYS table](create-synonym-2.png)

4.	After the table is there, double click on the second join node.

5.	Under **Join Definition**, connect the column `AGENCYNUM` from **Rank 1** to the `AGENCYNUM` column from the `SAGBOOKDAYS` table.

    ![Join 2 Definition](ss-14-join-2-definition.png)

6.	On the same panel, under **Mapping**, make sure the following columns are selected for the output: `AGENCYNUM`, `NUMBOOKING`, `NAME`, `ORDERDAY` and `DAYCOUNT`.

    ![Join 2 Mapping](ss-15-join-2-mapping.png)




### Add another rank node


1.	To find the days with the most bookings, add another rank node **between** Join 2 and the Aggregation node. Click on the rank icon (![Rank](icon-rank.png)) and then on the connection between the Join 2 and the Aggregation nodes. 
Remember, you can use the **Auto Layout** icon (![Auto Layout](icon-auto-layout.png)) to keep the canvas tidy.


2.	Double click the rank node to open it.

3.	Under Mapping, make sure all 5 columns appear in the Output Columns. If not, double click to add them.

    ![Rank 2 Mapping](ss-17-rank-2-mapping.png)

4.	Then, click on **Definition**. Adjust the settings similar to STEP 4:

      *	**Aggregation Function**: `Rank`
      *	**Result Set Direction**: `Top`
      *	**Result Set Type**: `Absolute`
      *	**Target Value**: `1` (this is different from STEP 4)
      *	**Offset**: `0`

    ![Rank 2 Definition](ss-18-rank-2-definition.png)

5.	Now click on the Partition Column area, and then click on the plus icon.

    > **What does a partition column do?**
    >
    > Defining a partition column will group the rows of the output based on a specific column.
    >
    >![Rank 2 Partition](ss-19-rank-2-partition.png)

6.	Add the column `AGENCYNUM` to group the rows based on this column.

    ![partition column](partition-col.png)

7.	Click on **Sort Column** and click on the **plus** icon. Add the column `DAYCOUNT` and select the sort direction as **Descending**. You can now close the rank settings.

    ![Rank 2 Sort column](sort-col.png)

8.	Double click the Aggregation node. Under **Mapping**, make sure all the columns under the Rank are selected for the output. To add a column to the output, simply double click it.

    ![Aggegration Node Mapping tab](aggregation-node-output.png)




### Deploy the view and access the output


You are almost done!

1.	On the SAP HANA Project panel, click on the deploy icon(![Deploy](icon-deploy.png)) next to the calculation view name. This will deploy the calculation view. Once this is successfully completed, it's time to check the output again.


2.	To access the Data Preview in the SAP HANA database explorer, click on the HDI container icon(![Container](icon-container.png)) next to the name of the project or access it directly.

    ![Open HDI container](open-hdi.png)

3.	On the list of databases, you will now see the HDI container that represents your project. Expand the catalog of that HDI container, then click on **Column Views** to find your calculation view.

4.	Next, click on the name of your calculation view on the panel below the catalog. Then click on **Open Data**.

    ![Open data](open-data.png)

5.	From here, you can click on **Raw Data** to see the output of this calculation view. This shows you the top 5 partners of Best Run Travel and the day in which they have the most bookings.

    ![final results](ss-21-final-results.png)

Well done!

You have completed the eighth tutorial of this mission! You learned how to create a calculation view in SAP Business Application Studio using the graphical calculation view editor. You used join and rank nodes to get Alex the business insights they were looking for. Now, all that's left to do is make this calculation view available to others in Alex organization. Learn in the last tutorial how to do that!



### Knowledge Check






---
