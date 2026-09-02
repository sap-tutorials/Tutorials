--- 
parser: v2 
auto_validation: true 
author_name: Dan van Leeuwen
author_profile: https://github.com/danielva
time: 10 
tags: [ tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud--sap-hana-database, software-product-function>sap-hana-cloud--data-lake] 
primary_tag: software-product>sap-hana-cloud 
--- 

# Browse and Explore Catalog Objects with the Database Objects App

<!-- description --> Dive into using the Database Objects app to explore and inspect schema objects in an SAP HANA Cloud, SAP HANA database or data lake Relational Engine.

## Prerequisites

- You have completed the first 3 tutorials in this group

## You will learn

- How to filter for specific tables and schemas within an instance
- How to inspect and explore objects in an SAP HANA Cloud database
- How to generate SQL Statements

## Introduction

The database objects app is a built-in application in SAP HANA Cloud Central that enables you to search, view metadata, and generate SQL for catalog objects.  Further details can be found in the documentation at [Using Database Objects in SAP HANA Cloud Central](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/database-objects).

---

### Navigation and Filters

1. Ensure that your database instance is running before attempting to open the Database Objects app.

    The application can be opened from the actions menu of an instance or directly from the list of applications on the left side bar of SAP HANA Cloud Central.

    ![Navigation from the instances list](DbObjNav.png)

    You can also open the database objects app through the SQL Console as shown below.

    ![Navigation from the SQL console](NavSQLConsole.png)

2. Upon opening database objects, click **Select an Instance** at the top of the page to choose the database you want to work with. You can browse the list or use the search field to quickly find your instance.

    ![Instances](Instances.png)

    ![Select database](SelectDatabase.png)

    Once selected, the **Search** and **Schema** filters are both available to use. Select the **Schema** filter and search for the HOTELS schema.  

    ![Hotels schema](SelectSchema.png)

    You can also search for the schema or any other objects directly in the **Search** filter. In this case after searching navigate to the **Schemas** tab directly to view the metadata for the HOTELS schema. This data includes ownership, privileges and create time.  

    ![Schema data](SchemaData.png)

### Explore tables

Information for tables includes columns, indexes, properties, runtime information and SQL CREATE Statements.  

1. Select the **Tables** tab to view all associated tables of the HOTELS schema.  

    The page now displays all tables in the schema HOTELS and their table type.

    ![Tables view](TableView.png)

2. Select the **RESERVATION** table to explore it further.  

    Click the full screen icon on the top right of the screen to maximize the page and view all tabs.

    ![Table data](TableData.png)

    By default you should see the column details for the table.  

    ![Column data](ColumnData.png)

3. Explore the **Runtime Information** tab, where further information about the table can be found. This information includes the total number of rows, disk size, partitions and memory consumption for the table, as well as individual columns.

    ![Runtime information](RuntimeInformation.png)

4. Examine the other tabs, such as **CREATE Statements**, where SQL code to generate the table can be found.  

    ![Create statement](CreateStatement.png)

5. Select the **Generate SQL Statement** dropdown to see the three ways to have SQL generated for the table.  

    ![SQL generation](GenerateSQL.png)

### Explore functions and procedures

1. To display functions in the Database Objects app, click on **Select Object Types** and turn on the functions object type. The visibility and order of the database objects can be customized by selecting the checkbox and dragging objects by the drag handle icon.

    ![Functions in the Database Objects app](SettingsFunc.png)

2. Open the **Functions** tab and select **AVERAGE_PRICE** to examine it further.  

    ![Average price function](AvgPrice.png)

    Select the **Generate SQL Statement** dropdown and click SELECT Statement to navigate to the SQL Console.

    ![Average price function generate statement](GenerateFuncStatement.png)

    Input *suite* in the single quotes of the SELECT statement to get the average price for suites.

    ![Function call](FuncCall.png)

3. Navigate back to the Database Objects app and open the **Procedure** tab. Select **RESERVATION_GENERATOR** to examine it further.  

    ![Procedure data](ProcedureData.png)

4. Click **Generate SQL** and select **CALL Statement** to get SQL that runs the stored procedure.

    ![Run procedure](Procedure.png)

    The following SQL will be generated:

   ```SQL
   CALL "HOTELS"."RESERVATION_GENERATOR"(NUMTOGENERATE => /*<INTEGER>*/)
   ```

    The parameter NUMTOGENERATE expects an integer value, which specifies how many reservations to generate. Replace *\<INTEGER\>* with the desired number of reservations you want the procedure to create.

### Additional features

1. Select the **Recent** tab to view all the recent objects you opened.  

    ![Recent](Recent.png)

2. Navigate to an object and click the star icon on the top right of the screen to favorite it. 

    ![Favorite icon](FavIcon.png)

    Once selected as a favorite, navigate to the **Favorites** tab to see it.

    ![Favorite](Fav.png)

3. Click All/Selected Instance toggle to filter between favorites for the current instance or to show all favorites.

    ![Filter favorites](FilterFav.png)

4. The [SAP HANA database explorer extension for Microsoft Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=SAPSE.hana-database-explorer) and the SAP Business Application Studio provide a few additional features that developers may find helpful which include the ability to [view and download SAP HANA Cloud trace files](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-developer-guide-for-cloud-foundry-multitarget-applications-sap-business-app-studio/work-with-trace-files), the ability to [debug SQL Script](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-developer-guide-for-cloud-foundry-multitarget-applications-sap-business-app-studio/debugging-sqlscript-in-database-explorer), and the ability to maintain a custom list of  instances.  Further details can be found at [Use the SAP HANA Database Explorer Extension in Visual Studio Code](hana-cloud-sqltools-dbx-extension).

### Knowledge check

Congratulations! You have now successfully navigated the Database Objects app and learned about the various features and tools available to you right from SAP HANA Cloud Central.
