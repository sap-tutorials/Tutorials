--- 
parser: v2 
auto_validation: true 
time: 10 
tags: [ tutorial>beginner, software-product-function>sap-hana-cloud--sap-hana-database, software-product-function>sap-hana-cloud--data-lake] 
primary_tag: software-product>sap-hana-cloud 
--- 

# Browse and Explore Catalog Objects with the Database Objects App

<!-- description --> Dive into using the database objects app to explore and inspect schema objects in an SAP HANA Cloud, SAP HANA database or data lake Relational Engine.  

## Prerequisites

- An SAP HANA Cloud database such as SAP HANA Cloud free tier
- You have completed the first 3 tutorials in this group

## You will learn

- How to filter for specific tables and schemas within an instance
- How to inspect and explore objects in an SAP HANA Cloud database
- How to generate SQL Statements

## Introduction

The database objects app is a built-in application in SAP HANA Cloud Central that enables you to search, view metadata, and generate SQL for catalog objects.

---

### Filters and navigation  

1. Ensure that your database instance is running before attempting to open the Database Objects app. Once it is started, you can access the app directly by selecting its icon from the left‑hand navigation panel on the Instances page.

    ![DBObj Navigation](DbObjNav.png)

    You can also open the database objects app through the SQL Console. Make sure you are connected to the correct database, then click the three‑dot menu in the top‑right corner and select Open Database Objects.

    ![DBObj Navigation SQL Console](NavSQLConsole.png)

2. Upon opening database objects, click “Select an Instance” at the top of the page to choose the database you want to work with. You can browse the list or use the search field to quickly find your instance.

    ![Instances](Instances.png)

    ![Select Database](SelectDatabase.png)

    Once selected, the **Schema** and **Search** filter are both available to use. Select the **Schema** filter and search for the HOTELS schema.  

    ![Hotels Schema](SelectSchema.png)

    You can also search for the schema or any other objects directly in the **Search** filter. In this case after searching navigate to the **Schemas** tab directly to view the metadata for the HOTELS schema. This data includes ownership, privileges and create time.  

    ![Schema Data](SchemaData.png)

### Explore tables

Information for tables includes columns, indexes, properties, runtime information and SQL CREATE Statements.  

1. Select the **Tables** tab to view all associated tables of the HOTELS schema.  

    The page now displays all tables in the schema HOTELS and their table type.

    ![Tables View](TableView.png)

2. Select the **RESERVATION** table to explore it further.  

    Click the full screen icon on the top right of the screen to maximize the page and view all tabs.

    ![Table Data](TableData.png)

    By default you should see the column details for the table.  

    ![Column Data](ColumnData.png)

3. Explore the **Runtime Information** tab, where further information about the table can be found. This information includes the total number of rows, disk size, partitions and memory consumption for the table, as well as individual columns.

    ![Runtime Information](RuntimeInformation.png)

4. Examine the other tabs, such as **CREATE Statements**, where SQL code to generate the table can be found.  

    ![Create Statement](CreateStatement.png)

5. Select the Generate SQL Statement dropdown to see the three ways to have SQL generated for the table.  

    ![SQL Generation](GenerateSQL.png)

### Explore functions and procedures

1. To display functions in the Database Objects app, click on **Select Object Types** and turn on the functions object type. The visibility and order of the database objects can be customized by selecting the checkbox and dragging objects by the drag handle icon.

    ![Functions Database Objects](SettingsFunc.png)


2. Open the **Functions** tab and select AVERAGE_PRICE to examine it further.  

    ![Average Price Function](AvgPrice.png)

    Select the Generate SQL Statement dropdown and click SELECT Statement to navigate to the SQL Console.

    ![Average Price Function Generate Statement](GenerateFuncStatement.png)

    Input *'suite'* in the single quotes of the SELECT statement to get the average price for suites.

    ![Function Call](FuncCall.png)

3. Navigate back to the Database Objects app and open the **Procedure** tab. Select RESERVATION_GENERATOR to examine it further.  

    ![Procedure Data](ProcedureData.png)

4. Click Generate SQL and select the CALL statement to get SQL that runs the stored procedure. 

    ![Run Procedure](Procedure.png)

    The following SQL will be generated:

    ```SQL
    CALL "HOTELS"."RESERVATION_GENERATOR"(NUMTOGENERATE => /*<INTEGER>*/)
    ```

    The parameter NUMTOGENERATE expects an integer value, which specifies how many reservations to generate. Replace /*<INTEGER>*/ with the desired number of reservations you want the procedure to create.

### Additional features

1. Select the **Recent** tab to view all the recent objects you opened.  

    ![Recent](Recent.png)

2. Navigate to an object and click the star icon on the top right of the screen to favorite it. 

    ![Favorite Icon](FavIcon.png)

    Once selected as a favorite, navigate to the **Favorites** tab to see it.

    ![Favorite](Fav.png)

3. Click All/Selected Instance toggle to filter favorites.

    ![Filter Favorites](FilterFav.png)

4. You can also navigate to HANA Cloud Central user settings to customize preferences for the Database Objects App.

    ![settings](SettingsFunc.png)

5. The [SAP HANA database explorer extension for Microsoft Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=SAPSE.hana-database-explorer) and the SAP Business Application Studio provide a few additional features that developers may find helpful which include the ability to [view and download SAP HANA Cloud trace files](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-developer-guide-for-cloud-foundry-multitarget-applications-sap-business-app-studio/work-with-trace-files) and the ability to [debug SQL Script](https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-developer-guide-for-cloud-foundry-multitarget-applications-sap-business-app-studio/debugging-sqlscript-in-database-explorer).

One additional feature that the extension provides is the ability to maintain a list connections to instances.  In addition the extension for Visual Studio Code provides an option to maintain these as [local connections](https://marketplace.visualstudio.com/items?itemName=SAPSE.hana-database-explorer).  

### Knowledge check

Congratulations! You have now successfully navigated the Database Objects app and learned about the various features and tools available to you right from SAP HANA Cloud Central.
