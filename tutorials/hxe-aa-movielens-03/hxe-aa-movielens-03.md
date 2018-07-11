---
title: Import the MovieLens dataset
description: Learn how to easily import flat dataset files in your SAP HANA, express edition instance using the Design-Time objects and Core Data Services (CDS) features
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
---

## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn

- How to create CDS artifacts to expose your dataset as CDS entities.
- How to load your dataset content in the CDS entities.

### Time to Complete
**20 Min**

[ACCORDION-BEGIN [Info: ](Which MovieLens dataset?)]

In order to build your movie recommendation engine, you will be using one of the `MovieLens` dataset.

These datasets are made available by the <a href="https://grouplens.org" target="new">```GroupLens``` Research &copy;</a> group.

They have collected and made available movie rating data sets from the <a href="http://movielens.org" target="new">```MovieLens```</a> web site which were collected over various periods of time.

The data set that you will be using for this series is the ***small*** version of the **`MovieLens` Latest Datasets** downloadable <a href="http://grouplens.org/datasets/movielens/latest/" target="new">here</a>.

This dataset, thanks to its size, can quickly be uploaded in your SAP HANA, express edition instance.

If you have additional capacity resources, you can run this tutorial series with the larger datasets, but the validations steps implemented were built based on the ***small dataset***.

Before using these data sets, please review the <a href="http://files.grouplens.org/datasets/movielens/ml-latest-small-README.html" target="new">README</a> file for the usage licenses and other details.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](SAP HANA data import options)]

There are multiple ways to import data set files inside of your SAP HANA, express edition instance.

- ***Eclipse IDE***

The ***SAP HANA Tools*** plugin for Eclipse provides an ***Import/Export*** feature which would allow you to create the appropriate physical tables first and then import the data.

However, this would require the ***Eclipse IDE*** to be locally installed and properly configured with the ***SAP HANA Tools*** plugin.

Then, you would need to know the complete data file format description in order to create the tables with the proper columns structure. And, last but not least, any changes would require to recreate the all structure and reload the data.

If you want to learn more about this import method, you can check the following tutorial: [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html)

- ***SAP HANA HDB Client***

The **SAP HANA HDB Client** provides an ***IMPORT FROM*** statement allowing you to import CSV files physically located on your SAP HANA, express edition host using a SQL command.

However, this method requires that the table are created before the execution of the command.

If you want to learn more about this import method, you can check the following tutorial: [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-sql-import.html)

- ***SAP HANA Persistence Model***

The **SAP HANA extended application services, advanced model**, (XS advanced) provide a comprehensive platform for the development and execution of native data-intensive applications.

The application data model comprises all the database artifacts used to store and provision data for your application's back end and user interface.

As part of the process of defining the database persistence model for your application, you can create database design-time artifacts such as tables and views, for example using Core Data Services (CDS).

At the same time, you can also create procedures and functions using SQLScript, which can be used to insert data into (and remove data from) tables or views.

#### ***Solution***

As the purpose of this tutorial series is to build an end to end solution including a native SAP HANA application, you will be using the ***SAP HANA Persistence Model*** with the ***Core Data Service*** (CDS) artifacts in a ***SAP HANA Database Module***.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a SAP HANA Database Module)]

Open the Web IDE, and login using the **`XSA_DEV`** credentials.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

![Web IDE](01-01.png)

As a reminder the default URL for the Web IDE is:

 - `https://hxehost:53075`

A link to the Web IDE can also be found on the ***XSA Controller page*** at:

- `https://hxehost:39030`

In the left panel, right click on the **`movielens`** project folder, then select **New > SAP HANA Database Module**.

![Web IDE](01-02.png)

Set the name to **`db`** and click on **Next**.

![Web IDE](01-03.png)

Set the following details on the next screen:

| Name                        | Value               |
|:----------------------------|--------------------:|
| Name Space                  | `aa.movielens.db`   |
| Schema Name                 | `ml`                |
| SAP HANA Database Version   | `2.0 SPS 03`        |
| Build module after creation | `checked`           |

Click on **Finish**.

![Web IDE](01-04.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create the folder structure)]

Expand the **`movielens/db/src`** folder.

Create the following directory structure:

```
|-- movielens/db
    |-- data
    |-- hdb
        |-- apl
            |-- afllang
            |-- procedures
            |-- views
        |-- pal
            |-- afllang
            |-- procedures
            |-- views        
        |-- summary
```

You can use the right click on the target folder and select **New > Folder**.

Enter the folder name, then click on **OK**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import the dataset CSV files)]

As the dataset files get updated periodically, and in order to ensure consistency of content and validation, a copy of the data is available under the <a href="https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/hxe-aa-movielens-03/data" target="new">data</a> directory within the SAP Tutorial GitHub repository.

Download the following files locally (right click on the link, then use the ***Save link as*** option):

- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/links.csv" target="new">links</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/movies.csv" target="new">movies</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/ratings.csv" target="new">ratings</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/tags.csv" target="new">tags</a>

Again, before using these files, make sure you have reviewed the dataset <a href="http://files.grouplens.org/datasets/movielens/ml-latest-small-README.html" target="new">README</a> file for the usage licenses and other details.

In the left side panel, expand the **`movielens/db/src/hdb/data`** tree node.

Right click on the **`data`** folder, and use the **Import** > **File or Project** menu item.

Select one of the previously downloaded files.

![Web IDE](02-03.png)

Click on **OK**.

Repeat the operation for all the previously downloaded files:

- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/links.csv" target="new">links</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/movies.csv" target="new">movies</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/ratings.csv" target="new">ratings</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-03/data/tags.csv" target="new">tags</a>

Your package structure should now look like this:

![Web IDE](02-04.png)

> ### **Note**:
>You should close the tabs that got opened for each of the imported files in order to release the resources used by your browser.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the CDS Entity Artifacts)]

> #### **CDS Entities Artifacts**
>
>In the SAP HANA database, a CDS entity is a table with a set of data elements that are organized using columns and rows.
>&nbsp;
>A CDS entity has a specified number of columns, defined at the time of entity creation, but can have any number of rows.
>&nbsp;
>Database entities also typically have meta-data associated with them; the meta-data might include constraints on the entity or on the values within particular columns.
>&nbsp;
>SAP HANA Extended Application Services (SAP HANA XS) enables you to create a database entity as a design-time file. All design-time files, including your CDS entity definition, can be transported to other SAP HANA systems and, when deployed, used to generate the same catalog objects. You can define the entity using CDS compliant DDL.
>&nbsp;
>They are stored in a CDS documents which are design-time source files that contain DDL code according to rules defined in Core Data Services.
>&nbsp;
>The SAP HANA Core Data Services (CDS) plug-in transforms a design-time SAP HANA CDS resource (defined in a `.hdbcds` artifact) into the database objects defined in the CDS document, for example, tables, views, types, etc.
>&nbsp;
>For additional details, check the [CDS Entities in XS Advanced](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/2.0.03/en-US/df46a790f0694b0d9820487b385d138c.html) documentation.

Create a new file named **`data.hdbcds`** in the **`moveilens/db/src/hdb`** folder.

By default the ***Graphical Editor*** will open. Close it, and right click on the **`data.hdbcds`** file, and go to **Open with > Code Editor**.

This is the full path of the created file:

```
movielens/db/src/hdb/data.hdbcds
```

Paste the following content:

```JavaScript
namespace aa.movielens.db.hdb;

context data {
    entity links {
        key MOVIEID : Integer;
            IMDBID  : Integer;
            TMDBID  : Integer;
    };

    entity movies {
        key MOVIEID : Integer;
            TITLE   : String(255);
            GENRES  : String(255);
    };

    entity ratings {
        key USERID    : Integer;
        key MOVIEID   : Integer;
            RATING    : DecimalFloat;
            TIMESTAMP : Integer;
    };

    entity tags {
        key USERID    : Integer;
        key MOVIEID   : Integer;
        key TAG       : String(255);
            TIMESTAMP : Integer;
    };
};
```

Save the file using the ![save](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the Table Data Artifacts)]

> #### **Table Data Artifact**
>
>The Table Data plug-in can be used to insert data defined in other design-time artifacts into database tables which are managed by SAP HANA DI and are not system-versioned, temporary, or virtual tables.
>&nbsp;
>For additional details, check the [Table Data in XS Advanced](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/2.0.03/en-US/35c4dd829d2046f29fc741505302f74d.html) documentation.

Create a new file named **`data.hdbtabledata`** in the **`moveilens/db/src/hdb`** folder.

This is the full path of the created file:

```
movielens/db/src/hdb/data.hdbtabledata
```

Paste the following content:

```JSON
{
    "format_version": 1,
    "imports": [
        {
            "column_mappings" : {
                "MOVIEID" : "movieId",
                "IMDBID" : "imdbId",
                "TMDBID" : "tmdbId"
            },
            "import_settings" : {
                "import_columns" : [ "MOVIEID", "IMDBID", "TMDBID"],
                "include_filter" : [], "exclude_filter" : []
            },
            "source_data" : {
                "data_type" : "CSV",
                "file_name" : "aa.movielens.db.data::links.csv",
                "has_header" : true,
                "dialect"   : "HANA"
            },
            "target_table" : "aa.movielens.db.hdb::data.links"  
        },
        {
            "column_mappings" : {
                "MOVIEID" : "movieId",
                "TITLE" : "title",
                "GENRES" : "genres"
            },
            "import_settings" : {
                "import_columns" : [ "MOVIEID", "TITLE", "GENRES"],
                "include_filter" : [], "exclude_filter" : []
            },
            "source_data" : {
                "data_type" : "CSV",
                "file_name" : "aa.movielens.db.data::movies.csv",
                "has_header" : true,
                "dialect"   : "HANA"
            },
            "target_table" : "aa.movielens.db.hdb::data.movies"  
        },
        {
            "column_mappings" : {
                "USERID" : "userId",
                "MOVIEID" : "movieId",
                "RATING" : "rating",
                "TIMESTAMP": "timestamp"
            },
            "import_settings" : {
                "import_columns" : [ "USERID", "MOVIEID", "RATING", "TIMESTAMP"],
                "include_filter" : [], "exclude_filter" : []
            },
            "source_data" : {
                "data_type" : "CSV",
                "file_name" : "aa.movielens.db.data::ratings.csv",
                "has_header" : true,
                "dialect"   : "HANA"
            },
            "target_table" : "aa.movielens.db.hdb::data.ratings"  
        },
        {
            "column_mappings" : {
                "USERID" : "userId",
                "MOVIEID" : "movieId",
                "TAG" : "tag",
                "TIMESTAMP": "timestamp"
            },
            "import_settings" : {
                "import_columns" : [ "USERID", "MOVIEID", "TAG", "TIMESTAMP"],
                "include_filter" : [], "exclude_filter" : []
            },
            "source_data" : {
                "data_type" : "CSV",
                "file_name" : "aa.movielens.db.data::tags.csv",
                "has_header" : true,
                "dialect"   : "HANA"
            },
            "target_table" : "aa.movielens.db.hdb::data.tags"  
        }        
    ]
}
```

Save the file using the ![plus](00-save.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Build the SAP HANA Database Module)]

Right click on the **`db`** folder and select **Build**.

![Web IDE](05-01.png)

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Open the Database Explorer)]

On the left side bar, click on the Database Explorer icon ![Web IDE](00-dbexplorer-icon.png) icon.

![Web IDE](00-dbexplorer.png)

Use the ***Add a database to the Database Explorer*** icon ![Web IDE](00-dbexplorer-plus.png).

Select **HDI Container** as ***Database Type*** and pick the entry that starts with ***`XSA_DEV`*** and ends with ***`movielens-hdi_db`*** that belongs to the **development** ***Space***, then click on **OK**.

![Web IDE](06-01.png)

Select the **Tables** element, and your tables should appear in the list.

![Web IDE](06-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Validate the import)]

Let's now validate that the data was properly loaded.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png).

```SQL
select 'links'   as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.links"
union all
select 'movies'  as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.movies"
union all
select 'ratings' as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.ratings"
union all
select 'tags'    as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.tags";
```

Based on the result returned by the above SQL statement, provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
