---
title: Import the MovieLens dataset
description: Learn how to easily import flat dataset files in your SAP HANA, express edition instance
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 15
---

## Details
### You will learn

- Create a Multi-Target Application Project
- Save your Web IDE project in a GIT repository


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

#### ***Solution***

Both options are valid and will be described here. To learn more about these options, you can refer to:

- [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html)
- [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-sql-import.html)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Download the dataset CSV files)]

As the dataset files get updated periodically, and in order to ensure consistency of content and validation, a copy of the data is available under the <a href="https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/hxe-aa-movielens-sql-02/data" target="new">data</a> directory within the SAP Tutorial GitHub repository.

Download the following files locally (right click on the link, then use the ***Save link as*** option):

- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/links.csv" target="new">links</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/movies.csv" target="new">movies</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/ratings.csv" target="new">ratings</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/tags.csv" target="new">tags</a>

Again, before using these files, make sure you have reviewed the dataset <a href="http://files.grouplens.org/datasets/movielens/ml-latest-small-README.html" target="new">README</a> file for the usage licenses and other details.

If you are planning on using the ***IMPORT FROM*** statement from the SAP HANA HDB Client, then you will need to transfer the file using an FTP client to following location on the HXE server:

```
/usr/sap/HXE/HDB90/work/data/
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the table structure)]

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statement to create the table structure:

```SQL
create column table movielens_links(
  movieid integer not null,
  imdbid  integer,
  tmdbid  integer,
  primary key (
    movieid
  )
);

create column table movielens_movies(
  movieid integer not null,
  title   nvarchar(255),
  genres  nvarchar(255),
  primary key (
    movieid
  )
);

create column table movielens_ratings(
  userid    integer not null,
  movieid   integer not null,
  rating    decimal,
  timestamp integer,
  primary key (
    userid,
    movieid
  )
);

create column table movielens_tags(
  userid    integer not null,
  movieid   integer not null,
  tag       nvarchar(255)  not null,
  timestamp integer,
  primary key (
    userid,
    movieid,
    tag
  )
);
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Import the Dataset)]

Depending on your the import method option you have selected, complete one of the following steps.

### **Using the SAP HANA Tools for Eclipse**

You can import the data using the [SAP HANA Tools for Eclipse Import feature](https://www.sap.com/developer/tutorials/mlb-hxe-import-data-eclipse.html) using the following details:

- **File Details:**

    - Set the **Field Delimiter** value to **Comma (,)**.
    - Check the **Header row exists** box and set the **Header row** value to 1
    - Check the **Import all data** box.

- **Manage Table Definition and Data Mappings:**

    Using the **Mapping menu** icon, select the **One by One** option.

The source files should be mapped with the following target tables:

| Source File                   | Target Table      
| ----------------------------- | -----------------
| `links.csv`                   | `ML_USER.MOVIELENS_LINKS`
| `movies.csv`                  | `ML_USER.MOVIELENS_MOVIES`
| `ratings.csv`                 | `ML_USER.MOVIELENS_RATINGS`
| `tags.csv`                    | `ML_USER.MOVIELENS_TAGS`

### **Using the IMPORT FROM SQL command**

The dataset files should be located in: **`/usr/sap/HXE/HDB90/work/data/movielens/`**

Connect to the **HXE** tenant using the **`ML_USER`** user credentials using your SQL query tool.

Execute the following SQL statement:

```SQL
import from csv file '/usr/sap/HXE/HDB90/work/data/movielens/links.csv' into movielens_links
with
   record delimited by '\n'
   field delimited by ','
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/links.csv.err'
;

import from csv file '/usr/sap/HXE/HDB90/work/data/movielens/movies.csv' into movielens_movies
with
   record delimited by '\n'
   field delimited by ','
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/movies.csv.err'
;

import from csv file '/usr/sap/HXE/HDB90/work/data/movielens/ratings.csv' into movielens_ratings
with
   record delimited by '\n'
   field delimited by ','
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/ratings.csv.err'
;

import from csv file '/usr/sap/HXE/HDB90/work/data/movielens/tags.csv' into movielens_tags
with
   record delimited by '\n'
   field delimited by ','
   optionally enclosed by '"'
   skip first 1 row
   fail on invalid data
   error log '/home/jupyteradm/log/tags.csv.err'
;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the imported data)]

You can verify that the data was imported properly using the following SQL statement:

```SQL
select 'links'   as table, count(1) as count from movielens_links
union all
select 'movies'  as table, count(1) as count from movielens_movies
union all
select 'ratings' as table, count(1) as count from movielens_ratings
union all
select 'tags'    as table, count(1) as count from movielens_tags;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

> ### **Note** If you are using Jupyter Notebook, you can download the following  [notebook](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/hxe-aa-movielens-sql-02.ipynb) to run most of the SQL statement listed in the tutorial.
