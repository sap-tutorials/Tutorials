---
title: Import the MovieLens dataset (MovieLens SQL)
description: Learn how to easily import flat dataset files in your SAP HANA, express edition instance
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 15
---

## Prerequisites
 - [Use Machine Learning to Build a Movie Recommendation model using SQL](https://developers.sap.com/group.hxe-aa-movielens-sql.html)

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

Before using these data sets, please review the <a href="http://files.grouplens.org/datasets/movielens/ml-latest-small-README.html" target="new">README</a> file for the usage licenses and other details.

> ### **Note:**:
>
> As the datasets get updated regularly, you will be provided with a copy of the ***small dataset***. so that the implemented validation will work.
>
> However, you are free to use the latest version of the dataset from the `MovieLens` website, but you won't be able to mark you work as completed as the validation will fail.
>
> In addition, if you have additional capacity resources, you may want to run this tutorial series with the larger datasets. but again the validations steps were built based on a local copy of the ***small dataset***.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](SAP HANA data import options)]

There are multiple ways to import data set files inside of your SAP HANA, express edition instance.

- ***Eclipse IDE***

The ***SAP HANA Tools*** plugin for Eclipse provides an ***Import/Export*** feature which would allow you to create the appropriate physical tables first and then import the data.

However, this would require the ***Eclipse IDE*** to be locally installed and properly configured with the ***SAP HANA Tools*** plugin.

Then, you would need to know the complete data file format description in order to create the tables with the proper columns structure. And, last but not least, any changes would require to recreate the all structure and reload the data.

If you want to learn more about this import method, you can check the following tutorial: **Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse**

- ***SAP HANA HDB Client***

The **SAP HANA HDB Client** provides an ***IMPORT FROM*** statement allowing you to import CSV files physically located on your SAP HANA, express edition host using a SQL command.

However, this method requires that the table are created before the execution of the command.

If you want to learn more about this import method, you can check the following tutorial: **Import CSV into SAP HANA, express edition using IMPORT FROM SQL command**.

#### ***Solution***

Both options are valid but for the sake of simplicity only the Eclipse option will be described here.

To learn more about these options, you can refer to:

- [Import CSV into SAP HANA, express edition using the SAP HANA Tools for Eclipse](https://developers.sap.com/tutorials/mlb-hxe-import-data-eclipse.html)
- [Import CSV into SAP HANA, express edition using IMPORT FROM SQL command](https://developers.sap.com/tutorials/mlb-hxe-import-data-sql-import.html)

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

As the dataset files get updated periodically, and in order to ensure consistency of content and validation, a copy of the data is available under the <a href="https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/hxe-aa-movielens-sql-02/data" target="new">data</a> directory within the SAP Tutorial GitHub repository.

Download the following files locally (right click on the link, then use the ***Save link as*** option):

- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/links.csv" target="new">links</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/movies.csv" target="new">movies</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/ratings.csv" target="new">ratings</a>
- <a href="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-02/data/tags.csv" target="new">tags</a>

Using the  **File > Import...** menu, select **SAP HANA Content > Data From Local File**.

You can also use the search field to locate the entry.

![image Step 3](03-0.png)

Click on **Next**.

Select the Target System connection **HXE @ HXE (`ML_USER`)**.

![image Step 3](03-1.png)

Click on **Next**.

The following panel allows you to set a series of import options:

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

**Repeat this operation for each dataset files.**

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
You can follow the [Use Jupyter Notebook with SAP HANA, express edition](https://developers.sap.com/tutorials/mlb-hxe-tools-jupyter.html)) tutorial for more details.
