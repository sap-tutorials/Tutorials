---
title: MovieLens with SAP HANA PAL Apriori
description: Understand the capabilities and options made available with the SAP HANA SAP HANA Predictive Analytics Library (PAL), find the algorithm to address your goal, and apply it to the data set
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 30
---

## Prerequisites
 - [Use Machine Learning to Build a Movie Recommendation model using SQL](https://www.sap.com/developer/groups/hxe-aa-movielens-sql.html)

## Details
### You will learn
- Understand the basics about the SAP HANA Predictive Analytics Library
- How to call SAP HANA Predictive Analytics Library functions from SQL
- Identify which algorithm options are available for recommendation engines

[ACCORDION-BEGIN [Info: ](SAP HANA Predictive Analytics Library)]

The ***SAP HANA Predictive Analytics Library*** (PAL) is an ***Application Function Library*** (AFL) which defines a set of functions that can be called from within ***SAP HANA SQL Script*** (an extension of SQL) to perform analytic algorithms.

The Predictive Analysis Library (PAL) defines functions that can be called from within ***SQL Script*** procedures to perform analytic algorithms and includes classic and universal predictive analysis algorithms in the following data-mining categories:

- Clustering
- Classification
- Regression
- Association
- Time Series
- Preprocessing
- Statistics
- Social Network Analysis
- Miscellaneous

With over 90 algorithm functions across the above data-mining categories, the ***SAP HANA Predictive Analytics Library*** has been built and enhanced upon on the following goals:

- provide the required algorithms for SAP HANA applications features
- provide the most commonly used algorithms based on market surveys
- provide a set of algorithms generally available in other database or processing platforms

For more details about the PAL function, check the online <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/latest/en-US/f652a8186a144e929a1ade7a3cb7abe8.html" target="new">documentation</a>..

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Calling PAL functions)]

In order to use an ***SAP HANA PAL*** function in SAP HANA, ultimately an AFL wrapper must be created and then invoked.

Creating and invoking the AFL wrapper is performed by executing ***SAP HANA `SQLScript`***.

Other database objects also need to be created, such as table types or signature table.

Just like with the SAP HANA APL, there are two techniques for calling PAL functions, the ***direct technique*** and the ***procedure technique***.

### **The direct technique**:

This technique consists of explicitly generating an AFL wrapper for the PAL function to be executed.

The generation of this AFL wrapper requires the explicit creation of table types, signature table, input and output tables, etc.

This is all supposed to be done by the PAL consumer, through SQL DDL & DML statements.

Once the AFL wrapper is generated, it can be invoked through a call statement. This "direct technique" is always available.

Here is a quick code example with the direct technique:

```
-- --------------------------------------------------------------------------
-- Create the table types
-- --------------------------------------------------------------------------
DROP TYPE PAL_APRIORI_DATA_T;
CREATE TYPE PAL_APRIORI_DATA_T AS TABLE(
	"CUSTOMER" INTEGER,
	"ITEM" VARCHAR(20)
);

DROP TYPE PAL_APRIORI_RESULT_T;
CREATE TYPE PAL_APRIORI_RESULT_T AS TABLE(
	"PRERULE" VARCHAR(500),
	"POSTRULE" VARCHAR(500),
	"SUPPORT" DOUBLE,
	"CONFIDENCE" DOUBLE,
	"LIFT" DOUBLE
);

DROP TYPE PAL_APRIORI_PMMLMODEL_T;
CREATE TYPE PAL_APRIORI_PMMLMODEL_T AS TABLE(
	"ID" INTEGER,
	"PMMLMODEL" VARCHAR(5000)
);

DROP TYPE PAL_CONTROL_T;
CREATE TYPE PAL_CONTROL_T AS TABLE(
	"NAME" VARCHAR(100),
	"INTARGS" INTEGER,
	"DOUBLEARGS" DOUBLE,
	"STRINGARGS" VARCHAR (100)
);

-- --------------------------------------------------------------------------
-- Create the AFL wrapper corresponding to the target PAL function
-- --------------------------------------------------------------------------
DROP TYPE PROCEDURE_SIGNATURE_T;
CREATE TYPE PROCEDURE_SIGNATURE_T AS TABLE(
    "NAME"        VARCHAR (50),
    "INTARGS"     INTEGER,
    "DOUBLEARGS"  DOUBLE,
    "STRINGARGS"  VARCHAR (100)
);

DROP TYPE TRAINED_MODEL_T;
CREATE TYPE TRAINED_MODEL_T AS TABLE(
    "NAME"  VARCHAR (50),
    "VALUE" VARCHAR (5000)
);
DROP TABLE OPERATION_CONFIG;

DROP TYPE OPERATION_CONFIG_T;
CREATE TYPE OPERATION_CONFIG_T AS TABLE(
    "NAME"  VARCHAR (50),
    "VALUE" VARCHAR (5000)
);

-- --------------------------------------------------------------------------
-- Create the AFL wrapper corresponding to the target PAL function
-- --------------------------------------------------------------------------
DROP TABLE PAL_APRIORI_PDATA_TBL;
CREATE COLUMN TABLE PAL_APRIORI_PDATA_TBL(
	"POSITION" INT,
	"SCHEMA_NAME" NVARCHAR(256),
	"TYPE_NAME" NVARCHAR(256),
	"PARAMETER_TYPE" VARCHAR(7)
);
INSERT INTO PAL_APRIORI_PDATA_TBL VALUES (1, 'MYSCHEMA', 'PAL_APRIORI_DATA_T', 'IN');
INSERT INTO PAL_APRIORI_PDATA_TBL VALUES (2, 'MYSCHEMA', 'PAL_CONTROL_T', 'IN');
INSERT INTO PAL_APRIORI_PDATA_TBL VALUES (3, 'MYSCHEMA', 'PAL_APRIORI_RESULT_T', 'OUT');
INSERT INTO PAL_APRIORI_PDATA_TBL VALUES (4, 'MYSCHEMA', 'PAL_APRIORI_PMMLMODEL_T', 'OUT');

CALL "SYS".AFLLANG_WRAPPER_PROCEDURE_DROP('MYSCHEMA', 'PAL_APRIORI_RULE_PROC');
CALL "SYS".AFLLANG_WRAPPER_PROCEDURE_CREATE('AFLPAL', 'APRIORIRULE', 'MYSCHEMA', 'PAL_APRIORI_RULE_PROC', PAL_APRIORI_PDATA_TBL);

-- --------------------------------------------------------------------------
-- Create the Parameter table corresponding to the target PAL function
-- --------------------------------------------------------------------------
DROP TABLE #PAL_CONTROL_TBL;
CREATE LOCAL TEMPORARY COLUMN TABLE #PAL_CONTROL_TBL(
	"NAME" VARCHAR(100),
	"INTARGS" INTEGER,
	"DOUBLEARGS" DOUBLE,
	"STRINGARGS" VARCHAR (100)
);
INSERT INTO #PAL_CONTROL_TBL VALUES ('THREAD_NUMBER', 2, null, null);
INSERT INTO #PAL_CONTROL_TBL VALUES ('MIN_SUPPORT', null, 0.1, null);
INSERT INTO #PAL_CONTROL_TBL VALUES ('MIN_CONFIDENCE', null, 0.3, null);
INSERT INTO #PAL_CONTROL_TBL VALUES ('MIN_LIFT', null, 1.1, null);
INSERT INTO #PAL_CONTROL_TBL VALUES ('MAX_CONSEQUENT', 1, null, null);

DROP TABLE PAL_APRIORI_RESULT_TBL;
CREATE COLUMN TABLE PAL_APRIORI_RESULT_TBL LIKE PAL_APRIORI_RESULT_T;

DROP TABLE PAL_APRIORI_PMMLMODEL_TBL;
CREATE COLUMN TABLE PAL_APRIORI_PMMLMODEL_TBL LIKE PAL_APRIORI_PMMLMODEL_T;

-- --------------------------------------------------------------------------
-- Call the target PAL function using the generated wrapper
-- --------------------------------------------------------------------------
CALL "DM_PAL".PAL_APRIORI_RULE_PROC(PAL_APRIORI_TRANS_TBL, #PAL_CONTROL_TBL, PAL_APRIORI_RESULT_TBL, PAL_APRIORI_PMMLMODEL_TBL) WITH overview;
```

For more information please refer to the online <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/1.0.12/en-US" target="new">documentation</a>..

### **The procedure technique**:

This technique is not only much simpler than the direct technique, but it's also more efficient and scalable.

Instead of having to deal with the life cycle of the AFL wrappers and all its companion database objects on a per-call basis, the PAL user can directly call PAL specific stored procedures which take care of all the AFL details.

These PAL stored procedures are part of the default installation and available under the **``**.

Here is a quick code example with the procedure technique:

```
-- --------------------------------------------------------------------------
-- Create the AFL wrapper corresponding to the target PAL function
-- --------------------------------------------------------------------------
DROP TABLE PAL_PARAMETER_TBL;
CREATE TABLE PAL_PARAMETER_TBL (
	"PARAM_NAME " VARCHAR(100),
	"INT_VALUE" INTEGER,
	"DOUBLE_VALUE" DOUBLE,
	"STRING_VALUE" VARCHAR (100)
);

INSERT INTO PAL_PARAMETER_TBL VALUES ('MIN_SUPPORT', null, 0.1, null);
INSERT INTO PAL_PARAMETER_TBL VALUES ('MIN_CONFIDENCE', null, 0.3, null);
INSERT INTO PAL_PARAMETER_TBL VALUES ('MIN_LIFT', null, 1.1, null);
INSERT INTO PAL_PARAMETER_TBL VALUES ('MAX_CONSEQUENT', 1, null, null);
INSERT INTO PAL_PARAMETER_TBL VALUES ('PMML_EXPORT', 1, null, null);

DROP TABLE PAL_APRIORI_TRANS_TBL;
CREATE COLUMN TABLE PAL_APRIORI_TRANS_TBL (
	"CUSTOMER" INTEGER,
	"ITEM" VARCHAR(20)
);

INSERT INTO PAL_APRIORI_TRANS_TBL VALUES (<CUSTOMER>, <ITEM>);

-- --------------------------------------------------------------------------
-- Execute the PAL function using its AFL wrapper and the actual input/output tables
-- --------------------------------------------------------------------------
CALL _SYS_AFL.PAL_APRIORI(PAL_APRIORI_TRANS_TBL, PAL_PARAMETER_TBL, ?, ?); with overview;
```

We will use the **procedure technique** in this tutorial.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Recommendation engines with SAP HANA PAL)]

As stated above, the ***SAP HANA Predictive Analytics Library*** (PAL) delivers over a hundred of ***best in class industry standard*** algorithms.

There are multiple association rules functions that can be used to address both a collaborative filtering and a content-based filtering scenario.

For both the collaborative filtering and the content-based filtering scenario, the SAP HANA PAL ***APRIORI*** algorithm is probably the most appropriate and the easiest one to use.

However, some preparation will be required to get the input in the right format.

> ### **Note**
><center><b>SAP HANA PAL Apriori algorithm</b></center>
>Given a set of items, the algorithm attempts to find subsets which are common to at least a minimum number of the item sets.
>&nbsp;
>Apriori uses a "bottom up" approach, where frequent subsets are extended one item at a time, a step known as candidate generation, and groups of candidates are tested against the data.
>&nbsp;
>The algorithm terminates when no further successful extensions are found. Apriori uses breadth-first search and a tree structure to count candidate item sets efficiently.
>&nbsp;
>It generates candidate item sets of length k from item sets of length k-1, and then prunes the candidates which have an infrequent sub pattern.
>&nbsp;
>The candidate set contains all frequent k-length item sets.
>&nbsp;
>After that, it scans the transaction database to determine frequent item sets among the candidates.
>Extracted from the documentation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](SAP HANA PAL APRIORI function)]
While assessing the available data, we found out that only the ratings can be used to build our collaborative filtering an content-based filter scenarios.

The SAP HANA PAL function that we will be using in this step is

- <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/1.0.12/en-US/7a073d66173a4c1589ef5fbe5bb3120f.html" target="new"><b>Apriori</b></a>

To apply a collaborative filtering approach with the ratings dataset, we would train a SAP HANA PAL `Apriori` model using the list of rated movies as the a *transactional* dataset, where each entry will represent a link between a user and an item.

The SAP HANA PAL `Apriori` algorithm provide multiple configuration options like:

Name                                 | Description
-------------------------------------|------------------------------
**maximum consequent**               | Maximum length of dependent items.
**maximum item length**              | Total length of leading items and dependent items in the output.
**minimum support**                  | ignores items whose support values are greater than the value during the frequent items mining phase (UBIQUITOUS).
**left/right-hand side restriction** | Specifies that some items are only allowed on the left/right-hand side of the association rules.

> ### **Note**
>In this scenario, we are not considering the rating notation itself (between 0.5 to 5) to build the output list, which would help a list of movies that both users rated the same way. To achieve that, we would need to investigate the ***Factorized Polynomial Regression Models*** algorithm available in SAP HANA 2.0 SPS02.
We could also transform the data structure, and use the movie as one node (entity type) and the user associated with the rating notation as the second node (entity type), then use the same algorithm. And finally instead of using the user as the entry point we would use the user and the rating notation as the entry point.
>
If you want to try out this scenario, you can build a view where the user id and the rating are concatenated into one column that will be used as the second entity type.

The PAL functions are really strict on the input dataset format, so you will create a view to provide the input dataset in the proper format (with only the user and movie id) .

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Select, install and configure a SQL query tool)]

As you will mostly execute SQL commands during this series, you will need to setup a SQL query tool for SAP HANA, express edition as describe in the following tutorial group:

 - [Select, install and configure a SQL query tool for SAP HANA, express edition](https://www.sap.com/developer/groups/mlb-hxe-tools-sql.html).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Run the APRIORI PAL function)]

While assessing the available data, we found out that only the ratings can be used to build our collaborative filtering an content-based filter scenarios.

The SAP HANA PAL function that we will be using in this step is

- <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/1.0.12/en-US/7a073d66173a4c1589ef5fbe5bb3120f.html" target="new"><b>Apriori</b></a>

To apply a collaborative filtering approach with the ratings dataset, we would train a SAP HANA PAL `Apriori` model using the list of rated movies as the a *transactional* dataset, where each entry will represent a link between a user and an item.

The SAP HANA PAL `Apriori` algorithm provide multiple configuration options like:

- **maximum consequent**: Maximum length of dependent items.
- **maximum item length** :Total length of leading items and dependent items in the output.
- **minimum support**: ignores items whose support values are greater than the value during the frequent items mining phase (UBIQUITOUS).
- **left/right-hand side restriction**: specifies that some items are only allowed on the left/right-hand side of the association rules.

> ### **Note**
>In this scenario, we are not considering the rating notation itself (between 0.5 to 5) to build the output list, which would help a list of movies that both users rated the same way. To achieve that, we would need to investigate the ***Factorized Polynomial Regression Models*** algorithm available in SAP HANA 2.0 SPS02.
We could also transform the data structure, and use the movie as one node (entity type) and the user associated with the rating notation as the second node (entity type), then use the same algorithm. And finally instead of using the user as the entry point we would use the user and the rating notation as the entry point.
>
If you want to try out this scenario, you can build a view where the user id and the rating are concatenated into one column that will be used as the second entity type.

The PAL functions are really strict on the input format, so you will need to create a view to provide the input dataset in the proper format (`PAL_MOVIELENS_APRIORI_DATA_INPUT`) .

The results will be stored in multiple tables:

 - **`PAL_MOVIELENS_APRIORI_RESULT`** : the rules set linking and movies to movies
 - **`PAL_MOVIELENS_APRIORI_PMMLMODEL`** : the model definition in a PMML format

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following series of SQL statements.

### **Clean previous results:**

```SQL
-- --------------------------------------------------------------------------
-- drop function in/out tables, helper tables and views
-- --------------------------------------------------------------------------
drop table pal_movielens_parameters;
drop table pal_movielens_apriori_pmmlmodel;
drop table pal_movielens_apriori_result;
drop  view pal_movielens_apriori_data_input;
```

> ### **Note**
>You may receive a series of errors and warnings in the console log while running the above code. They should all be related to the drop statements at the beginning which are intended to help you re-run the script if needed.

### **Create input and output table and views structures:**

```SQL
-- --------------------------------------------------------------------------
-- create the config and output tables
-- --------------------------------------------------------------------------
create column table pal_movielens_parameters (
	param_name   varchar(100),
	int_value    integer,
	double_value double,
	string_value varchar (100)
);

create column table pal_movielens_apriori_result (
	prerule    varchar(500),
	postrule   varchar(500),
	support    double,
	confidence double,
	lift       double
);
create row table pal_movielens_apriori_pmmlmodel (
  row_index integer,
  model_content clob
);
-- --------------------------------------------------------------------------
-- create the input data view
-- --------------------------------------------------------------------------
create view pal_movielens_apriori_data_input as
select userid, movieid
from   movielens_ratings;
```

### **Set the algorithm parameters:**

```SQL
-- --------------------------------------------------------------------------
-- configuration
-- --------------------------------------------------------------------------
truncate table pal_movielens_parameters;
insert into pal_movielens_parameters values ('MIN_SUPPORT'   , null, 0.1 , null); -- no default
insert into pal_movielens_parameters values ('MIN_CONFIDENCE', null, 0.1 , null); -- no default
insert into pal_movielens_parameters values ('MIN_LIFT'      , null, 0.0 , null); -- default is 0.0
insert into pal_movielens_parameters values ('MAX_CONSEQUENT', 1   , null, null); -- default is 500
insert into pal_movielens_parameters values ('MAXITEMLENGTH' , 2   , null, null); -- default is 5
insert into pal_movielens_parameters values ('UBIQUITOUS'    , null, 1.0 , null); -- default is 1.0
insert into pal_movielens_parameters values ('PMML_EXPORT'   , 1   , null, null); -- default is 0
insert into pal_movielens_parameters values ('TIMEOUT'       , 3600, null, null); -- default is 3600
insert into pal_movielens_parameters values ('THREAD_RATIO'  , null, 0.0 , null); -- default is 0.0
select * from pal_movielens_parameters;
```

You will notice that the `MIN_SUPPORT` & `MIN_CONFIDENCE` are mandatory attributes to filter out some of the candidate associations.

The value selected here were set relatively high to prevent long running processes on your trial environment. But in a real life scenario, these settings must be determined based on an the initial analysis of the training dataset.

### **Run the algorithm:**

```SQL
truncate table pal_movielens_apriori_result;
truncate table pal_movielens_apriori_pmmlmodel;
-- --------------------------------------------------------------------------
-- execute the pal function to train the model
-- --------------------------------------------------------------------------
call _sys_afl.pal_apriori(
    pal_movielens_apriori_data_input
  , pal_movielens_parameters
  , pal_movielens_apriori_result
  , pal_movielens_apriori_pmmlmodel
) with overview;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the results views)]

From this model, you can now create a view that you will use to extract a list of up to 5 recommended movies per users based on other users with the most similar rating list.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following series of SQL statements.

As a reminder, the model used the ***Support*** as a result weight/score metric, that will be transformed into a ***confidence*** metric in the result view.

### **Collaborative filtering results**

Now, you can create the view that you will use to extract up to 5 movies per users based on other users with the most similar rating list.

The generated result in the `PAL_MOVIELENS_APRIORI_RESULT` are "just" the rules between movies, now you need to associate the users with their rated movies as `PRERULE`, and rank the `POSTRULE` using the confidence.

The code of this view was built from scratch:

```SQL
drop   view pal_movielens_apriori_result_collaborative;
create view pal_movielens_apriori_result_collaborative as
select *
from (
  select
      t1.userid
    , row_number() over(partition by t1.userid order by t1.score desc, t1.consequent desc ) as rank
    , t1.consequent as movieid
    , t1.score
    , movies.title
    , movies.genres
    , links.imdbid
    , links.tmdbid
  from (
    select input_data.userid, rules.postrule as consequent, max(rules.confidence) as score
    from movielens_ratings as input_data
    left outer join (select * from pal_movielens_apriori_result) rules on (cast (input_data.movieid as varchar(500)) = rules.prerule)
    where rules.postrule is not null
    group by input_data.userid, rules.postrule
  ) t1
    left outer join movielens_movies movies on movies.movieid = t1.consequent
    left outer join movielens_links  links  on links.movieid  = t1.consequent
) t1
where t1.rank <= 5;
```

### **Content-based filtering results**

To address the content-based filtering scenario with the rating dataset, your goal is to provide a list of similar movies based on the number of users who rated the same movies together.

Here, it assumes that the rating action of a single movie by multiple users is a proof of similarity.

For this scenario, you won't actually need to build another model as previous one already provides the links between movies based on user ratings.

Now, you can create the view to extract the results.

The code of this view was built from scratch:


```SQL
drop   view pal_movielens_apriori_result_contentbased;
create view pal_movielens_apriori_result_contentbased as
select *
from (
  select
      t1.movieid
    , row_number() over(partition by t1.movieid order by t1.score desc, t1.consequent desc ) as rank
    , t1.consequent as similar_movieid
    , t1.score
    , movies.title
    , movies.genres
    , links.imdbid
    , links.tmdbid
  from (
    select movieid, rules.postrule as consequent, rules.confidence as score
    from movielens_movies as input_data
    left outer join (select * from pal_movielens_apriori_result) rules on (cast (input_data.movieid as varchar(500)) = rules.prerule)
    where rules.postrule is not null
  ) t1
    left outer join movielens_movies movies on movies.movieid = t1.consequent
    left outer join movielens_links  links  on links.movieid  = t1.consequent
) t1
where t1.rank <= 5;
```

Again, this model is for demonstration purpose and very specific to my initial purpose which is to give you a quick tour of the algorithm and may not be applicable as-is to other use cases or dataset.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Validate the collaborative filtering results)]

In order to be consistent, we should validate the same details that you will verify with other models results.

Let's verify how many users will actually get recommendations using the following SQL:

```SQL
select reco_count, count(1) as user_count
from (
  select userid, max(rank) as reco_count
  from   pal_movielens_apriori_result_collaborative
  group by userid
) group by reco_count order by reco_count desc;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
select
    count(1) as movie_count
  , count(1) *100 / (select count(1) as count from movielens_movies ) as movie_ratio
from (
  select movieid
  from   pal_movielens_apriori_result_collaborative
  group by movieid
);
```

Let's verify how many distinct movies will potentially get recommended to a user (not just the top 5 scores) using the following SQL:

```SQL
select
    count(1) as movie_count
  , count(1) *100 / (select count(1) as count from movielens_movies ) as movie_ratio
from (
  select prerule as movieid
  from pal_movielens_apriori_result
  where prerule not like '%&%'
  group by prerule
);
```

Based on the last result, you can conclude that:

 - all 660 users will receive the requested 5 recommendations
 - only about 0.45% of the movies (41 out of the 9,125) are in the top 5 lists
 - only about 2.2% of the movies (200 out of the 9,125) will get potentially recommended


Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

-BEGIN [Step 8: ](Validate the content-based filtering results)]

In order to be consistent, we should validate the same details that you will verify with other models results.

Let's verify how many movies will actually get recommendations using the following SQL:

```SQL
select reco_count, count(1) as movie_count
from (
  select movieid, max(rank) as reco_count
  from pal_movielens_apriori_result_contentbased
  group by movieid
) group by reco_count order by 1 desc;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
select
    count(1) as movie_count
  , count(1) *100 / (select count(1) as count from movielens_movies ) as movie_ratio
from (
  select movieid
  from pal_movielens_apriori_result_contentbased
  group by movieid
);
```

Only 200 movies (out of the 9,125) will receive at least one recommendations, and only 166 will receive the requested 5 recommendations.

Let's verify how many rating does the movies with no recommendation have using the following SQL:

```SQL
select rating_count, count(1) as movie_count
from (
  select ratings.movieid, count(1) as rating_count
  from movielens_ratings ratings
  left outer join (
    select movieid
    from (
      select prerule as movieid
      from pal_movielens_apriori_result
      where prerule not like '%&%'
      group by prerule
    )
  ) t1 on (ratings.movieid = t1.movieid)
  where t1.movieid is null
  group by ratings.movieid
) group by rating_count;
```

As you can see, the movies with no recommendations have up to 92 ratings, and this list include the 3063 movies with only one rating and the 1202 with only 2 ratings.

[DONE]
[ACCORDION-END]

> ### **Note** If you are using Jupyter Notebook, you can download the following  [notebook](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-05/hxe-aa-movielens-sql-05.ipynb) to run most of the SQL statement listed in the tutorial.

For the purpose of this tutorial series we will not play further with the algorithm, its parameters or the data. However you are more than welcome to do so considering the resources currently made available to you on the SAP Cloud Platform.
