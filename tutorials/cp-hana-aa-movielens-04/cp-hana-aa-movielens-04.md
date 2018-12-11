---
title: SAP HANA PAL & the MovieLens dataset
description: Understand the capabilities and options made available with SAP HANA PAL and apply them to the data set to build your recommendation engine
auto_validation: true
primary_tag: topic>machine-learning
tags: [  tutorial>beginner, products>sap-hana, products>sap-cloud-platform, topic>machine-learning ]
time: 20
---

## Prerequisites
- [Leverage SAP HANA 1.0 Machine Learning capabilities to build a recommendation engine on the SAP Cloud Platform](https://developers.sap.com/group.cp-hana-aa-movielens.html)

## Details
### You will learn
- How to use SAP HANA PAL APRIORI algorithm

> ### **Note**
>As of today, the SAP Cloud Platform provide an SAP HANA MDC instance up to version 1.0 SPS12 which will be using for the rest of the series.
If you are using a different version of SAP HANA, the code and the results may differ.
>
For more details about the PAL function, check the online <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/1.0.12/en-US/f652a8186a144e929a1ade7a3cb7abe8.html" target="new">documentation</a>.

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

For more details about the PAL function, check the online <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/1.0.12/en-US/f652a8186a144e929a1ade7a3cb7abe8.html" target="new">documentation</a>..

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Calling SAP HANA PAL functions)]

In order to use an ***SAP HANA PAL*** function in SAP HANA, ultimately an AFL wrapper needs to be created and invoked and this AFL wrapper is going to invoke the PAL function.

Creating and invoking the AFL wrapper is performed by executing SQL statements through a ***SAP HANA SQL Script*** (an extension of SQL).

Other database objects also need to be created, such as database table types and database tables.

> ### **Note**
> With version 2.0 SPS02 of SAP HANA, a new and simplified scripting approach has been introduced. However, as the SAP Cloud Platform currently only offers SAP HANA 1.0 SPS12 (which may change in the future), we will only describe here what is currently possible with an SAP HANA instance on the SAP Cloud Platform.

Just like the ***SAP HANA APL*** direct technique, it consists of explicitly generating an AFL wrapper for the APL function to be executed.
The generation of this AFL wrapper requires the explicit creation of table types, signature table, input and output tables, etc.
This is all supposed to be done by the PAL consumer, through SQL DDL & DML statements.
Once the AFL wrapper is generated, it can be invoked through a call statement.

Here is a quick code example:

```
-- --------------------------------------------------------------------------
-- Create the table type for the dataset
-- --------------------------------------------------------------------------
DROP TYPE TRAINING_DATASET_T;
-- the training dataset definition
CREATE TYPE TRAINING_DATASET_T AS TABLE( .... );

-- --------------------------------------------------------------------------
-- Create the AFL wrapper corresponding to the target APL function
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
DROP TABLE CREATE_MODEL_SIGNATURE;
CREATE COLUMN TABLE CREATE_MODEL_SIGNATURE like PROCEDURE_SIGNATURE_T;
-- the signature is defined in the PAL API documentation
INSERT INTO CREATE_MODEL_SIGNATURE VALUES (1,'MYSCHEMA', 'TRAINING_DATASET_T' ,'IN');
INSERT INTO CREATE_MODEL_SIGNATURE VALUES (2,'MYSCHEMA', 'OPERATION_CONFIG_T' ,'IN');
INSERT INTO CREATE_MODEL_SIGNATURE VALUES (3,'MYSCHEMA', 'TRAINED_MODEL_T'    ,'OUT');

call SYS.AFLLANG_WRAPPER_PROCEDURE_DROP('MYSCHEMA','APLWRAPPER_CREATE_MODEL');
call SYS.AFLLANG_WRAPPER_PROCEDURE_CREATE('AFLPAL','ARIMATRAIN','MYSCHEMA', 'APLWRAPPER_CREATE_MODEL', "CREATE_MODEL_SIGNATURE");

DROP TABLE OPERATION_CONFIG;
CREATE COLUMN TABLE OPERATION_CONFIG like OPERATION_CONFIG_T;
-- the function configuration is defined in the PAL API documentation
INSERT INTO OPERATION_CONFIG VALUES ('P', 1,null,null);
INSERT INTO OPERATION_CONFIG VALUES ('Q', 1,null,null);
INSERT INTO OPERATION_CONFIG VALUES ('D', 0,null,null);
INSERT INTO OPERATION_CONFIG VALUES ('METHOD', 1,null,null);
INSERT INTO OPERATION_CONFIG VALUES ('STATIONARY', 1,null,null);

DROP TABLE TRAINED_MODEL;
CREATE COLUMN TABLE TRAINED_MODEL LIKE TRAINED_MODEL_T;

CALL APLWRAPPER_CREATE_MODEL("TRAINING_DATASET", "OPERATION_CONFIG");
```

For more information please refer to the online <a href="https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/1.0.12/en-US" target="new">documentation</a>..

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Recommendation engines with SAP HANA PAL)]

As stated above, the ***SAP HANA Predictive Analytics Library*** (PAL) delivers ***best in class industry standard*** algorithm.

Even if version 1.0 SPS12 doesn't provide an out-of-the-box ***Recommendation*** function (there is one in version 2.0 SPS02), there is a set of association rules functions that can be used to address both a collaborative filtering and a content-based filtering scenario.

For both the collaborative filtering and the content-based filtering scenario, the SAP HANA PAL ***APRIORI*** algorithm is probably the most appropriate and the easiest one to use. However, some preparation will be required to get the input in the right format.

> ### **Note**
><center><b>SAP HANA PAL Apriori algorithm</b></center>
>Given a set of items, the algorithm attempts to find subsets which are common to at least a minimum number of the item sets. Apriori uses a "bottom up" approach, where frequent subsets are extended one item at a time, a step known as candidate generation, and groups of candidates are tested against the data. The algorithm terminates when no further successful extensions are found. Apriori uses breadth-first search and a tree structure to count candidate item sets efficiently. It generates candidate item sets of length k from item sets of length k-1, and then prunes the candidates which have an infrequent sub pattern. The candidate set contains all frequent k-length item sets. After that, it scans the transaction database to determine frequent item sets among the candidates.
>Extracted from the documentation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Switch to the Catalog perspective)]

In our series of SQL statement, we will be using the **Catalog** perspective available in the **SAP HANA Web-based Development Workbench**.

If your are accessing the ***SAP HANA Web-based Development Workbench*** main panel then click on **Catalog**:

![SAP HANA Web-based Development Workbench](01.png)

Else if you are already accessing one of the perspective, then use the ![plus](0-navigation.png) icon from the menu:

![SAP HANA Web-based Development Workbench](02.png)

> ### **Note**
>**Make sure the currently connected user is `MOVIELENS_USER` and not SYSTEM**. Check the upper right corner of the SAP HANA Web-based Development Workbench.

For each of the next step, you can decide to open a new **SQL Console** using the ![sql](0-opensqlconsole.png) icon from the menu or reuse the same one by replacing its current over and over.

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

The PAL functions are really strict on the input format, so you will need to create a view to provide the input dataset in the proper format (`PAL_APRIORI_MODEL_INPUT`) .

The results will be stored in the `PAL_APRIORI_RESULT` table under the `movielens` schema.

Open a new **SQL Console** using the ![sql](0-opensqlconsole.png) icon from the menu or reuse an existing one.

Paste the following content in the console, and use the execute icon ![run](0-run.png) from the menu.

```SQL
SET SCHEMA "MOVIELENS";
-- --------------------------------------------------------------------------
-- Drop generic table types
-- --------------------------------------------------------------------------
DROP TYPE "MOVIELENS"."PAL_T_MOVIELENS_RATINGS";
DROP TYPE "MOVIELENS"."PAL_T_OPERATION_CONFIG";
DROP TYPE "MOVIELENS"."PAL_T_APRIORI_RESULT";
DROP TYPE "MOVIELENS"."PAL_T_APRIORI_PMMLMODEL";
-- --------------------------------------------------------------------------
-- Drop generic tables
-- --------------------------------------------------------------------------
DROP TABLE "MOVIELENS"."PAL_OPERATION_CONFIG";
DROP TABLE "MOVIELENS"."PAL_PROCEDURE_SIGNATURE";
DROP TABLE "MOVIELENS"."PAL_APRIORI_RESULT";
-- --------------------------------------------------------------------------
-- PAL
-- --------------------------------------------------------------------------
DROP   VIEW "MOVIELENS"."PAL_APRIORI_MODEL_INPUT";
CREATE VIEW "MOVIELENS"."PAL_APRIORI_MODEL_INPUT" AS
SELECT "USERID", "MOVIEID"
FROM   "MOVIELENS"."public.aa.movielens.hdb::data.RATINGS";
-- --------------------------------------------------------------------------
-- Create PAL table types
-- --------------------------------------------------------------------------
CREATE TYPE "MOVIELENS"."PAL_T_MOVIELENS_RATINGS" AS TABLE (
  "USERID"  INTEGER,
  "MOVIEID" INTEGER
);
CREATE TYPE "MOVIELENS"."PAL_T_OPERATION_CONFIG" AS TABLE (
  "NAME"       VARCHAR(100),
  "INTARGS"    INTEGER,
  "DOUBLEARGS" DOUBLE,
  "STRINGARGS" VARCHAR (100)
);
CREATE TYPE "MOVIELENS"."PAL_T_APRIORI_RESULT" AS TABLE (
  "PRERULE"    VARCHAR(500),
  "POSTRULE"   VARCHAR(500),
  "SUPPORT"    DOUBLE,
  "CONFIDENCE" DOUBLE,
  "LIFT"       DOUBLE
);
CREATE TYPE "MOVIELENS"."PAL_T_APRIORI_PMMLMODEL" AS TABLE (
  "ID" INTEGER,
  "PMMLMODEL" VARCHAR(5000)
);
-- --------------------------------------------------------------------------
-- Create PAL function signature table
-- --------------------------------------------------------------------------
CREATE COLUMN TABLE "MOVIELENS"."PAL_PROCEDURE_SIGNATURE" (
  "POSITION"       INT,
  "SCHEMA_NAME"    NVARCHAR(256),
  "TYPE_NAME"      NVARCHAR(256),
  "PARAMETER_TYPE" VARCHAR(7)
);
INSERT INTO "MOVIELENS"."PAL_PROCEDURE_SIGNATURE" VALUES (1, 'MOVIELENS', 'PAL_T_MOVIELENS_RATINGS' , 'IN');
INSERT INTO "MOVIELENS"."PAL_PROCEDURE_SIGNATURE" VALUES (2, 'MOVIELENS', 'PAL_T_OPERATION_CONFIG'  , 'IN');
INSERT INTO "MOVIELENS"."PAL_PROCEDURE_SIGNATURE" VALUES (3, 'MOVIELENS', 'PAL_T_APRIORI_RESULT'    , 'OUT');
INSERT INTO "MOVIELENS"."PAL_PROCEDURE_SIGNATURE" VALUES (4, 'MOVIELENS', 'PAL_T_APRIORI_PMMLMODEL' , 'OUT');
-- --------------------------------------------------------------------------
-- Drop & Create PAL AFL Wrapper function
-- --------------------------------------------------------------------------
CALL "SYS"."AFLLANG_WRAPPER_PROCEDURE_DROP"('MOVIELENS', 'PROC_PAL_APRIORI');
CALL "SYS"."AFLLANG_WRAPPER_PROCEDURE_CREATE"('AFLPAL', 'APRIORIRULE', 'MOVIELENS', 'PROC_PAL_APRIORI', "MOVIELENS"."PAL_PROCEDURE_SIGNATURE");
-- --------------------------------------------------------------------------
-- Create PAL function required input and output tables
-- --------------------------------------------------------------------------
CREATE COLUMN TABLE "MOVIELENS"."PAL_APRIORI_RESULT"   LIKE "MOVIELENS"."PAL_T_APRIORI_RESULT";
CREATE COLUMN TABLE "MOVIELENS"."PAL_OPERATION_CONFIG" LIKE "MOVIELENS"."PAL_T_OPERATION_CONFIG";
INSERT INTO "MOVIELENS"."PAL_OPERATION_CONFIG" VALUES ('MIN_SUPPORT'     , null, 0.1   , null);
INSERT INTO "MOVIELENS"."PAL_OPERATION_CONFIG" VALUES ('MIN_CONFIDENCE'  , null, 0.1   , null);
INSERT INTO "MOVIELENS"."PAL_OPERATION_CONFIG" VALUES ('MAX_CONSEQUENT'  , 1   , null  , null);
INSERT INTO "MOVIELENS"."PAL_OPERATION_CONFIG" VALUES ('MAX_ITEM_LENGTH' , 1   , null  , null);

-- --------------------------------------------------------------------------
-- Call PAL function
-- --------------------------------------------------------------------------
CALL "MOVIELENS"."PROC_PAL_APRIORI" (
    "MOVIELENS"."PAL_APRIORI_MODEL_INPUT"
  , "MOVIELENS"."PAL_OPERATION_CONFIG"
  , "MOVIELENS"."PAL_APRIORI_RESULT"
  , ?
) WITH overview;
```

> ### **Note**
>You may receive a series of errors and warnings in the console log while running the above code. They should all be related to the drop statements at the beginning which are intended to help you re-run the script if needed.

You will notice that the `MIN_SUPPORT` & `MIN_CONFIDENCE` are mandatory attributes to filter out some of the candidate associations.

The value selected here were set relatively high to prevent long running processes on your trial environment. But in a real life scenario, these settings must be determined based on an the initial analysis of the training dataset.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get the collaborative filtering results)]

Now, you can create the view that you will use to extract up to 5 movies per users based on other users with the most similar rating list.

The generated result in the `PAL_APRIORI_RESULT` are "just" the rules between movies, now you need to associate the users with their rated movies as `PRERULE`, and rank the `POSTRULE` using the confidence.

The code of this view was built from scratch:

```SQL
-- --------------------------------------------------------------------------
-- Create the result view
-- --------------------------------------------------------------------------
DROP   VIEW "MOVIELENS"."PAL_APRIORI_MODEL_USERS_RESULTS";
CREATE VIEW "MOVIELENS"."PAL_APRIORI_MODEL_USERS_RESULTS" AS
SELECT *
FROM (
  SELECT
      "T1"."USERID"
    , ROW_NUMBER() OVER(PARTITION BY "T1"."USERID" ORDER BY "T1"."SCORE" DESC, "T1"."CONSEQUENT" DESC ) AS "RANK"
    , "T1"."CONSEQUENT" AS "MOVIEID"
    , "T1"."SCORE"
    , "MOVIES"."TITLE"
    , "MOVIES"."GENRES"
    , "LINKS"."IMDBID"
    , "LINKS"."TMDBID"
  FROM (
    SELECT "INPUT_DATA"."USERID", "RULES"."POSTRULE" AS "CONSEQUENT", MAX("RULES"."CONFIDENCE") AS "SCORE"
    FROM "MOVIELENS"."public.aa.movielens.hdb::data.RATINGS" AS INPUT_DATA
    LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."PAL_APRIORI_RESULT") "RULES" ON (CAST ("INPUT_DATA"."MOVIEID" as VARCHAR(500)) = "RULES"."PRERULE")
    WHERE "RULES"."POSTRULE" IS NOT NULL
    GROUP BY "INPUT_DATA"."USERID", "RULES"."POSTRULE"
  ) "T1"
    LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.hdb::data.MOVIES" "MOVIES" ON "MOVIES"."MOVIEID" = "T1"."CONSEQUENT"
    LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.hdb::data.LINKS"  "LINKS"  ON "LINKS"."MOVIEID"  = "T1"."CONSEQUENT"
) "T1"
WHERE "T1"."RANK" <= 5;
```

Off course, this model is for demonstration purpose and very specific to my initial purpose which is to give you a quick tour of the algorithm and may not be applicable as-is to other use cases or dataset.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Validate the collaborative filtering results)]

In order to be consistent, we should validate the same details that you verified with the SAP HANA APL Recommendation results.

Let's verify how many users will actually get recommendations using the following SQL:

```SQL
SELECT "RECO_COUNT", COUNT(1) AS "USER_COUNT"
FROM (
  SELECT "USERID", MAX(RANK) AS "RECO_COUNT"
  FROM "MOVIELENS"."PAL_APRIORI_MODEL_USERS_RESULTS"
  GROUP BY "USERID"
) GROUP BY "RECO_COUNT" ORDER BY "RECO_COUNT" DESC;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
SELECT
    COUNT(1) AS "MOVIE_COUNT"
  , COUNT(1) *100 / (SELECT COUNT(1) AS "COUNT" FROM "MOVIELENS"."public.aa.movielens.hdb::data.MOVIES" ) AS "MOVIE_RATIO"
FROM (
  SELECT "MOVIEID"
  FROM "MOVIELENS"."PAL_APRIORI_MODEL_USERS_RESULTS"
  GROUP BY "MOVIEID"
);
```

Let's verify how many distinct movies will potentially get recommended to a user (not just the top 5 scores) using the following SQL:

```SQL
SELECT
    COUNT(1) AS "MOVIE_COUNT"
  , COUNT(1) *100 / (SELECT COUNT(1) AS "COUNT" FROM "MOVIELENS"."public.aa.movielens.hdb::data.MOVIES" ) AS "MOVIE_RATIO"
FROM (
  SELECT "PRERULE" AS "MOVIEID"
  FROM "MOVIELENS"."PAL_APRIORI_RESULT"
  WHERE "PRERULE" NOT LIKE '%&%'
  GROUP BY "PRERULE"
);
```

Only 660 of the initial users will receive the requested 5 recommendations. However, only 200 distinct movies (2.2%) in total can be proposed overall and 41 movies (0.45%) in the top 5 lists.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Get the content-based filtering results)]

To address the content-based filtering scenario with the rating dataset, your goal is to provide a list of similar movies based on the number of users who rated the same movies together.

Here, it assumes that the rating action of a single movie by multiple users is a proof of similarity.

For this scenario, you won't actually need to build another model as previous one already provides the links between movies based on user ratings.

Now, you can create the view to extract the results.

The code of this view was built from scratch:

```SQL
-- --------------------------------------------------------------------------
-- Create the result view
-- --------------------------------------------------------------------------
DROP   VIEW "MOVIELENS"."PAL_APRIORI_MODEL_ITEMS_RESULTS";
CREATE VIEW "MOVIELENS"."PAL_APRIORI_MODEL_ITEMS_RESULTS" AS
SELECT *
FROM (
  SELECT
      "T1"."MOVIEID"
    , ROW_NUMBER() OVER(PARTITION BY "T1"."MOVIEID" ORDER BY "T1"."SCORE" DESC, "T1"."CONSEQUENT" DESC ) AS "RANK"
    , "T1"."CONSEQUENT" AS "SIMILAR_MOVIEID"
    , "T1"."SCORE"
    , "MOVIES"."TITLE"
    , "MOVIES"."GENRES"
    , "LINKS"."IMDBID"
    , "LINKS"."TMDBID"
  FROM (
    SELECT "MOVIEID", "RULES"."POSTRULE" AS "CONSEQUENT", "RULES"."CONFIDENCE" AS "SCORE"
    FROM "MOVIELENS"."public.aa.movielens.hdb::data.MOVIES" AS INPUT_DATA
    LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."PAL_APRIORI_RESULT") "RULES" ON (CAST ("INPUT_DATA"."MOVIEID" as VARCHAR(500)) = "RULES"."PRERULE")
    WHERE "RULES"."POSTRULE" IS NOT NULL
  ) "T1"
    LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.hdb::data.MOVIES" "MOVIES" ON "MOVIES"."MOVIEID" = "T1"."CONSEQUENT"
    LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.hdb::data.LINKS"  "LINKS"  ON "LINKS"."MOVIEID"  = "T1"."CONSEQUENT"
) "T1"
WHERE "T1"."RANK" <= 5;
```

Again, this model is for demonstration purpose and very specific to my initial purpose which is to give you a quick tour of the algorithm and may not be applicable as-is to other use cases or dataset.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Validate the content-base filtering results)]

In order to be consistent, we should validate the same details that you verified with the SAP HANA APL Recommendation results.

Let's verify how many movies will actually get recommendations using the following SQL:

```SQL
SELECT "RECO_COUNT", COUNT(1) AS "MOVIE_COUNT"
FROM (
  SELECT "MOVIEID", MAX(RANK) AS "RECO_COUNT"
  FROM "MOVIELENS"."PAL_APRIORI_MODEL_ITEMS_RESULTS"
  GROUP BY "MOVIEID"
) GROUP BY "RECO_COUNT" order by 1 DESC;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
SELECT
    COUNT(1) AS "MOVIE_COUNT"
  , COUNT(1) *100 / (SELECT COUNT(1) AS "COUNT" FROM "MOVIELENS"."public.aa.movielens.hdb::data.MOVIES" ) AS "MOVIE_RATIO"
FROM (
  SELECT "MOVIEID"
  FROM "MOVIELENS"."PAL_APRIORI_MODEL_ITEMS_RESULTS"
  GROUP BY "MOVIEID"
);
```

Only 200 movies (out of the 9,125) will receive at least one recommendations, and only 166 will receive the requested 5 recommendations.

Let's verify how many rating does the movies with no recommendation have using the following SQL:

```SQL
SELECT "RATING_COUNT", COUNT(1) AS "MOVIE_COUNT"
FROM (
  SELECT "RATINGS"."MOVIEID", COUNT(1) as "RATING_COUNT"
  FROM "MOVIELENS"."public.aa.movielens.hdb::data.RATINGS" "RATINGS"
  LEFT OUTER JOIN (
    SELECT "MOVIEID"
    FROM (
      SELECT "PRERULE" AS "MOVIEID"
      FROM "MOVIELENS"."PAL_APRIORI_RESULT"
      WHERE "PRERULE" NOT LIKE '%&%'
      GROUP BY "PRERULE"
    )
  ) "T1" ON ("RATINGS"."MOVIEID" = "T1"."MOVIEID")
  WHERE "T1"."MOVIEID" IS NULL
  GROUP BY "RATINGS"."MOVIEID"
) GROUP BY "RATING_COUNT";
```

As you can see, the movies with no recommendations have up to 92 ratings, and this list include the 3063 movies with only one rating and the 1202 with only 2 ratings.

[DONE]
[ACCORDION-END]

For the purpose of this tutorial series we will not play further with the algorithm, its parameters or the data. However you are more than welcome to do so considering the resources currently made available to you on the SAP Cloud Platform.
