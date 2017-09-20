DESC---
title: SAP HANA APL & the MovieLens dataset
description: Understand the capabilities and options made available with SAP HANA APL and apply them to the data set to build your recommendation engine
auto_validation: true
primary_tag: topic>machine-learning
tags: [  tutorial>beginner, products>sap-hana, products>sap-cloud-platform, topic>machine-learning ]
---

> ### **Note**
>As of the publication date for this tutorial, the SAP Cloud Platform provides SAP HANA MDC instances up to version 1.0 SPS12 and SAP HANA APL version 3.2 which will be use for the rest of the series.
If you are using a different version of SAP HANA and the SAP HANA APL library, the code and the results may differ.
>
For more details about the APL function, check the online <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/3.1/en-US/9bf31268c57e4c079f0cbabd36f39640.html" target="new">documentation</a>.
&nbsp;

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  

- How to use SAP HANA APL Recommendation algorithm from SAP HANA APL version 3.2

### Time to Complete
**20 Min**

[ACCORDION-BEGIN [Info: ](SAP HANA Automated Predictive Library)]

The ***SAP HANA Automated Predictive Library*** (APL) is an ***Application Function Library*** (AFL) which lets you use the data mining capabilities of the ***SAP Predictive Analytics*** automated analytics engine on your SAP HANA stored data.

With the APL, you can create the following types of models to answer your business questions:

- Classification/Regression models
- Clustering models
- Time series analysis models
- Recommendation models
- Social network analysis models

For more details about the SAP HANA APL function, check the online <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/3.1/en-US/9bf31268c57e4c079f0cbabd36f39640.html" target="new">documentation</a>.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Calling SAP HANA APL functions)]

In order to use any ***SAP HANA APL*** functions, ultimately you need to create an AFL wrapper and then invoke this AFL wrapper which is how the APL function is called.

Creating and invoking the AFL wrapper is performed by executing SAP HANA SQL Script statements through a ***SAP HANA `SQLScript`*** (an extension of SQL).

Other database objects also need to be created, such as table types or signature table.

There are two techniques for calling APL functions:

- **The direct technique**:

This technique consists of explicitly generating an AFL wrapper for the APL function to be executed.
The generation of this AFL wrapper requires the explicit creation of table types, signature table, input and output tables, etc.
This is all supposed to be done by the APL consumer, through SQL DDL & DML statements.
Once the AFL wrapper is generated, it can be invoked through a call statement. This "direct technique" is always available.

However, the ***SAP HANA APL*** installation package includes a script where you can import pre-defined table types (used in the code below) which helps reduce the code size.

Here is a quick code example with the direct technique:

```
-- --------------------------------------------------------------------------   
-- Create the table type for the dataset   
-- --------------------------------------------------------------------------   
drop type TRAINING_DATASET_T;   
create type TRAINING_DATASET_T as table ( .... );   

-- --------------------------------------------------------------------------   
-- Create the AFL wrapper corresponding to the target APL function   
-- --------------------------------------------------------------------------   
DROP TABLE CREATE_MODEL_SIGNATURE;   
create column table CREATE_MODEL_SIGNATURE  like PROCEDURE_SIGNATURE_T;   

-- the signature is defined in the APL API documentation   
INSERT INTO CREATE_MODEL_SIGNATURE values (1, 'MYSCHEMA','FUNCTION_HEADER_T'  , 'IN');   
INSERT INTO CREATE_MODEL_SIGNATURE values (2, 'MYSCHEMA','OPERATION_CONFIG_T' , 'IN');   
INSERT INTO CREATE_MODEL_SIGNATURE values (3, 'MYSCHEMA','TRAINING_DATASET_T' , 'IN');   
INSERT INTO CREATE_MODEL_SIGNATURE values (4, 'MYSCHEMA','MODEL_BIN_OID_T'    , 'OUT');   
INSERT INTO CREATE_MODEL_SIGNATURE values (5, 'MYSCHEMA','VARIABLE_DESC_OID_T', 'OUT');   

call SYS.AFLLANG_WRAPPER_PROCEDURE_DROP('MYSCHEMA','APLWRAPPER_CREATE_MODEL');   
call SYS.AFLLANG_WRAPPER_PROCEDURE_CREATE('APL_AREA','CREATE_MODEL','MYSCHEMA', 'APLWRAPPER_CREATE_MODEL', CREATE_MODEL_SIGNATURE);   

-- --------------------------------------------------------------------------   
-- Create the input/output tables used as arguments for the APL function   
-- --------------------------------------------------------------------------   
DROP TABLE FUNCTION_HEADER;   
CREATE COLUMN TABLE FUNCTION_HEADER LIKE FUNCTION_HEADER_T;   
INSERT INTO FUNCTION_HEADER values ('key', 'value');   

DROP TABLE OPERATION_CONFIG;   
CREATE COLUMN TABLE OPERATION_CONFIG LIKE OPERATION_CONFIG_T;   
INSERT INTO OPERATION_CONFIG values ('key', 'value');   

DROP TABLE TRAINED_MODEL;   
CREATE COLUMN TABLE TRAINED_MODEL LIKE MODEL_BIN_OID_T;   

DROP TABLE VARIABLE_DESC;   
CREATE COLUMN TABLE VARIABLE_DESC LIKE VARIABLE_DESC_OID_T;   

-- --------------------------------------------------------------------------   
-- Execute the APL function using its AFL wrapper and the actual input/output tables
-- --------------------------------------------------------------------------
call APLWRAPPER_CREATE_MODEL(FUNCTION_HEADER, OPERATION_CONFIG, MYSCHEMA.TRAINING_DATASET, TRAINED_MODEL, VARIABLE_DESC) with overview;
```    

- **The procedure technique**:

This technique is not only much simpler than the direct technique, but it's also more efficient and scalable.
Instead of having to deal with the life cycle of the AFL wrappers and all its companion database objects on a per-call basis, the APL user can directly call APL specific stored procedures which take care of all the AFL details.

These APL stored procedures are part of the `HCO_PA_APL` delivery unit which is automatically deployed when installing **SAP HANA APL**.

Here is a quick code example with the procedure technique:

```
SET SESSION 'APL_CACHE_SCHEMA' = 'APL_CACHE';  

-- --------------------------------------------------------------------------   
-- Create the input/output tables used as arguments for the APL function   
-- --------------------------------------------------------------------------   
DROP TABLE FUNCTION_HEADER;   
CREATE COLUMN TABLE FUNCTION_HEADER LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.FUNCTION_HEADER";
INSERT INTO FUNCTION_HEADER values ('key', 'value');   

DROP TABLE OPERATION_CONFIG;   
CREATE COLUMN TABLE OPERATION_CONFIG LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.OPERATION_CONFIG_DETAILED";   
INSERT INTO OPERATION_CONFIG values ('key', 'value');   

DROP TABLE TRAINED_MODEL;   
CREATE COLUMN TABLE TRAINED_MODEL LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.MODEL_BIN_OID";   

DROP TABLE VARIABLE_DESC;   
CREATE COLUMN TABLE VARIABLE_DESC LIKE  "SAP_PA_APL"."sap.pa.apl.base::BASE.T.VARIABLE_DESC_OID";   

-- --------------------------------------------------------------------------   
-- Execute the APL function using its AFL wrapper and the actual input/output tables
-- --------------------------------------------------------------------------
call "SAP_PA_APL"."sap.pa.apl.base::CREATE_MODEL"(FUNCTION_HEADER, OPERATION_CONFIG, 'MYSCHEMA','TRAINING_DATASET', TRAINED_MODEL, VARIABLE_DESC) with overview;  
```    

We will use the procedure technique in this tutorial.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Recommendation engines with SAP HANA APL)]

As stated above, the ***SAP HANA Automated Predictive Library*** (APL) uses the data mining capabilities provided by the ***SAP Predictive Analytics*** automated analytics engine.

SAP HANA APL provides a ***Recommendation*** function that can address both the collaborative filtering and content-based filtering scenarios.

Using "classical" classification models is also a potential option but would require first a different dataset structure, but also building as many models as items (movies) to address the collaborative filtering scenario and an even larger number of models for the content-based filtering scenario (movie count square).

In the rating dataset, we have about 100,000 ratings with 671 distinct users and more than 9,000 distinct movies.

This is why the SAP HANA APL ***Recommendation*** algorithm is probably the most appropriate here.

This algorithm uses a *social network* analysis approach to translate your transactional data in the form of a graph, made of nodes and links.

The nodes are the actors/items within a network (individuals, customers, products, organizations…). The links are the relations, or social interactions, between them (transactions, visits, clicks, calls).

These links can be directed or undirected depending on the type of relation, symmetric or not. So in its simplest form, a social network is a map of all the relevant links between the nodes being studied.

Social networks can be used to represent many kinds of networks: informational (web, blogs), communicational (phone calls, emails), social (social networking, illness), technological (power grid, roads, internet router), financial (transactions), etc.

For more information please refer to the online <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/3.1/en-US/2ee67eddf0fb47b3a593887fdfa555df.html" target="new">documentation</a>.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Switch to the Catalog perspective)]

In order to create the CDS artifacts, we will be using the **Catalog** perspective available in the **SAP HANA Web-based Development Workbench**.

From the ***SAP HANA Web-based Development Workbench*** main panel, click on **Catalog**:

![SAP HANA Web-based Development Workbench](01.png)

Else, if you are already accessing one of the perspective, then use the ![plus](0-navigation.png) icon from the menu:

![SAP HANA Web-based Development Workbench](02.png)

> ### **Note**
>**Make sure the currently connected user is TRIAL and not SYSTEM**. Check the upper right corner of the SAP HANA Web-based Development Workbench.

For each of the next steps, you can decide to open a new **SQL Console** using the ![sql](0-opensqlconsole.png) icon from the menu or reuse the same one by replacing its current over and over.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run Recommendation APL function)]

While assessing the available data, you found out that the ratings was the best candidate to build our collaborative filtering an content-based filter scenarios.

The SAP HANA APL function that you will be using is

- <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/3.1/en-US/0bc196486e4047c2a7671ccf529167b6.html" target="new"><b>`CREATE_RECO_MODEL_AND_TRAIN`</b></a>

For more information please refer to the online <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/3.1/en-US/2ee67eddf0fb47b3a593887fdfa555df.html" target="new">documentation</a>.

The ***SAP HANA APL Recommendation*** function provides multiple configuration options like:

- **max top nodes**: prevent additional links to be loaded when a threshold is reached of a node
- **best sellers**: identifies nodes with too many links and exclude them from the results unless explicitly requested
- **minimum support**: the minimum number of times a pair of items are linked to the same user to create a rule (default value is 2)
- **minimum confidence**: the percentage of times a rule between 2 items (a movie being rated by 2 or more users) was found in the total set of transactions (default value is 5%)
- **minimum predictive power**: the minimum quality indicator for a candidate rules
- **weight column**: allows to apply a strength in the transaction
- **weight rule**: either Support (the number of links found for each node) or the Independence Ratio (2 events are independent if the probability that both events occur equals the probability of event A times the probability of event B. 1 indicates completely independent events)

By default, the function will identify **mega-hubs** (using the 4 "sigma" rule), identify **best sellers**, and apply pre & post filters to address your needs.

> ### **Note**:
>In this scenario, we are not considering the rating notation itself (between 0.5 to 5) to build the output list, which would help a list of movies that both users rated the same way. To achieve that, we would need to transform the data structure, and use the movie as one node (entity type) and the user associated with the rating notation as the second node (entity type), then use the same algorithm. And finally instead of using the user as the entry point we would use the user and the rating notation as the entry point.
>
If you want to try out this scenario, you can build a view where the user id and the rating are concatenated into one column that will be used as the second entity type.

&nbsp;

When executed, the following code will generate a ***Recommendation*** model linking `USERID` & `MOVIEID` from the `RATINGS` CDS Entity.

The results will be stored in the `APL_MODEL_USERS_LINKS` table under the `movielens` schema.

Open a new **SQL Console** using the ![sql](0-opensqlconsole.png) icon from the menu or reuse an existing one.

Paste the following content in the console, and use the execute icon ![run](0-run.png) from the menu.

```SQL
SET SCHEMA "MOVIELENS";

SET SESSION 'APL_CACHE_SCHEMA' = 'TRIAL';
-- --------------------------------------------------------------------------
-- Cleanup SAPL objects
-- --------------------------------------------------------------------------
call "SAP_PA_APL"."sap.pa.apl.base::CLEANUP"(1,?);
-- --------------------------------------------------------------------------
-- Drop generic tables
-- --------------------------------------------------------------------------
DROP TABLE "MOVIELENS"."APL_FUNCTION_HEADER";
DROP TABLE "MOVIELENS"."APL_OPERATION_CONFIG";
DROP TABLE "MOVIELENS"."APL_VARIABLE_DESC";
DROP TABLE "MOVIELENS"."APL_OPERATION_LOG";
DROP TABLE "MOVIELENS"."APL_SUMMARY";
DROP TABLE "MOVIELENS"."APL_INDICATORS";
DROP TABLE "MOVIELENS"."APL_OPERATION_RESULT";
-- --------------------------------------------------------------------------
-- Drop model specific tables
-- --------------------------------------------------------------------------
DROP TABLE "MOVIELENS"."APL_MODEL_USERS";
DROP TABLE "MOVIELENS"."APL_MODEL_USERS_NODE_USERS";
DROP TABLE "MOVIELENS"."APL_MODEL_USERS_NODE_ITEMS";
DROP TABLE "MOVIELENS"."APL_MODEL_USERS_LINKS";
-- --------------------------------------------------------------------------
-- Create generic tables
-- --------------------------------------------------------------------------
CREATE COLUMN TABLE "MOVIELENS"."APL_FUNCTION_HEADER"   LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.FUNCTION_HEADER";
CREATE COLUMN TABLE "MOVIELENS"."APL_OPERATION_CONFIG"  LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.OPERATION_CONFIG_DETAILED";
CREATE COLUMN TABLE "MOVIELENS"."APL_VARIABLE_DESC"     LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.VARIABLE_DESC_OID";
CREATE COLUMN TABLE "MOVIELENS"."APL_OPERATION_LOG"     LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.OPERATION_LOG";
CREATE COLUMN TABLE "MOVIELENS"."APL_SUMMARY"           LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.SUMMARY";
CREATE COLUMN TABLE "MOVIELENS"."APL_INDICATORS"        LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.INDICATORS";
CREATE COLUMN TABLE "MOVIELENS"."APL_OPERATION_RESULT"  LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.RESULT";
-- --------------------------------------------------------------------------
-- Create model tables
-- --------------------------------------------------------------------------
CREATE COLUMN TABLE "MOVIELENS"."APL_MODEL_USERS"       LIKE "SAP_PA_APL"."sap.pa.apl.base::BASE.T.MODEL_NATIVE";
CREATE COLUMN TABLE "MOVIELENS"."APL_MODEL_USERS_LINKS" (
    "GRAPH_NAME"        NVARCHAR(255),
    "WEIGHT"            DOUBLE,
    "KXNODEFIRST"       INTEGER,    -- must be of the same SQL type as the User column (USERID here)
    "KXNODESECOND"      INTEGER,    -- must be of the same SQL type as the Item column (MOVIEID here)
    "KXNODESECOND_2"    INTEGER     -- must be of the same SQL type as the Item column (MOVIEID here)
);
CREATE COLUMN TABLE "MOVIELENS"."APL_MODEL_USERS_NODE_USERS" (
    "node" INTEGER    -- must be of the same SQL type as the User column (USERID here)
);
CREATE COLUMN TABLE "MOVIELENS"."APL_MODEL_USERS_NODE_ITEMS" (
    "node" INTEGER    -- must be of the same SQL type as the Item column (MOVIEID here)
);
-- --------------------------------------------------------------------------
-- Insert config
-- --------------------------------------------------------------------------
INSERT INTO "MOVIELENS"."APL_FUNCTION_HEADER" values ('Oid', '#42');
INSERT INTO "MOVIELENS"."APL_FUNCTION_HEADER" values ('LogLevel', '8');

INSERT INTO "MOVIELENS"."APL_OPERATION_CONFIG" values ('APL/ModelType'  , 'recommendation'  ,null);
INSERT INTO "MOVIELENS"."APL_OPERATION_CONFIG" values ('APL/User'       , 'USERID'          ,null); -- mandatory
INSERT INTO "MOVIELENS"."APL_OPERATION_CONFIG" values ('APL/Item'       , 'MOVIEID'         ,null); -- mandatory
INSERT INTO "MOVIELENS"."APL_OPERATION_CONFIG" values ('APL/RuleWeight' , 'Support'         ,null);
-- --------------------------------------------------------------------------
-- Execute the APL function to train the model
-- --------------------------------------------------------------------------
CALL "SAP_PA_APL"."sap.pa.apl.base::CREATE_RECO_MODEL_AND_TRAIN" (
    "MOVIELENS"."APL_FUNCTION_HEADER"
  , "MOVIELENS"."APL_OPERATION_CONFIG"
  , "MOVIELENS"."APL_VARIABLE_DESC"
  , 'MOVIELENS', 'public.aa.movielens.cds::data.RATINGS'
  , "MOVIELENS"."APL_MODEL_USERS"
  , 'MOVIELENS', 'APL_MODEL_USERS_NODE_USERS'
  , 'MOVIELENS', 'APL_MODEL_USERS_NODE_ITEMS'
  , 'MOVIELENS', 'APL_MODEL_USERS_LINKS'
  , "MOVIELENS"."APL_OPERATION_LOG"
  , "MOVIELENS"."APL_SUMMARY"
  , "MOVIELENS"."APL_INDICATORS"
  , "MOVIELENS"."APL_OPERATION_RESULT"
) with overview;
-- --------------------------------------------------------------------------
-- Cleanup SAPL objects
-- --------------------------------------------------------------------------
CALL "SAP_PA_APL"."sap.pa.apl.base::CLEANUP"(1,?);
```

> ### **Note**
>You may receive a series of errors and warnings in the console log while running the above code. They should all be related to the drop statements at the beginning which are intended to help you re-run the script if needed.

&nbsp;

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Check the logs and summary)]

For every function calls, a series of logs and summary data will be provided along with the results.

- The operation log:

When performing an APL operation, especially training or applying a model, the Automated Analytics engine produces status/warning/error messages.

These messages are returned from an APL function through an output database table.

```sql
select * from "MOVIELENS"."APL_OPERATION_LOG";
```

- The summary:

When training or applying a model, debriefing information related to the operation is produced.

This is known as the summary. This information is a set of indicators, provided as string pairs { KEY, VALUE }.

```sql
select * from "MOVIELENS"."APL_SUMMARY";
```

- The indicators:

When training, testing or querying a model, it's possible to retrieve variable indicators (i.e. variable statistics).

For each variable, a collection of indicators may be retrieved. These indicators are described using the following attributes: { variable name, indicator name, indicator value, indicator detail (when applicable) }.

Indicators are returned from an APL function through an output database table. The output table contains estimator indicators for regression models, to help plotting the regression curve.

Even if this output is not applicable for a recommendation mode, here is the SQL to check the output:

```sql
select * from "MOVIELENS"."APL_INDICATORS";
```

- The operation result log:

When performing some of the APL operation, a result might be returned in the operation result table. In the recommendation scenario, the returned result is the SQL to extract results from the links table.

```sql
select * from "MOVIELENS"."APL_OPERATION_RESULT";
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Get the collaborative filtering results)]

From this model, you can now create a view that you will use to extract a list of up to 5 recommended movies per users based on other users with the most similar rating list.

The following code, that you will use to create your result view, was actually generated by the APL function itself as text (in the operation result log, so just I re-formatted it and adjusted some parts to ease the lecture):

```SQL
-- --------------------------------------------------------------------------
-- Create the result view
-- --------------------------------------------------------------------------
DROP   VIEW "MOVIELENS"."APL_MODEL_USERS_RESULTS";
CREATE VIEW "MOVIELENS"."APL_MODEL_USERS_RESULTS" AS
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
      SELECT
          "T1"."USERID"
        , "T1"."CONSEQUENT"
        , max("T1"."SUPPORT"/"T1"."COUNTANTECEDENT") AS "SCORE"
      FROM (
        SELECT
            "T1"."USERID"
          , "T1"."ANTECEDENT"
          , "T1"."CONSEQUENT"
          , "T1"."SUPPORT"
          , (COALESCE ("T2_1"."COUNTANTECEDENT",0) + COALESCE ("T2_2"."COUNTANTECEDENT",0) ) AS "COUNTANTECEDENT"
        FROM (
            SELECT
                "T1"."USERID"
              , "T1"."ANTECEDENT"
              , "T1"."CONSEQUENT"
              , "T1"."WEIGHT" AS "SUPPORT"
            FROM (
              SELECT
                  "SPACEIN"."USERID"
                , "RULES"."KXNODESECOND"   AS "ANTECEDENT"
                , "RULES"."KXNODESECOND_2" AS "CONSEQUENT"
                , "RULES"."WEIGHT"
              FROM
                "MOVIELENS"."public.aa.movielens.cds::data.RATINGS" "SPACEIN"
              LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Transactions') "PRODUCTS" ON ("PRODUCTS"."KXNODEFIRST"  = "SPACEIN"."USERID")
              LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item'        ) "RULES"    ON ("PRODUCTS"."KXNODESECOND" = "RULES"."KXNODESECOND")
              LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Transactions') "NOTIN"    ON ("RULES"."KXNODESECOND_2"  = "NOTIN"."KXNODESECOND") AND ("NOTIN"."KXNODEFIRST" = "SPACEIN"."USERID")
                WHERE "RULES"."KXNODESECOND" IS NOT NULL  AND "NOTIN"."KXNODESECOND" IS NULL
            ) "T1" where userid in(12,  120)
            UNION ALL
            SELECT
                "T1"."USERID"
              , "T1"."ANTECEDENT"
              , "T1"."CONSEQUENT"
              , "T1"."WEIGHT" AS "SUPPORT"
            FROM (
              SELECT
                  "SPACEIN"."USERID"
                , "RULES"."KXNODESECOND_2" AS "ANTECEDENT"
                , "RULES"."KXNODESECOND"   AS "CONSEQUENT"
                , "RULES"."WEIGHT"
              FROM
                "MOVIELENS"."public.aa.movielens.cds::data.RATINGS" "SPACEIN"
              LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Transactions') "PRODUCTS" ON ("PRODUCTS"."KXNODEFIRST"  = "SPACEIN"."USERID")
              LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item'        ) "RULES"    ON ("PRODUCTS"."KXNODESECOND" = "RULES"."KXNODESECOND_2")
              LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Transactions') "NOTIN"    ON ("RULES"."KXNODESECOND"    = "NOTIN"."KXNODESECOND") AND ("NOTIN"."KXNODEFIRST" = "SPACEIN"."USERID")
              WHERE "RULES"."KXNODESECOND_2" IS NOT NULL AND "NOTIN"."KXNODESECOND" IS NULL
            ) "T1"
        ) "T1"
        LEFT OUTER JOIN (SELECT "KXNODESECOND"   AS "ANTECEDENT", CAST(COUNT(*) AS FLOAT) AS "COUNTANTECEDENT" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" ='Transactions' GROUP BY "KXNODESECOND"  ) "T2_1" ON ("T1"."ANTECEDENT" = "T2_1"."ANTECEDENT")
        LEFT OUTER JOIN (SELECT "KXNODESECOND_2" AS "ANTECEDENT", CAST(COUNT(*) AS FLOAT) AS "COUNTANTECEDENT" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" ='Transactions' GROUP BY "KXNODESECOND_2") "T2_2" ON ("T1"."ANTECEDENT" = "T2_2"."ANTECEDENT")
      ) "T1" GROUP BY "T1"."USERID",  "T1"."CONSEQUENT"
  ) "T1"
  LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.cds::data.MOVIES" "MOVIES" ON "MOVIES"."MOVIEID" = "T1"."CONSEQUENT"
  LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.cds::data.LINKS"  "LINKS"  ON "LINKS"."MOVIEID"  = "T1"."CONSEQUENT"
) "T1"
WHERE "T1"."RANK" <= 5;
```

As you can notice the view use both the model generated links (`APL_MODEL_USERS_LINKS`) and the initial dataset (`public.aa.movielens.cds::data.RATINGS`).

Off course, this model is for demonstration purpose and very specific to my initial purpose which is to give you a quick tour of the algorithm and may not be applicable as-is to other use cases or dataset.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Validate the collaborative filtering results)]

Let's verify how many users will actually get recommendations using the following SQL:

```SQL
SELECT "RECO_COUNT", COUNT(1) AS "USER_COUNT"
FROM (
  SELECT "USERID", MAX(RANK) AS "RECO_COUNT"
  FROM "MOVIELENS"."APL_MODEL_USERS_RESULTS"
  GROUP BY "USERID"
) GROUP BY "RECO_COUNT" order by 1 DESC;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
SELECT
    COUNT(1) AS "MOVIE_COUNT"
  , COUNT(1) *100 / (SELECT COUNT(1) AS "COUNT" FROM "MOVIELENS"."public.aa.movielens.cds::data.MOVIES" ) AS "MOVIE_RATIO"
FROM (
  SELECT "MOVIEID"
  FROM "MOVIELENS"."APL_MODEL_USERS_RESULTS"
  GROUP BY "MOVIEID"
);
```

Let's verify how many distinct movies will potentially get recommended to a user (not just the top 5 scores) using the following SQL:

```SQL
SELECT
    COUNT(1) AS "MOVIE_COUNT"
  , COUNT(1) *100 / (SELECT COUNT(1) AS "COUNT" FROM "MOVIELENS"."public.aa.movielens.cds::data.MOVIES" ) AS "MOVIE_RATIO"
FROM (
    SELECT "MOVIEID"
    FROM (
      SELECT "KXNODESECOND"   AS "MOVIEID" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item' GROUP BY  "KXNODESECOND"
      UNION ALL
      SELECT "KXNODESECOND_2" AS "MOVIEID" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item' GROUP BY  "KXNODESECOND_2"
    ) GROUP BY "MOVIEID"
);
```

All users will receive the requested 5 recommendations. However, only about 13% of the movies (1176 out of the 9,125) will get potentially recommended and only about 1.5% of the movies (149 out of the 9,125) are in the top 5 lists.

&nbsp;

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Get the content-based filtering results)]

To address the content-based filtering scenario with the rating dataset, your goal is to provide a list of similar movies based on the number of users who rated the same movies together.

Here, it assumes that the rating action of a single movie by multiple users is a proof of similarity.

For this scenario, you won't actually need to build another model as previous one already provides the links between movies based on user ratings.

Now, you can create the view to extract the results.

The code of this view was also generated by the APL function itself as text (so I re-formatted it and adjusted some parts):

```SQL
-- --------------------------------------------------------------------------
-- Create the result view
-- --------------------------------------------------------------------------
DROP   VIEW "MOVIELENS"."APL_MODEL_ITEMS_RESULTS";
CREATE VIEW "MOVIELENS"."APL_MODEL_ITEMS_RESULTS" AS
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
      SELECT
          "T1"."MOVIEID"
        , "T1"."CONSEQUENT"
        , MAX("T1"."SUPPORT"/"T1"."COUNTANTECEDENT") AS "SCORE"
      FROM (
        SELECT
            "T1"."MOVIEID"
          , "T1"."CONSEQUENT"
          , "T1"."SUPPORT"
          , (COALESCE ("T2_1"."COUNTANTECEDENT",0) + COALESCE ("T2_2"."COUNTANTECEDENT",0) ) AS "COUNTANTECEDENT"
        FROM (
            SELECT
                "NODES"."node" AS "MOVIEID"
              , "RULES"."KXNODESECOND"   AS "ANTECEDENT"
              , "RULES"."KXNODESECOND_2" AS "CONSEQUENT"
              , "RULES"."WEIGHT" AS "SUPPORT"
            FROM
              "MOVIELENS"."APL_MODEL_USERS_NODE_ITEMS" "NODES"
            LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item' ) "RULES"    ON ("NODES"."node" = "RULES"."KXNODESECOND")
              WHERE "RULES"."KXNODESECOND_2" IS NOT NULL
            UNION ALL
            SELECT
                "NODES"."node" AS "MOVIEID"
              , "RULES"."KXNODESECOND_2" AS "ANTECEDENT"
              , "RULES"."KXNODESECOND"   AS "CONSEQUENT"
              , "RULES"."WEIGHT" AS "SUPPORT"
            FROM
              "MOVIELENS"."APL_MODEL_USERS_NODE_ITEMS" "NODES"
            LEFT OUTER JOIN (SELECT * FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item' ) "RULES"    ON ("NODES"."node" = "RULES"."KXNODESECOND_2")
            WHERE "RULES"."KXNODESECOND" IS NOT NULL
        ) "T1"
        LEFT OUTER JOIN (SELECT "KXNODESECOND"   AS "ANTECEDENT", CAST(COUNT(*) AS FLOAT) AS "COUNTANTECEDENT" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" ='Transactions' GROUP BY "KXNODESECOND"  ) "T2_1" ON ("T1"."ANTECEDENT" = "T2_1"."ANTECEDENT")
        LEFT OUTER JOIN (SELECT "KXNODESECOND_2" AS "ANTECEDENT", CAST(COUNT(*) AS FLOAT) AS "COUNTANTECEDENT" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" ='Transactions' GROUP BY "KXNODESECOND_2") "T2_2" ON ("T1"."ANTECEDENT" = "T2_2"."ANTECEDENT")
      ) "T1" GROUP BY "T1"."MOVIEID", "T1"."CONSEQUENT"
  ) "T1"
  LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.cds::data.MOVIES" "MOVIES" ON "MOVIES"."MOVIEID" = "T1"."CONSEQUENT"
  LEFT OUTER JOIN "MOVIELENS"."public.aa.movielens.cds::data.LINKS"  "LINKS"  ON "LINKS"."MOVIEID"  = "T1"."CONSEQUENT"
) "T1"
WHERE "T1"."RANK" <= 5;
```

Again, this model is for demonstration purpose and very specific to my initial purpose which is to give you a quick tour of the algorithm and may not be applicable as-is to other use cases or dataset.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Validate the content-base filtering results)]

Let's verify how many movies will actually get recommendations using the following SQL:

```SQL
SELECT "RECO_COUNT", COUNT(1) AS "MOVIE_COUNT"
FROM (
  SELECT "MOVIEID", MAX(RANK) AS "RECO_COUNT"
  FROM "MOVIELENS"."APL_MODEL_ITEMS_RESULTS"
  GROUP BY "MOVIEID"
) GROUP BY "RECO_COUNT";
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
SELECT
    COUNT(1) AS "MOVIE_COUNT"
  , COUNT(1) *100 / (SELECT COUNT(1) AS "COUNT" FROM "MOVIELENS"."public.aa.movielens.cds::data.MOVIES" ) AS "MOVIE_RATIO"  
FROM (
  SELECT "MOVIEID"
  FROM "MOVIELENS"."APL_MODEL_ITEMS_RESULTS"
  GROUP BY "MOVIEID"
);
```

Only 1050 will receive the requested 5 recommendations out of the 1176 movies that will receive at least one recommendations.

Let's verify how many rating does the movies with no recommendation have using the following SQL:

```SQL
SELECT "RATING_COUNT", COUNT(1) AS "MOVIE_COUNT"
FROM (
  SELECT "RATINGS"."MOVIEID", COUNT(1) as "RATING_COUNT"
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS" "RATINGS"
  LEFT OUTER JOIN (
    SELECT "MOVIEID"
    FROM (
        SELECT "MOVIEID"
        FROM (
          SELECT "KXNODESECOND"   AS "MOVIEID" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item' GROUP BY  "KXNODESECOND"
          UNION ALL
          SELECT "KXNODESECOND_2" AS "MOVIEID" FROM "MOVIELENS"."APL_MODEL_USERS_LINKS" WHERE "GRAPH_NAME" = 'Item' GROUP BY  "KXNODESECOND_2"
        ) GROUP BY "MOVIEID"
    )
  ) "T1" ON ("RATINGS"."MOVIEID" = "T1"."MOVIEID")
  WHERE "T1"."MOVIEID" IS NULL
  GROUP BY "RATINGS"."MOVIEID"
) GROUP BY "RATING_COUNT";
```

As you can see, the movies with no recommendations have all less than 27 ratings, and this list include the 3063 movies with only one rating and the 1202 with only 2 ratings.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

For the purpose of this tutorial series we will not play further with the algorithm, its parameters or the data. However you are more than welcome to do so considering the resources currently made available to you on the SAP Cloud Platform.
