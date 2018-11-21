---
title: MovieLens with SAP HANA APL Recommendation (MovieLens SQL)
description: Understand the capabilities and options made available with the SAP HANA Automated Predictive Library (APL), which algorithm can be used to address your goal, and apply it to the data set
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 30
---

## Prerequisites
 - [Use Machine Learning to Build a Movie Recommendation model using SQL](https://www.sap.com/developer/groups/hxe-aa-movielens-sql.html)

## Next Steps
 - [Use Machine Learning to Build a Movie Recommendation model using SQL](https://www.sap.com/developer/groups/hxe-aa-movielens-sql.html)

## Details
### You will learn
- Understand the basics about the SAP HANA Automated Predictive Library
- How to call SAP HANA Automated Predictive Library functions from SQL
- Identify which algorithm options are available for recommendation engines

[ACCORDION-BEGIN [Info: ](SAP HANA Automated Predictive Library)]

The ***SAP HANA Automated Predictive Library*** (APL) is an ***Application Function Library*** (AFL) which lets you use the data mining capabilities of the ***SAP Predictive Analytics*** automated analytics engine on your SAP HANA stored data.

With the APL, you can create the following types of models to answer your business questions:

- Classification/Regression models
- Clustering models
- Time series analysis models
- Recommendation models
- Social network analysis models

For more details about the SAP HANA APL function, check the online <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/latest/en-US/59b79cbb6beb4607875fa3fe116a8eef.html" target="new">documentation</a>.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Calling APL functions)]

In order to use any ***SAP HANA APL*** functions, ultimately an AFL wrapper must be created and then invoked.

For more details, you can check the [AFL Language Procedures](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/latest/en-US/7f630904dfe045beb114a6c25896649f.html) documentation.

Creating and invoking the AFL wrapper is performed by executing ***SAP HANA `SQLScript`***.

Other database objects also need to be created, such as table types or signature table.

There are two techniques for calling APL functions, the ***direct technique*** and the ***procedure technique***.

### **The direct technique**:

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

### **The procedure technique**:

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

We will use the **procedure technique** in this tutorial.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](Recommendation engines with SAP HANA APL)]

As stated previously, the ***SAP HANA Automated Predictive Library*** (APL) uses the data mining capabilities provided by the ***SAP Predictive Analytics*** automated analytics engine.

SAP HANA APL provides a ***Recommendation*** function that can address both the collaborative filtering and content-based filtering scenarios.

Using ***classical*** classification models is also a potential option but would require first a different dataset structure, but also building as many models as items (movies) to address the collaborative filtering scenario and an even larger number of models for the content-based filtering scenario (movie count square).

In the rating dataset, we have about 100,000 ratings with 671 distinct users and more than 9,000 distinct movies.

This is why the ***SAP HANA APL Recommendation*** algorithm is probably the most appropriate here.

This algorithm uses a ***link analysis*** approach to translate your transactional data in the form of a graph, made of nodes and links.

The nodes are the actors/items within a network (individuals, customers, products, organizations…). The links are the relations, or social interactions, between them (Transactions, visits, clicks, calls).

These links can be directed or undirected depending on the type of relation, symmetric or not. So in its simplest form, a social network is a map of all the relevant links between the nodes being studied.

Social networks can be used to represent many kinds of networks: informational (web, blogs), communicational (phone calls, emails), social (social networking, illness), technological (power grid, roads, internet router), financial (Transactions), etc.

For more information please refer to the online <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/latest/en-US/2ee67eddf0fb47b3a593887fdfa555df.html" target="new">documentation</a>.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](SAP HANA APL Recommendation function)]

The **SAP HANA APL** function that you will be using is:

- <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/latest/en-US/0bc196486e4047c2a7671ccf529167b6.html" target="new"><b>Create Recommendation Model and Train</b></a>

The ***Recommendation*** function provides multiple configuration options like:

Name                            | Description
--------------------------------|------------------------------
**max top nodes**               | Prevent additional links to be loaded when a threshold is reached on a node
**best sellers**                | Identifies nodes with too many links and exclude them from the results unless explicitly requested
**minimum support**             | The minimum number of times a pair of items are linked to the same user to create a rule (default value is 2)
**minimum confidence**          | The minimum percentage of times a rule between 2 items (a movie being rated by 2 or more users) was found in the total set of Transactions (default value is 5%)
**minimum predictive power**    | The minimum quality indicator for a candidate rules
**weight column**               | Allows to apply a strength in the transaction
**weight rule**                 | Either Support (the number of links found for each node) or the Independence Ratio (2 events are independent if the probability that both events occur equals the probability of event A times the probability of event B. 1 indicates completely independent events)

By default, the function will identify **mega-hubs** (using the 4 ***sigma*** rule), identify **best sellers**, and apply pre & post filters to address your needs.

> ### **Note**:
>In this scenario, we are not considering the rating notation itself (between 0.5 to 5) to build the output list, which would help a list of movies that both users rated the same way.
>&nbsp;
>To achieve that, we would need to transform the data structure, and use the movie as one node (entity type) and the user associated with the rating notation as the second node (entity type), then use the same algorithm.
>&nbsp;
>And finally instead of using the user as the entry point we would use the user and the rating notation as the entry point.
>&nbsp;
>If you want to try out this scenario, you can build a view where the user id and the rating are concatenated into one column that will be used as the second entity type.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Pre-requisite: ](Install SAP HANA APL package)]

The installation requires you to have access to the system using a SSH client like ***`PuTTY`***, but also to have access to the ***`hxeadm`*** user with ***`sudo`*** rights configured.

To run the download manager you will need Java t be installed on the system.

The installation will trigger a restart of your SAP HANA instance, so make sure to save your current work before.

Once the SAP HANA Automated Predictive Library installation is completed, you will need to wait a few minutes for all services to be back online and proceed with the next step.

So if not done yet, you will need to complete the [SAP HANA Automated Predictive Library installation for SAP HANA, express edition](https://www.sap.com/developer/tutorials/hxe-ua-apl-binary.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Add the APL Role to your User)]

To execute APL functions, you need add the **`APL_EXECUTE`** role to your user.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following SQL statements.

```sql
call _SYS_REPO.GRANT_ACTIVATED_ROLE ('sap.pa.apl.base.roles::APL_EXECUTE','ML_USER');
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run Recommendation APL function)]

While assessing the available data, you found out that the ratings was the best candidate to build our collaborative filtering an content-based filter scenarios.

The SAP HANA APL function that you will be using is

- <a href="https://help.sap.com/viewer/cb31bd99d09747089754a0ba75067ed2/3.1/en-US/0bc196486e4047c2a7671ccf529167b6.html" target="new"><b>Create Recommendation Model and Train</b></a>

When executed, the following code will generate a ***Recommendation*** model linking `USERID` & `MOVIEID` from the `RATINGS` table.

The results will be stored in multiple tables:

 - **`APL_MOVIELENS_RECO_MODEL`** : the model definition
 - **`APL_MOVIELENS_RECO_MODEL_NODE_USERS`** : the list of user found while building the model
 - **`APL_MOVIELENS_RECO_MODEL_NODE_ITEMS`** : the list of items (movies) found while building the model
 - **`APL_MOVIELENS_RECO_MODEL_LINKS`** : the rules set linking users to movies and movies to movies

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following series of SQL statements.

### **Clean previous results:**

```SQL
-- --------------------------------------------------------------------------
-- cleanup sapl objects
-- --------------------------------------------------------------------------
call sap_pa_apl."sap.pa.apl.base::CLEANUP"(1,?);
-- --------------------------------------------------------------------------
-- drop function in/out tables, helper tables and views
-- --------------------------------------------------------------------------
drop table apl_movielens_function_header;
drop table apl_movielens_parameters;
drop table apl_movielens_variable_desc;
drop table apl_movielens_operation_log;
drop table apl_movielens_summary;
drop table apl_movielens_indicators;
drop table apl_movielens_operation_result;
drop table apl_movielens_model;
drop table apl_movielens_model_node_users;
drop table apl_movielens_model_node_items;
drop table apl_movielens_model_links;
```

> ### **Note**
>You may receive a series of errors and warnings in the console log while running the above code. They should all be related to the drop statements at the beginning which are intended to help you re-run the script if needed.

### **Create input and output table structures:**

```SQL
-- --------------------------------------------------------------------------
-- create generic tables using pre-built table types
-- --------------------------------------------------------------------------
create column table apl_movielens_function_header   like sap_pa_apl."sap.pa.apl.base::BASE.T.FUNCTION_HEADER";
create column table apl_movielens_parameters        like sap_pa_apl."sap.pa.apl.base::BASE.T.OPERATION_CONFIG_EXTENDED";
create column table apl_movielens_variable_desc     like sap_pa_apl."sap.pa.apl.base::BASE.T.VARIABLE_DESC_OID";
create column table apl_movielens_operation_log     like sap_pa_apl."sap.pa.apl.base::BASE.T.OPERATION_LOG";
create column table apl_movielens_summary           like sap_pa_apl."sap.pa.apl.base::BASE.T.SUMMARY";
create column table apl_movielens_indicators        like sap_pa_apl."sap.pa.apl.base::BASE.T.INDICATORS";
create column table apl_movielens_operation_result  like sap_pa_apl."sap.pa.apl.base::BASE.T.RESULT";
create column table apl_movielens_model             like sap_pa_apl."sap.pa.apl.base::BASE.T.MODEL_NATIVE";
-- --------------------------------------------------------------------------
-- create model tables
-- --------------------------------------------------------------------------
create column table apl_movielens_model_node_users (
    node integer    -- must be of the same sql type as the user column (userid here)
);
create column table apl_movielens_model_node_items (
    node integer    -- must be of the same sql type as the item column (movieid here)
);
create column table apl_movielens_model_links (
    graph_name        nvarchar(255),
    weight            double,
    kxnodefirst       integer,    -- must be of the same sql type as the user column (userid here)
    kxnodesecond      integer,    -- must be of the same sql type as the item column (movieid here)
    kxnodesecond_2    integer     -- must be of the same sql type as the item column (movieid here)
);
```

### **Set the algorithm parameters:**

```SQL
-- --------------------------------------------------------------------------
-- configuration
-- --------------------------------------------------------------------------
truncate table apl_movielens_function_header;
insert into apl_movielens_function_header values ('Oid', '#1');
insert into apl_movielens_function_header values ('LogLevel', '8');

truncate table apl_movielens_parameters;
insert into apl_movielens_parameters values ('APL/ModelType'              , 'recommendation'  , null);
insert into apl_movielens_parameters values ('APL/User'                   , 'USERID'          , null); -- mandatory
insert into apl_movielens_parameters values ('APL/Item'                   , 'MOVIEID'         , null); -- mandatory
insert into apl_movielens_parameters values ('APL/RuleWeight'             , 'Support'         , null); -- default is Independence Probability
insert into apl_movielens_parameters values ('APL/BestSeller'             , '50000'           , null); -- default is 50000
insert into apl_movielens_parameters values ('APL/MaxTopNodes'            , '100000'          , null); -- default is 100000
insert into apl_movielens_parameters values ('APL/MinimumConfidence'      , '0.05'            , null); -- default is 0.05
insert into apl_movielens_parameters values ('APL/MinimumPredictivePower' , '0.0'             , null); -- default is 0.0
insert into apl_movielens_parameters values ('APL/MinimumSupport'         , '2'               , null); -- default is 2
insert into apl_movielens_parameters values ('APL/Top'                    , '5'               , null); -- default is max
insert into apl_movielens_parameters values ('APL/IncludeBestSellers'     , 'false'           , null); -- default is false

select * from apl_movielens_parameters;
```

### **Run the algorithm:**

```SQL
-- --------------------------------------------------------------------------
-- Clean result tables
-- --------------------------------------------------------------------------
truncate table apl_movielens_model;
truncate table apl_movielens_model_node_users;
truncate table apl_movielens_model_node_items;
truncate table apl_movielens_model_links;
-- --------------------------------------------------------------------------
-- execute the apl function to train the model
-- --------------------------------------------------------------------------
call sap_pa_apl."sap.pa.apl.base::CREATE_RECO_MODEL_AND_TRAIN" (
    apl_movielens_function_header
  , apl_movielens_parameters
  , apl_movielens_variable_desc
  , current_schema, 'MOVIELENS_RATINGS'
  , apl_movielens_model
  , current_schema, 'APL_MOVIELENS_MODEL_NODE_USERS'
  , current_schema, 'APL_MOVIELENS_MODEL_NODE_ITEMS'
  , current_schema, 'APL_MOVIELENS_MODEL_LINKS'
  , apl_movielens_operation_log
  , apl_movielens_summary
  , apl_movielens_indicators
  , apl_movielens_operation_result
) with overview;
```

### **Check the logs and summary:**

#### **The operation log:**

When performing an APL operation, especially training or applying a model, the Automated Analytics engine produces status/warning/error messages.

These messages are returned from an APL function through an output database table.

```sql
select * from apl_movielens_operation_log;
```

#### **The summary:**

When training or applying a model, debriefing information related to the operation is produced.

This is known as the summary. This information is a set of indicators, provided as string pairs { KEY, VALUE }.

```sql
select * from apl_movielens_summary;
```

#### **The indicators:**

When training, testing or querying a model, it's possible to retrieve variable indicators (i.e. variable statistics).

For each variable, a collection of indicators may be retrieved. These indicators are described using the following attributes: { variable name, indicator name, indicator value, indicator detail (when applicable) }.

Indicators are returned from an APL function through an output database table. The output table contains estimator indicators for regression models, to help plotting the regression curve.

Even if this output is not applicable for a recommendation mode, here is the SQL to check the output:

```sql
select * from apl_movielens_indicators;
```

#### **The operation result log:**

When performing some of the APL operation, a result might be returned in the operation result table. In the recommendation scenario, the returned result is the SQL to extract results from the links table.

```sql
select * from apl_movielens_operation_result;
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the results views)]

From this model, you can now create a view that you will use to extract a list of up to 5 recommended movies per users based on other users with the most similar rating list.

Connect to the **HXE** tenant using the **`ML_USER`** user credentials and execute the following series of SQL statements.

As a reminder, the model used the ***Support*** as a result weight/score metric, that will be transformed into a ***confidence*** metric in the result view.

### **Collaborative filtering results**

The following code, that you will use to create your result view, was actually generated by the APL function itself as text (in the operation result log, so just I re-formatted it and adjusted some parts to ease the lecture):

As you will notice the view use both the model generated links (`APL_MOVIELENS_MODEL_LINKS`) and the initial dataset (`MOVIELENS_RATINGS`).

Off course, this model is for demonstration purpose and very specific to my initial purpose which is to give you a quick tour of the algorithm and may not be applicable as-is to other use cases or dataset.

```SQL
drop   view apl_movielens_collaborative_filtering;
create view apl_movielens_collaborative_filtering as
select
  userid, rank, t1.movieid, score, title, genres, imdbid, tmdbid
from (
  select
      t1.userid
    , row_number() over(partition by t1.userid order by t1.score desc, t1.consequent desc ) as rank
    , t1.consequent as movieid
    , t1.score      as score
  from (
      select
          t1.userid, t1.consequent, max(t1.score)  as score
      from (
        select
            t1.userid
          , t1.consequent
          , t1.support / ( (coalesce (t2_1.count_antecedent,0) + coalesce (t2_2.count_antecedent,0) ) )  as score -- confidence calculation
        from (
            select
                t1.userid
              , t1.antecedent , t1.consequent
              , t1.support
            from (
              select
                  spacein.userid
                , rules.kxnodesecond   as antecedent
                , rules.kxnodesecond_2 as consequent
                , rules.weight         as support
              from movielens_ratings spacein
              left outer join (select * from apl_movielens_model_links where graph_name = 'Transactions') products on (products.kxnodefirst  = spacein.userid)
              left outer join (select * from apl_movielens_model_links where graph_name = 'Item'        ) rules    on (products.kxnodesecond = rules.kxnodesecond)
              left outer join (select * from apl_movielens_model_links where graph_name = 'Transactions') notin    on (rules.kxnodesecond_2  = notin.kxnodesecond) and (notin.kxnodefirst = spacein.userid)
                where rules.kxnodesecond is not null  and notin.kxnodesecond is null
            ) t1
            union all
            select
                t1.userid
              , t1.antecedent , t1.consequent
              , t1.support
            from (
              select
                  spacein.userid
                , rules.kxnodesecond_2 as antecedent
                , rules.kxnodesecond   as consequent
                , rules.weight         as support
              from movielens_ratings spacein
              left outer join (select * from apl_movielens_model_links where graph_name = 'Transactions') products on (products.kxnodefirst  = spacein.userid)
              left outer join (select * from apl_movielens_model_links where graph_name = 'Item'        ) rules    on (products.kxnodesecond = rules.kxnodesecond_2)
              left outer join (select * from apl_movielens_model_links where graph_name = 'Transactions') notin    on (rules.kxnodesecond    = notin.kxnodesecond) and (notin.kxnodefirst = spacein.userid)
              where rules.kxnodesecond_2 is not null and notin.kxnodesecond is null
            ) t1
        ) t1
        left outer join (select kxnodesecond   as antecedent, cast(count(*) as float) as count_antecedent from apl_movielens_model_links where graph_name ='Transactions' group by kxnodesecond  ) t2_1 on (t1.antecedent = t2_1.antecedent)
        left outer join (select kxnodesecond_2 as antecedent, cast(count(*) as float) as count_antecedent from apl_movielens_model_links where graph_name ='Transactions' group by kxnodesecond_2) t2_2 on (t1.antecedent = t2_2.antecedent)
      ) t1 group by t1.userid,  t1.consequent
  ) t1
) t1
left outer join movielens_movies movies on movies.movieid = t1.movieid
left outer join movielens_links  links  on links.movieid  = t1.movieid
where rank <= 5;
```

### **Content-based filtering results**

To address the content-based filtering scenario with the rating dataset, your goal is to provide a list of similar movies based on the number of users who rated the same movies together.

Here, it assumes that the rating action of a single movie by multiple users is a proof of similarity.

For this scenario, you won't actually need to build another model as previous one already provides the links between movies based on user ratings.

Now, you can create the view to extract the results.

The code of this view was also generated by the APL function itself as text (so I re-formatted it and adjusted some parts):

```SQL
drop   view apl_movielens_contentbased_filtering;
create view apl_movielens_contentbased_filtering as
select
  t1.movieid, rank, similar_movie, score, title, genres, imdbid, tmdbid
from (
  select
      t1.movieid
    , row_number() over(partition by t1.movieid order by t1.score desc, t1.consequent desc ) as rank
    , t1.consequent as similar_movie
    , t1.score
  from (
      select
          t1.movieid
        , t1.consequent
        , max(t1.score) as score
      from (
        select
            t1.movieid
          , t1.consequent
          , t1.support / (coalesce (t2_1.count_antecedent,0) + coalesce (t2_2.count_antecedent,0) ) as score
        from (
            select
                nodes.node as movieid
              , rules.kxnodesecond   as antecedent
              , rules.kxnodesecond_2 as consequent
              , rules.weight as support
            from
              apl_movielens_model_node_items nodes
            left outer join (select * from apl_movielens_model_links where graph_name = 'Item' ) rules    on (nodes.node = rules.kxnodesecond)
              where rules.kxnodesecond_2 is not null
            union all
            select
                nodes.node as movieid
              , rules.kxnodesecond_2 as antecedent
              , rules.kxnodesecond   as consequent
              , rules.weight as support
            from
              apl_movielens_model_node_items nodes
            left outer join (select * from apl_movielens_model_links where graph_name = 'Item' ) rules    on (nodes.node = rules.kxnodesecond_2)
            where rules.kxnodesecond is not null
        ) t1
        left outer join (select kxnodesecond   as antecedent, cast(count(*) as float) as count_antecedent from apl_movielens_model_links where graph_name ='Transactions' group by kxnodesecond  ) t2_1 on (t1.antecedent = t2_1.antecedent)
        left outer join (select kxnodesecond_2 as antecedent, cast(count(*) as float) as count_antecedent from apl_movielens_model_links where graph_name ='Transactions' group by kxnodesecond_2) t2_2 on (t1.antecedent = t2_2.antecedent)
      ) t1 group by t1.movieid, t1.consequent
  ) t1
) t1
left outer join movielens_movies movies on movies.movieid = t1.similar_movie
left outer join movielens_links  links  on links.movieid  = t1.similar_movie
where rank <= 5;
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
  from apl_movielens_collaborative_filtering
  group by userid
) group by reco_count order by 1 desc;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
select
    count(1) as movie_count
  , count(1) * 100 / (select count(1) as cnt from movielens_movies) as movie_ratio
from (
  select movieid
  from apl_movielens_collaborative_filtering
  group by movieid
);
```

Let's verify how many distinct movies will potentially get recommended to a user (not just the top 5 scores) using the following SQL:

```SQL
select
    count(1) as movie_count
  , count(1) * 100 / (select count(1) as cnt from movielens_movies) as movie_ratio
from (
    select movieid
    from (
      select kxnodesecond  as movieid from apl_movielens_model_links where graph_name = 'Item' group by  kxnodesecond
      union all
      select kxnodesecond_2 as movieid from apl_movielens_model_links where graph_name = 'Item' group by  kxnodesecond_2
    ) group by movieid
);
```

Based on the last result, you can conclude that:

 - all 671 users will receive the requested 5 recommendations
 - only about 2% of the movies (181 out of the 9,125) are in the top 5 lists
 - only about 13% of the movies (1176 out of the 9,125) will get potentially recommended

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Validate the content-based filtering results)]

Let's verify how many movies will actually get recommendations using the following SQL:

```SQL
select reco_count, count(1) as movie_count
from (
  select movieid, max(rank) as reco_count
  from apl_movielens_contentbased_filtering
  group by movieid
) group by reco_count;
```

Let's verify how many distinct movies will actually get recommended to a user (part of the top 5 scores) using the following SQL:

```SQL
select
    count(1) as movie_count
  , count(1) * 100 / (select count(1) as cnt from movielens_movies ) as movie_ratio
from (
  select movieid
  from apl_movielens_contentbased_filtering
  group by movieid
);
```

Only 1050 movies will receive the requested 5 recommendations out of the 1176 movies that will receive at least one recommendations.

Let's verify how many rating does the movies with no recommendation have using the following SQL:

```SQL
select rating_count, count(1) as movie_count
from (
  select ratings.movieid, count(1) as rating_count
  from movielens_ratings ratings
  left outer join (
    select movieid
    from (
      select movieid
      from (
        select kxnodesecond  as movieid from apl_movielens_model_links where graph_name = 'Item' group by  kxnodesecond
        union all
        select kxnodesecond_2 as movieid from apl_movielens_model_links where graph_name = 'Item' group by  kxnodesecond_2
      ) group by movieid
    )
  ) t1 on (ratings.movieid = t1.movieid)
  where t1.movieid is null
  group by ratings.movieid
) group by rating_count;
```

As you can see, the movies with no recommendations have all less than 24 ratings, and this list include the 3063 movies with only one rating and the 1202 with only 2 ratings.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

> ### **Note** If you are using Jupyter Notebook, you can download the following  [notebook](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hxe-aa-movielens-sql-04/hxe-aa-movielens-sql-04.ipynb) to run most of the SQL statement listed in the tutorial.
You can follow the [Use Jupyter Notebook with SAP HANA, express edition](https://www.sap.com/developer/tutorials/mlb-hxe-tools-jupyter.html)) tutorial for more details.

For the purpose of this tutorial series we will not play further with the algorithm, its parameters or the data. However you are more than welcome to do so considering the resources currently made available to you on the SAP Cloud Platform.
