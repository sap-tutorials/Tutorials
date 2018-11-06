---
title: Create additional database artifacts (MovieLens App)
description: Create a series of SQL Views that will be used in a SAPUI5 application
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 20
---

## Prerequisites
 - [Use Machine Learning to Build a Movie Recommendation application using the XS advanced development model](https://www.sap.com/developer/groups/hxe-aa-movielens.html)

## Next Steps
 - [Use Machine Learning to Build a Movie Recommendation application using the XS advanced development model](https://www.sap.com/developer/groups/hxe-aa-movielens.html)

## Details
### You will learn
- Create a HDB SQL View Artifact
- Create a HDB Procedure Artifact

[ACCORDION-BEGIN [Step 1: ](Open the Web IDE)]

Open the Web IDE, and login using the **`XSA_DEV`** credentials.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

![Web IDE](01-01.png)

As a reminder the default URL for the Web IDE is:

 - `https://hxehost:53075`

A link to the Web IDE can also be found on the ***XSA Controller page*** at:

- `https://hxehost:39030`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Rating User Summary view)]

When building your SAPUI5 application, an additional set of views, inspired by the series of SQL used to validate our SAP HANA APL & PAL results, will be needed.

This set of views will help you provide a better application user experience.

First, you will create a view that present a ***user rating summary*** which includes the number of ratings, the average notation etc. for each user.

In the left side panel, expand the **`movielens/db/src/hdb`** tree node.

Right click on the **`hdb`** folder and select **New > Folder**.

Enter **`summary`** as the folder name, then click on **OK**.

Right click on the **`summary`** folder node from the tree, and select **New > File**.

Enter **`ratings_user.hdbview`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/summary/ratings_user.hdbview
```

Paste the following content:

```SQL
view "aa.movielens.db.hdb.summary::ratings_user" as
select distinct
    userid
  , 'user id: ' || userid || ' - rating count: ' || count(1) over( partition by userid ) as description
  , count(1)               over( partition by userid )                                   as rating_count
  , avg(rating)            over( partition by userid )                                   as rating_avg
  , nth_value(timestamp,1) over( partition by userid order by timestamp desc, movieid)   as last_rating_date
  , nth_value(rating  ,1)  over( partition by userid order by timestamp desc, movieid)   as last_rating
  , nth_value(movieid  ,1) over( partition by userid order by timestamp desc, movieid)   as last_movieid
from "aa.movielens.db.hdb::data.ratings";
```

Save the file using the ![save](00-save.png) icon from the menu.

Right click on **`ratings_user.hdbview`** and select **Build Selected Files**.

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
select
  'summary::ratings_user' as "view name",
  count(1) as "row count"
from  "aa.movielens.db.hdb.summary::ratings_user";
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a Rating Movie Summary view)]

Then, you will create a view that present a ***movie rating summary*** which includes the number of ratings, the average notation etc. for each movie.

Right click on the **`summary`** folder node from the tree, and select **New > File**.

Enter **`ratings_movie.hdbview`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/summary/ratings_movie.hdbview
```

Paste the following content:

```SQL
view "aa.movielens.db.hdb.summary::ratings_movie" as
select distinct
      t1.movieid
    , 'movie id: ' || t1.movieid || ' - rating count: ' || count(1) over( partition by t1.movieid ) as description
    , t2.title
    , t2.genres
    , t3.imdbid
    , t3.tmdbid
    , count(1)                over( partition by t1.movieid ) as rating_count
    , avg(rating)             over( partition by t1.movieid ) as rating_avg
    , nth_value(timestamp, 1) over( partition by t1.movieid order by t1.timestamp desc, t1.movieid) as last_rating_date
    , nth_value(rating   , 1) over( partition by t1.movieid order by t1.timestamp desc, t1.movieid) as last_rating
    , nth_value(userid   , 1) over( partition by t1.movieid order by t1.timestamp desc, t1.movieid) as last_userid
from "aa.movielens.db.hdb::data.ratings" t1
left outer join "aa.movielens.db.hdb::data.movies" t2 on (t1.movieid = t2.movieid)
left outer join "aa.movielens.db.hdb::data.links"  t3 on (t1.movieid = t3.movieid);
```

Save the file using the ![save](00-save.png) icon from the menu.

The path of the file you have just created is **`movielens/db/src/hdb/summary/ratings_movie.hdbview`**.

Right click on **`ratings_movie.hdbview`**  and select **Build Selected Files**.

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
select
  'summary::ratings_movie' as "view name",
  count(1) as "row count"
from "aa.movielens.db.hdb.summary::ratings_movie"
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Rating Details view)]

And finally, you will create a view that present the ***rating details*** which joins the ratings along with the movie and links details.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

Right click on the **`summary`** folder node from the tree, and select **New > File**.

Enter **`ratings_detailed.hdbview`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/summary/ratings_detailed.hdbview
```

Paste the following content:

```SQL
view "aa.movielens.db.hdb.summary::ratings_detailed" as
select
      t1.movieid
    , t1.userid
    , 'user id: ' || t1.userid || ' - user rating: ' || t1.rating || ' - movie id: ' || t1.movieid || ' - title: ' || t2.title as description
    , t2.title
    , t2.genres
    , t3.imdbid
    , t3.tmdbid
    , t1.rating
    , t1.timestamp
from "aa.movielens.db.hdb::data.ratings" t1
left outer join "aa.movielens.db.hdb::data.movies" t2 on (t1.movieid = t2.movieid)
left outer join "aa.movielens.db.hdb::data.links"  t3 on (t1.movieid = t3.movieid);
```

Save the file using the ![save](00-save.png) icon from the menu.

The path of the file you have just created is **`movielens/db/src/hdb/summary/ratings_detailed.hdbview`**.

Right click on **`ratings_detailed.hdbview`**  and select **Build Selected Files**.

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
select
  'summary::ratings_detailed' as "view name",
  count(1) as "row count"
from "aa.movielens.db.hdb.summary::ratings_detailed";
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create the APL procedures)]

In order to expose in your application the ability to execute the APL Recommendation algorithm and get the results, you will need create a series of stored procedure, that you will later expose as XSJS services.

In this procedure, you will expose a series of parameters from the APL Recommendation function:

- Best Seller Threshold
- Max Top Nodes
- Minimum Confidence
- Minimum Predictive Power
- Minimum Support

Certain parameters are not included either because they are not compatible with the recommendation model type used here, or because they don't influence the generated rules but just the generated SQL to consume the rules.

The complete description of these parameters is available in the [`CREATE_RECO_MODEL_AND_TRAIN` function](https://help.sap.com/viewer/4055990955524bb2bc61ee75de3b08ff/latest/en-US/0bc196486e4047c2a7671ccf529167b6.html) documentation.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

Right click on the **`apl`** folder (from **`movielens/db/src/hdb`** ) and select **New > Folder**.

Enter **`procedures`** as the folder name, then click on **OK**.

#### The execution procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`recommendation_execute.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/apl/procedures/recommendation_execute.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.movielens.db.hdb.apl.procedures::recommendation_execute" (
   in BestSellerThreshold    integer default 50000,
   in MaxTopNodes            integer default 100000,
   in MinimumConfidence      double  default 0.05,
   in MinimumPredictivePower double  default null,
   in MinimumSupport         integer default 2,
   out operation_log  "aa.movielens.db.hdb.apl::recommendation.tt_operation_log",
   out summary        "aa.movielens.db.hdb.apl::recommendation.tt_summary",
   out indicators     "aa.movielens.db.hdb.apl::recommendation.tt_indicators",
   out model_sql_code "aa.movielens.db.hdb.apl::recommendation.tt_model_sql_code"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    -- Insert operation parameters
    truncate table "aa.movielens.db.hdb.apl::recommendation.function_header";
    function_header = select * from "aa.movielens.db.hdb.apl::recommendation.function_header";                                 

    truncate table "aa.movielens.db.hdb.apl::recommendation.operation_config";
    if :BestSellerThreshold     is not null then insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/BestSeller'             , cast(:BestSellerThreshold    as varchar));   end if;
    if :MaxTopNodes             is not null then insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/MaxTopNodes'            , cast(:MaxTopNodes            as varchar));   end if;
    if :MinimumConfidence       is not null then insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/MinimumConfidence'      , cast(:MinimumConfidence      as varchar));   end if;
    if :MinimumPredictivePower  is not null then insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/MinimumPredictivePower' , cast(:MinimumPredictivePower as varchar));   end if;
    if :MinimumSupport          is not null then insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/MinimumSupport'         , cast(:MinimumSupport         as varchar));   end if;
    insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/ModelType'  , 'recommendation'  );
    insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/User'       , 'USERID'          ); -- mandatory
    insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/Item'       , 'MOVIEID'         ); -- mandatory
    insert into "aa.movielens.db.hdb.apl::recommendation.operation_config" values ('APL/RuleWeight' , 'Support'         );
    operation_config = select * from "aa.movielens.db.hdb.apl::recommendation.operation_config";       

    truncate table "aa.movielens.db.hdb.apl::recommendation.variable_descs";
    variable_descs = select * from "aa.movielens.db.hdb.apl::recommendation.variable_descs";                                   

    movielens_dataset = select * from "aa.movielens.db.hdb::data.ratings";           
    call "aa.movielens.db.hdb.apl.afllang::recommendation"(
        :function_header,
        :operation_config,
        :variable_descs,
        :movielens_dataset,
        :model,
        :model_node_user,
        :model_node_movie,
        :model_links,
        :operation_log,
        :summary,
        :indicators,
        :model_sql_code
    );
    -- Clear tables content
    truncate table "aa.movielens.db.hdb.apl::recommendation.model";
    truncate table "aa.movielens.db.hdb.apl::recommendation.model_node_user";
    truncate table "aa.movielens.db.hdb.apl::recommendation.model_node_movie";
    truncate table "aa.movielens.db.hdb.apl::recommendation.model_links";
    truncate table "aa.movielens.db.hdb.apl::recommendation.operation_log";
    truncate table "aa.movielens.db.hdb.apl::recommendation.summary";
    truncate table "aa.movielens.db.hdb.apl::recommendation.indicators";    
    truncate table "aa.movielens.db.hdb.apl::recommendation.model_sql_code";

    -- Insert the results
    insert into "aa.movielens.db.hdb.apl::recommendation.model"            select * from :model;
    insert into "aa.movielens.db.hdb.apl::recommendation.model_node_user"  select * from :model_node_user;
    insert into "aa.movielens.db.hdb.apl::recommendation.model_node_movie" select * from :model_node_movie;
    insert into "aa.movielens.db.hdb.apl::recommendation.model_links"      select * from :model_links;
    insert into "aa.movielens.db.hdb.apl::recommendation.operation_log"    select * from :operation_log;
    insert into "aa.movielens.db.hdb.apl::recommendation.summary"          select * from :summary;
    insert into "aa.movielens.db.hdb.apl::recommendation.indicators"       select * from :indicators;        
    insert into "aa.movielens.db.hdb.apl::recommendation.model_sql_code"   select * from :model_sql_code;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

#### The collaborative filtering result procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`recommendation_result_collaborative.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/apl/procedures/recommendation_result_collaborative.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.movielens.db.hdb.apl.procedures::recommendation_result_collaborative" (
    in UserId              integer default -1
   ,in IncludeBestSeller   integer default 0
   ,in BestSellerThreshold integer default 50000
   ,in SkipAlreadyOwned    integer default 1
   ,in KeepTopN            integer default 5
   ,out results            "aa.movielens.db.hdb.apl::recommendation.tt_movielens_collaborative_result"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    results = select
    userid, rank, t1.movieid, score, title, genres, imdbid, tmdbid
  from (
    select
        t1.userid
    , t1.consequent as movieid, t1.score as score
    , row_number() over(partition by t1.userid order by t1.score desc, t1.consequent desc ) as rank
    from (
        select
            t1.userid, t1.consequent, max(t1.score)  as score
        from (
          select
                t1.userid, t1.consequent
              , t1.support / ( (coalesce (t2_1.count_antecedent,0) + coalesce (t2_2.count_antecedent,0) ) ) as score -- confidence calculation
          from (
              select
                  t1.userid, t1.antecedent , t1.consequent, t1.support
              from (
                select
                    spacein.userid
                  , rules.kxnodesecond   as antecedent
                  , rules.kxnodesecond_2 as consequent
                  , rules.weight         as support
                from "aa.movielens.db.hdb::data.ratings" spacein
                left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Transactions') products on (products.kxnodefirst  = spacein.userid)
                left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Item'        ) rules    on (products.kxnodesecond = rules.kxnodesecond)
                left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Transactions') notin    on (rules.kxnodesecond_2  = notin.kxnodesecond) and (notin.kxnodefirst = spacein.userid) and (:SkipAlreadyOwned = 1)
                  where rules.kxnodesecond is not null
                  and   spacein.userid = :UserId
                  and   notin.kxnodesecond is null
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
                from "aa.movielens.db.hdb::data.ratings" spacein
                left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Transactions') products on (products.kxnodefirst  = spacein.userid)
                left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Item'        ) rules    on (products.kxnodesecond = rules.kxnodesecond_2)
                left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Transactions') notin    on (rules.kxnodesecond    = notin.kxnodesecond) and (notin.kxnodefirst = spacein.userid) and (:SkipAlreadyOwned = 1)
                where rules.kxnodesecond_2 is not null
                and   spacein.userid = :UserId
                and notin.kxnodesecond is null
              ) t1
          ) t1
          left outer join (select kxnodesecond   as antecedent, cast(count(*) as float) as count_antecedent from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name ='Transactions' group by kxnodesecond  ) t2_1 on (t1.antecedent = t2_1.antecedent)
          left outer join (select kxnodesecond_2 as antecedent, cast(count(*) as float) as count_antecedent from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name ='Transactions' group by kxnodesecond_2) t2_2 on (t1.antecedent = t2_2.antecedent)
          union all
          select :UserId as userid, movieid, count(1) from "aa.movielens.db.hdb::data.ratings" spacein
          left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Transactions') notin    on (spacein.movieid    = notin.kxnodesecond) and (notin.kxnodefirst = spacein.userid) and :SkipAlreadyOwned = 1
          where :IncludeBestSeller = 1 group by movieid having count(1) > BestSellerThreshold
        ) t1 group by t1.userid,  t1.consequent
    ) t1
  ) t1
  left outer join "aa.movielens.db.hdb::data.movies" movies on movies.movieid = t1.movieid
  left outer join "aa.movielens.db.hdb::data.links"  links  on links.movieid  = t1.movieid
  where rank <= :KeepTopN;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

#### The content based filtering result procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`recommendation_result_contentbased.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/apl/procedures/recommendation_result_contentbased.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.movielens.db.hdb.apl.procedures::recommendation_result_contentbased" (
    in MovieId             integer default -1
   ,in IncludeBestSeller   integer default 0
   ,in BestSellerThreshold integer default 50000
   ,in KeepTopN            integer default 5
   ,out results          "aa.movielens.db.hdb.apl::recommendation.tt_movielens_contentbased_result"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    results = select
    t1.movieid, rank, similar_movie, score, title, genres, imdbid, tmdbid
  from (
    select
        t1.movieid
      , t1.consequent as similar_movie , t1.score
      , row_number() over(partition by t1.movieid order by t1.score desc, t1.consequent desc ) as rank
    from (
        select
            t1.movieid, t1.consequent
          , max(t1.score) as score
        from (
          select
              t1.movieid, t1.consequent
            , t1.support / (coalesce (t2_1.count_antecedent,0) + coalesce (t2_2.count_antecedent,0) ) as score
          from (
              select
                  nodes.node as movieid
                , rules.kxnodesecond   as antecedent
                , rules.kxnodesecond_2 as consequent
                , rules.weight as support
              from
                "aa.movielens.db.hdb.apl::recommendation.model_node_movie" nodes
              left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Item' ) rules    on (nodes.node = rules.kxnodesecond)
              where rules.kxnodesecond_2 is not null
              and   nodes.node = :MovieId
              union all
              select
                  nodes.node as movieid
                , rules.kxnodesecond_2 as antecedent
                , rules.kxnodesecond   as consequent
                , rules.weight as support
              from
                "aa.movielens.db.hdb.apl::recommendation.model_node_movie" nodes
              left outer join (select * from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name = 'Item' ) rules    on (nodes.node = rules.kxnodesecond_2)
              where rules.kxnodesecond is not null
              and   nodes.node = :MovieId
          ) t1
          left outer join (select kxnodesecond   as antecedent, cast(count(*) as float) as count_antecedent from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name ='Transactions' group by kxnodesecond  ) t2_1 on (t1.antecedent = t2_1.antecedent)
          left outer join (select kxnodesecond_2 as antecedent, cast(count(*) as float) as count_antecedent from "aa.movielens.db.hdb.apl::recommendation.model_links" where graph_name ='Transactions' group by kxnodesecond_2) t2_2 on (t1.antecedent = t2_2.antecedent)
          union all
          select :MovieId as movieid, movieid as consequent, count(1) from "aa.movielens.db.hdb::data.ratings" nodes
          where :IncludeBestSeller = 1 and movieid != :MovieId group by movieid having count(1) > BestSellerThreshold
        ) t1 group by t1.movieid, t1.consequent
    ) t1
  ) t1
  left outer join "aa.movielens.db.hdb::data.movies" movies on movies.movieid = t1.similar_movie
  left outer join "aa.movielens.db.hdb::data.links"  links  on links.movieid  = t1.similar_movie
  where rank <= :KeepTopN;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

Using the CTRL button on the keyboard, select the following files:

- **`recommendation_execute.hdbprocedure`**
- **`recommendation_result_collaborative.hdbprocedure`**
- **`recommendation_result_contentbased.hdbprocedure`**

Then, right click the selection, then use the **Build Selected Files** menu.

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check the APL procedure (1/3))]

You can now check how many users will get recommendation for the collaborative filtering approach.

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
  call "aa.movielens.db.hdb.apl.procedures::recommendation_execute"(
    BESTSELLERTHRESHOLD    => 50000,
    MAXTOPNODES            => 100000,
    MINIMUMCONFIDENCE      => 0.05,
    MINIMUMPREDICTIVEPOWER => null,
    MINIMUMSUPPORT         => 2,
    OPERATION_LOG   => :OPERATION_LOG,
    SUMMARY         => :SUMMARY,
    INDICATORS      => :INDICATORS,
    MODEL_SQL_CODE  => :MODEL_SQL_CODE
  );

  select 'distinct users included in the model'  as key, count(1) as value from "aa.movielens.db.hdb.apl::recommendation.model_node_user"
  union all
  select 'distinct movies included in the model' as key, count(1) as value from "aa.movielens.db.hdb.apl::recommendation.model_node_movie";
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Check the APL procedure (2/3))]

You can now check how the collaborative filtering results using the created stored procedure.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
  call "aa.movielens.db.hdb.apl.procedures::recommendation_result_collaborative"(
    USERID              => 32,
    INCLUDEBESTSELLER   => 0,
    BESTSELLERTHRESHOLD => 50000,
    SKIPALREADYOWNED    => 1,
    KEEPTOPN            => 5,
    RESULTS             => :results
  );
  select * from :results;
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Check the APL procedure (3/3))]

You can now check how the content based filtering results using the created stored procedure.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
  call "aa.movielens.db.hdb.apl.procedures::recommendation_result_contentbased"(
    MOVIEID             => 32,
    INCLUDEBESTSELLER   => 0,
    BESTSELLERTHRESHOLD => 50000,
    KEEPTOPN            => 5,
    RESULTS             => :results
  );
  select * from :results;
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_6]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Create the PAL APRIORI procedure)]

In order to expose in your application the ability to execute the PAL APRIORI algorithm, you will need create a stored procedure, that you will later expose in a XSJS service.

In this procedure, you will expose a series of parameters from the PAL APRIORI function:

  - Minimum Support
  - Minimum Confidence
  - Minimum Lift
  - Ubiquitous

Certain parameters are not included because they are not compatible with the recommendation model type used here, or because they would require a more ~~complex~~ sophisticated implementation.

The complete description of these parameters is available in the [APRIORI function](https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/latest/en-US/7a073d66173a4c1589ef5fbe5bb3120f.html) documentation.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

Right click on the **`pal`** folder (from **`movielens/db/src/hdb`** ) and select **New > Folder**.

Enter **`procedures`** as the folder name, then click on **OK**.

#### The execution procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`apriori_execute.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/pal/procedures/apriori_execute.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.movielens.db.hdb.pal.procedures::apriori_execute" (
   in min_support    double default 0.1,
   in min_confidence double default 0.1,
   in min_lift       double default 0.0,
   in ubiquitous     double default 1.0,
   out rules   "aa.movielens.db.hdb.pal::apriori.rules",
   out pmml    "aa.movielens.db.hdb.pal::apriori.pmml"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    -- Insert operation parameters
    truncate table "aa.movielens.db.hdb.pal::apriori.parameter";
    if :min_support     is not null then insert into "aa.movielens.db.hdb.pal::apriori.parameter" VALUES ('MIN_SUPPORT'     , null, :min_support    , null);    end if;
    if :min_confidence  is not null then insert into "aa.movielens.db.hdb.pal::apriori.parameter" VALUES ('MIN_CONFIDENCE'  , null, :min_confidence , null);    end if;
    if :min_lift        is not null then insert into "aa.movielens.db.hdb.pal::apriori.parameter" VALUES ('MIN_LIFT'        , null, :min_lift       , null);    end if;
    if :ubiquitous      is not null then insert into "aa.movielens.db.hdb.pal::apriori.parameter" VALUES ('UBIQUITOUS'      , null, :ubiquitous     , null);    end if;
    insert into "aa.movielens.db.hdb.pal::apriori.parameter" VALUES ('MAX_CONSEQUENT'  , 1   , null  , null);
    insert into "aa.movielens.db.hdb.pal::apriori.parameter" VALUES ('MAX_ITEM_LENGTH' , 1   , null  , null);
    parameter = select * from "aa.movielens.db.hdb.pal::apriori.parameter";                                 

    movielens_dataset = select USERID, MOVIEID from "aa.movielens.db.hdb::data.ratings";
    call "aa.movielens.db.hdb.pal.afllang::apriori"(
        :movielens_dataset,
        :parameter,
        :rules,
        :pmml
    );
    -- Clear tables content
    truncate table "aa.movielens.db.hdb.pal::apriori.rules";
    truncate table "aa.movielens.db.hdb.pal::apriori.pmml";
    -- Insert the results
    insert into "aa.movielens.db.hdb.pal::apriori.rules" select * from :rules;
    insert into "aa.movielens.db.hdb.pal::apriori.pmml"  select * from :pmml;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

#### The collaborative filtering result procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`apriori_result_collaborative.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/apl/procedures/apriori_result_collaborative.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.movielens.db.hdb.pal.procedures::apriori_result_collaborative" (
    in UserId    integer default -1
   ,in KeepTopN  integer default 5
   ,out results  "aa.movielens.db.hdb.pal::apriori.tt_movielens_collaborative_result"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    results = select
    userid, rank, t1.movieid, score, title, genres, imdbid, tmdbid
  from (
    select
        t1.userid, cast(t1.consequent as integer) as movieid, t1.score
      , row_number() over(partition by t1.userid order by t1.score desc, t1.consequent desc ) as rank
    from (
      select
        input_data.userid,
        rules.postrule as consequent,
        max(rules.confidence) as score
      from "aa.movielens.db.hdb::data.ratings" as input_data
      left outer join "aa.movielens.db.hdb.pal::apriori.rules" rules on (cast (input_data.movieid as varchar(500)) = rules.prerule)
      where rules.postrule is not null
      and   input_data.userid = :UserId
      group by input_data.userid, rules.postrule
    ) t1
  ) t1
  left outer join "aa.movielens.db.hdb::data.movies" movies on movies.movieid = t1.movieid
  left outer join "aa.movielens.db.hdb::data.links"  links  on links.movieid  = t1.movieid
  where t1.rank <= :KeepTopN;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

#### The content based filtering result procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`apriori_result_contentbased.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
movielens/db/src/hdb/apl/procedures/apriori_result_contentbased.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.movielens.db.hdb.pal.procedures::apriori_result_contentbased" (
    in MovieId   integer default -1
   ,in KeepTopN  integer default 5
   ,out results  "aa.movielens.db.hdb.pal::apriori.tt_movielens_contentbased_result"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    results = select
    t1.movieid, rank, similar_movie, score, title, genres, imdbid, tmdbid    
  from (
    select
        t1.movieid
      , row_number() over(partition by t1.movieid order by t1.score desc, t1.consequent desc ) as rank
      , cast(t1.consequent as integer) as similar_movie
      , t1.score
    from (
      select movieid, rules.postrule as consequent, rules.confidence as score
      from "aa.movielens.db.hdb::data.movies" as input_data
      left outer join "aa.movielens.db.hdb.pal::apriori.rules" rules on (cast (input_data.movieid as varchar(500)) = rules.prerule)
      where rules.postrule is not null
      and   input_data.movieid = :MovieId
    ) t1
  ) t1
  left outer join "aa.movielens.db.hdb::data.movies" movies on movies.movieid = t1.similar_movie
  left outer join "aa.movielens.db.hdb::data.links"  links  on links.movieid  = t1.similar_movie
  where t1.rank <= :KeepTopN;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

Using the CTRL button on the keyboard, select the following files:

- **`apriori_execute.hdbprocedure`**
- **`apriori_result_collaborative.hdbprocedure`**
- **`apriori_result_contentbased.hdbprocedure`**

Then, right click the selection, then use the **Build Selected Files** menu.

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Check the PAL procedure (1/3))]

You can now check how many users will get recommendation for the collaborative filtering approach.

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
  call "aa.movielens.db.hdb.pal.procedures::apriori_execute"(
    MIN_SUPPORT    => 0.1,
    MIN_CONFIDENCE => 0.1,
    MIN_LIFT       => 0.0,
    UBIQUITOUS     => 1.0,
    RULES   => :rules,
    PMML    => ?
  );

  select 'rules count' as key, count(1) as value from :rules;
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Check the PAL procedure (2/3))]

You can now check how the collaborative filtering results using the created stored procedure.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
  call "aa.movielens.db.hdb.pal.procedures::apriori_result_collaborative"(
    USERID    => 23,
    KEEPTOPN  => 5,
    RESULTS   => :results
  );
  select * from :results;
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_8]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Check the PAL procedure (3/3))]

You can now check how the content based filtering results using the created stored procedure.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
  call "aa.movielens.db.hdb.pal.procedures::apriori_result_contentbased"(
    MOVIEID   => 32,
    KEEPTOPN  => 5,
    RESULTS   => :results
  );
  select * from :results;
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_9]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Build the SAP HANA Database Module)]

Right click on the **`db`** folder and select **Build**.

![Web IDE](03-01.png)

The console should display at the end the following message:

```
(Builder) Build of /movielens/db completed successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
