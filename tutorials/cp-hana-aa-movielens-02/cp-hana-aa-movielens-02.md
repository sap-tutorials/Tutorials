---
title: Analyze the MovieLens dataset
description: Understanding the data set structure and content by extracting some statistics will allow you to better pick your algorithm and the associated setting
auto_validation: true
primary_tag: topic>machine-learning
tags: [  tutorial>beginner, products>sap-hana, products>sap-cloud-platform, topic>machine-learning ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  

- Understand the basics about recommendation engines
- Which statistics can help you better understand the structure of the dataset
- Based on the statistical assessment, identity what algorithm options are available

### Time to Complete
**20 Min**

[ACCORDION-BEGIN [Info: ](Recommendation Systems)]

The goal of a recommendation systems is to produce a list of rules. This set of rules are usually built using a transactional type of data set which identifies links between a user and an item. Then, the rule set is applied to either to a user or an item to get a list of items to recommend.

These 2 approaches to building recommendation systems are:

- Collaborative filtering:

    With this approach, you can build a model using past users behaviors (items previously purchased by a user or movies rated by a user for example) and based on behavior similarities between users.

    Collaborative filtering is based on the assumption that users who had similar behaviors in the past will have the same behaviors in the future, and that they will like similar kinds of items as they liked in the past.

    In this scenario, you can build your model by analyzing links (transactions) between 2 types of nodes, one will be the user and the other the item. And the resulting model will allow you to extract the likelihood of a relation between a user and an item.

    In other word, when scoring with the model, the input information will be a user, and the output will be a list of items and their associated score.

- Content-based filtering:

    With this approach, you can recommend items that are similar to each other based on the number of links they have in common compared to other items using for example a series of associated keywords or tags, but also user clicks or orders.

    Just like with *Collaborative filtering*, you can build your model by analyzing links (transactions) between 2 types of nodes, one will be the user and the other the item. Here the "user" doesn't represent always correspond to a user or buyer, but can reference a tag or a web session id.

    In the end, unlike with *Collaborative filtering*, the resulting model will allow you to extract the likelihood of a relation between a pair of items and when scoring with the model, the input information will be an item, and the output will be a list of items and their associated score.

> ### **Note**
>The above description is only meant to give you a brief (and simplified) overview of what is a recommendation system in general.
>For more details, you can check the <a href="https://en.wikipedia.org/wiki/Recommender_system" target="new">Recommender system on Wikipedia</a> page for more details and references (which I used here to provide some details for this tutorial series).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](The dataset structure)]

Using the dataset <a href="http://files.grouplens.org/datasets/movielens/ml-latest-small-README.html" target="new">README</a> content, you can extract the following details about the data file structure:

- ***RATINGS***:

    - `USERID` & `MOVIEID`: represent the user id and movie id
    - `RATING` : uses a 5-star scale, with 0.5 star increments
    - `TIMESTAMP` : use the epoch format (seconds since midnight of January 1, 1970 on UTC time zone)

- ***TAGS***:

    - `USERID` & `MOVIEID`: represent the user id and movie id
    - `TAG` : represent user-generated textual metadata
    - `TIMESTAMP` : use the epoch format (seconds since midnight of January 1, 1970 on UTC time zone)

- ***MOVIES***:

    - `MOVIEID`: represent the movie id
    - `TITLE` : represent the full movie title and may include the year of release
    - `GENRE` : a pipe-separated list of genres associated with the movie

- ***LINKS***:

    - `MOVIEID`: represent the movie id
    - `IMDBID` : can be used to generate a link to the **`IMDb`** site.
    - `TMDBID` : can be used to generate a link to the **`The Movie DB`** site.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Open the Catalog perspective)]

There are multiple ways to analyze this type of dataset, like using SAP Lumira or SAP Analytics Cloud but this would require additional product to be installed or activated.

To keep it simple and easy, we will be executing a series of SQL statements using the ***SAP HANA Web-based Development Workbench*** available with any SAP HANA MDC on the SAP Cloud Platform.

In order to execute our series of SQL statements, we will be using the **Catalog** perspective available in the **SAP HANA Web-based Development Workbench**.

From the ***SAP HANA Web-based Development Workbench*** main panel, click on **Catalog**:

![SAP HANA Web-based Development Workbench](01.png)

Else, if you are already accessing one of the perspective, then use the ![plus](0-navigation.png) icon from the menu:

![SAP HANA Web-based Development Workbench](02.png)

> ### **Note**
>**Make sure the currently connected user is TRIAL and not SYSTEM**. Check the upper right corner of the SAP HANA Web-based Development Workbench.
>
>For each of the next steps, you can decide to open a new **SQL Console** using the ![sql](0-opensqlconsole.png) icon from the menu or reuse the same one by replacing its current over and over.

&nbsp;

First, let's count the rows in each table.

Open a new **SQL Console** using the ![sql](0-opensqlconsole.png) icon from the menu or reuse an existing one.

Paste the following content in the console, and use the execute icon ![run](0-run.png) from the menu.

```SQL
select 'links'   as "table name", count(1) as "row count" from "MOVIELENS"."public.aa.movielens.cds::data.LINKS"
union all
select 'movies'  as "table name", count(1) as "row count" from "MOVIELENS"."public.aa.movielens.cds::data.MOVIES"
union all
select 'ratings' as "table name", count(1) as "row count" from "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
union all
select 'tags'    as "table name", count(1) as "row count" from "MOVIELENS"."public.aa.movielens.cds::data.TAGS";
```

The result should be:

table name | row count
-----------|-----------
links      | 9125    
movies     | 9125    
ratings    | 100004  
tags       | 1296    

Here are a few conclusion we can make upfrone:

- links: contains URL and therfore cannot be used in our models
- movies: the genres can be inestigated but because the data structure it will require some transformations
- tags: not every movie has tags (1296 tags across 9125 movies) so we cannot use it in our models

Therefore only the ratings will be analyzed here.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Ratings - Check the movie distribution)]

Now let's determine the rating count distribution per movies using the following SQL:

```SQL
SELECT "RATING_COUNT", COUNT(1) as "MOVIE_COUNT"
FROM (
  SELECT "MOVIEID", COUNT(1) as "RATING_COUNT"
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
  GROUP BY "MOVIEID"
)
GROUP BY "RATING_COUNT" ORDER BY "RATING_COUNT" asc;
```

This time, the list is a bit long to extract insights.

However, you can notice that 3063 movies have only one rating and 1202 have only 2 ratings.

Instead of browsing the results for insights, you can use some aggregates like the min, max, average, count, median and standard deviation using the following SQL:

```SQL
SELECT DISTINCT
  MIN("RATING_COUNT") OVER( ) AS "MIN",
  MAX("RATING_COUNT") OVER( ) AS "MAX",
  AVG("RATING_COUNT") OVER( ) AS "AVG",
  SUM("RATING_COUNT") OVER( ) AS "SUM",
  MEDIAN("RATING_COUNT") OVER( ) AS "MEDIAN",
  STDDEV("RATING_COUNT") OVER( ) AS "STDDEV",
  COUNT(*) OVER( ) AS "CATEGORY_COUNT"
FROM (
  SELECT "MOVIEID", COUNT(1) as "RATING_COUNT"
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
  GROUP BY "MOVIEID"
)
GROUP BY "RATING_COUNT";
```

And the result is:

  Aggregation    | Value
-----------------|------
           `MIN` | 1
           `MAX` | 341
           `AVG` | 104.135135
           `SUM` | 19265
        `MEDIAN` | 93
        `STDDEV` | 73.027825
`CATEGORY_COUNT` | 185

Using the results provided by the previous SQL statements, provide an answer to the question below then click on **Validate**.

[VALIDATE_5]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Ratings - Check the user distribution)]

Now let's determine the rating count distribution per user using the following SQL:

```SQL
SELECT "RATING_COUNT", COUNT(1) as "USER_COUNT"
FROM (
  SELECT "USERID", COUNT(1) as "RATING_COUNT"
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
  GROUP BY "USERID"
)
GROUP BY "RATING_COUNT" ORDER BY 1 DESC;
```

You can notice that one user rated 2391 movies, and the top 10 users all rated more than 1000 movies.

Again her, instead of browsing the results for insights, you can use some aggregates like the min, max, average, count, median and standard deviation using the following SQL:

```SQL
SELECT DISTINCT
  MIN("RATING_COUNT") OVER( ) AS "MIN",
  MAX("RATING_COUNT") OVER( ) AS "MAX",
  AVG("RATING_COUNT") OVER( ) AS "AVG",
  SUM("RATING_COUNT") OVER( ) AS "SUM",
  MEDIAN("RATING_COUNT") OVER( ) AS "MEDIAN",
  STDDEV("RATING_COUNT") OVER( ) AS "STDDEV",
  COUNT(*) OVER( ) AS "CATEGORY_COUNT"
FROM (
  SELECT "USERID", COUNT(1) as "RATING_COUNT"
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
  GROUP BY "USERID"
)
GROUP BY "RATING_COUNT" ORDER BY 1 DESC;
```

And the result is:

  Aggregation    | Value
-----------------|------
           `MIN` | 20
           `MAX` | 2391
           `AVG` | 274.420454
           `SUM` | 72447
        `MEDIAN` | 174
        `STDDEV` | 322.354847
`CATEGORY_COUNT` | 264

Using the results provided by the previous SQL statements, provide an answer to the question below then click on **Validate**.

[VALIDATE_6]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Ratings - Check the rating notation distribution)]

Now let's determine the rating notation distribution using the following SQL:

```SQL
SELECT "RATING", COUNT(1) as "RATING_COUNT"
FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
GROUP BY "RATING" ORDER BY 1 DESC;
```

And the result is:

`RATING` | `RATING_COUNT`
---------|--------------
   5     |       15095
   4.5   |        7723
   4     |       28750
   3.5   |       10538
   3     |       20064
   2.5   |        4449
   2     |        7271
   1.5   |        1687
   1     |        3326
   0.5   |        1101

Now let's determine the users distribution per rating notation using the following SQL:

```SQL
SELECT "RATING",  COUNT(1) as "USERS_COUNT" FROM (
  SELECT "USERID", "RATING", COUNT(1) as RATING_COUNT
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
  GROUP BY "USERID", "RATING"
)
GROUP BY "RATING" ORDER BY 1 DESC;
```

And the result is:

`RATING` | `USERS_COUNT`
---------|-------------
   5     |        644
   4.5   |        339
   4     |        669
   3.5   |        346
   3     |        655
   2.5   |        286
   2     |        554
   1.5   |        199
   1     |        409
   0.5   |        177

Now let's determine the movies distribution per rating notation using the following SQL:

```SQL
SELECT "RATING",  COUNT(1) as MOVIES_COUNT FROM (
  SELECT "MOVIEID", "RATING", COUNT(1) as RATING_COUNT
  FROM "MOVIELENS"."public.aa.movielens.cds::data.RATINGS"
  GROUP BY "MOVIEID", "RATING"
)
GROUP BY "RATING" ORDER BY 1 DESC;
```

And the result is:

`RATING` | `MOVIES_COUNT`
---------|--------------
   5     |         3127
   4.5   |         2454
   4     |         5141
   3.5   |         3612
   3     |         4771
   2.5   |         2409
   2     |         3130
   1.5   |         1204
   1     |         1959
   0.5   |          868

Using the results provided by the previous SQL statements, provide an answer to the question below then click on **Validate**.

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Ratings - Conclusion)]

Here are a few insights that you can gather based on the previous results:

- the distribution for the rating count per movie is "better" spread compared to the rating count per user
- at least a user has rated 2391 movies, and the top 10 users have rated more than 1000 movies each
- 3063 movies have a single rating
- users are mostly rating movies they liked compared to the one they dislike
- only one third of the rating notations uses half star notations

Using the rating count distribution per users results, especially the average and median metric and the standard deviation, you can assess that the distribution is skewed and that we have some outliers users (remember the top 10 users rated more than 1000 movies each, when the average is 274, and the median is 174).

> ### Note
>
><center><b>The mean vs the median</b></center>
>
>To keep it simple the median is the middle point of your list. So, imagine you have a list with 7 items, then the median is the 4th element.
>The median helps you assess if your data set is not skewed or have outliers.
>
>With another example, imagine you have a 5 people working in you company where the compensation goes from 10 to 100 and the sum of all compensation would be 200 (whatever the unit). So here, your minimum and maximum compensations are respectively 10 and 100.
>
But the average is 200/5 = 40 which is really far from the maximum. Actually with 5 people, you can sum your min and max, and the rest will be assigned to the other 3 people (200 - 100 - 10 = 90), therefore each one should get something closer to 30 than 40, but also 2 could get 15, and the last 60.
>With the median, you can sort your list of employee by compensation and pick the one in the middle.
>
>
><center><b>The "4 sigma" rule of thumb</b></center>
>
>In short, when a node (here a user) has 4 times more links (rating) than the standard deviation value, this node can be considered as a *mega hub* and can be associated to an outlier node, and usually should be flagged or excluded from the model.
>
>Definition by the author, `Abdel Dadouche` (not meant to be fully scientific & open to discussions)

&nbsp;

Based on the elements gathered over the last few steps, and despite some of the phenomenon assessed in the data, we can consider that the rating dataset on its own is a possible candidate to build a solid recommendation engine.

Again, we could eventually combine it to the tags and the genres and improve the overall recommendation results by increasing the result coverage in terms of users or movies.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Final conclusion)]

As a final conclusion, you can consider that the rating dataset on its own is the most promising candidate to build a  recommendation engine and despite some of the phenomenon assessed for the rating dataset.

However, while using this data will, you will need to pay attention to the following algorithm parameters:

- minimum support: a substantial population of movies have only one rating
- mega-hubs: 10 users have rated more than a 1000 movies each (out of 100,000 ratings).

> ### Note
>
><center><b>The minimum support</b></center>
>
>Defines the number of links between a pair of items. For example, with transaction between users and products , if the minimum support to link a pair of product is 10, then we will need to find 10 users who bought bot products to be able to create a link between products.
>
><center><b>The mega-hub</b></center>
>
>We consider a node to be a mega-hub when the number of links to other node is above a threshold. Theses node are usually excluded from analysis as they can severely impact the quality of a model.
>
>For example if we try to analyze grocery receipts, you can agree that many customer will "buy" a plastic bags (at least in France because we have a tendency to leave them in the car). But as an item it doesn't make sense to use it as it will be linked to many users and therefore many other items.

&nbsp;

[DONE]
[ACCORDION-END]

