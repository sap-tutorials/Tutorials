---
title: Analyze the MovieLens dataset
description: Understanding the data set structure and content by extracting some statistics will allow you to better pick your algorithm and the associated setting
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 30
---

## Prerequisites
 - [Use Machine Learning to Build a Movie Recommendation application using the XS advanced development model](https://www.sap.com/developer/groups/hxe-aa-moveilens.html)

## Next Steps
 - [Use Machine Learning to Build a Movie Recommendation application using the XS advanced development model](https://www.sap.com/developer/groups/hxe-aa-moveilens.html)

## Details
### You will learn

- Understand the basics about recommendation engines
- Which statistics can help you better understand the structure of the dataset
- Based on the statistical assessment, identity what algorithm options are available

[ACCORDION-BEGIN [Info: ](Recommendation Systems)]

The goal of a recommendation systems is to produce a list of rules. This set of rules are usually built using a transactional type of data set which identifies links between users and items. Then, the rule set is applied to either a user or an item to get a list of items to recommend.

This imply 2 approaches to building recommendation systems:

- **Collaborative filtering**:

  With this approach, you can build a model using past similarities between users behaviors (items previously purchased by a user or movies rated by a user for example).

  Collaborative filtering is based on the assumption that users who had similar behaviors in the past will have the same behaviors in the future, and that they will like items that other users with similar behaviors liked in the past.

  In this scenario, you can build your model by analyzing links (transactions) between 2 types of nodes, one will be the user and the other the item. And the resulting model will allow you to extract the likelihood of a relation between a user and an item.

  In other word, when scoring with the model, the input information will be a user, and the output will be a list of items and their associated score.

- **Content-based filtering**:

  With this approach, you can recommend items that are similar to each other based on the number of links they have in common compared to other items using for example a series of associated keywords or tags, but also user clicks or orders.

  Just like with *Collaborative filtering*, you can build your model by analyzing links (transactions) between 2 types of nodes, one will be the user and the other the item.

  In the end, unlike with *Collaborative filtering*, the resulting model will allow you to extract the likelihood of a relation between a pair of items (not user to items) and when scoring with the model, the input information will be an item, and the output will be a list of items and their associated score.

> ### **Note**
>The above description is only meant to give you a brief (and simplified) overview of what is a recommendation system in general.
>For more details, you can check the <a href="https://en.wikipedia.org/wiki/Recommender_system" target="new">Recommender system on Wikipedia</a> page for more details and references (which I used here to provide some details for this tutorial series).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Info: ](The dataset structure)]

Using the <a href="http://files.grouplens.org/datasets/movielens/ml-latest-small-README.html" target="new">README</a> available with the dataset content, you can extract the following details about the data file structure:

- ***Ratings***:

    - `userId` & `movieid`: represent the user id and movie id
    - `rating` : uses a 5-star scale, with 0.5 star increments
    - `timestamp` : use the epoch format (seconds since midnight of January 1, 1970 on UTC time zone)

- ***Tags***:

    - `userId` & `movieid`: represent the user id and movie id
    - `tag` : represent user-generated textual metadata
    - `timestamp` : use the epoch format (seconds since midnight of January 1, 1970 on UTC time zone)

- ***Movies***:

    - `movieid`: represent the movie id
    - `title` : represent the full movie title and may include the year of release
    - `genre` : a pipe-separated list of genres associated with the movie

- ***Links***:

    - `movieid`: represent the movie id
    - `imdbId` : can be used to generate a link to the ***`IMDb`*** site.
    - `tmdbId` : can be used to generate a link to the ***`The Movie DB`*** site.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Open the Database Explorer)]

Open the Web IDE, and login using the **`XSA_DEV`** credentials.

Switch to the ***Database Explorer*** perspective using the ![Database Explorer](00-dbexplorer-icon.png) icon.

![Web IDE](00-dbexplorer.png)

As a reminder the default URL for the Web IDE is:

 - `https://hxehost:53075`

A link to the Web IDE can also be found on the ***XSA Controller page*** at:

- `https://hxehost:39030`

Select **HDI Container** connection created previously with a name starting with ***`XSA_DEV`***.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

> ### **Note**
>For each of the next steps, you can decide to open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon from the menu or reuse the same one by replacing its current over and over.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
select 'links'   as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.links"
union all
select 'movies'  as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.movies"
union all
select 'ratings' as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.ratings"
union all
select 'tags'    as "table name", count(1) as "row count" from "aa.movielens.db.hdb::data.tags";
```

The result should be:

table name  | row count
-----------:|-----------
links       | 9125
movies      | 9125
ratings     | 100004
tags        | 1296

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Links - Check missing values)]

As stated earlier, the link dataset only includes details to build URL to external web site.

Let's verify that every movie has a corresponding link and vice-versa using the following SQL:

```SQL
select count(1)
from "aa.movielens.db.hdb::data.links" l
where not exists (select 1 from "aa.movielens.db.hdb::data.movies" m where l.movieid = m.movieid)
union all
select count(1)
from "aa.movielens.db.hdb::data.movies" m
where not exists (select 1 from "aa.movielens.db.hdb::data.links" l where l.movieid = m.movieid);
```

Based on the result, it seems that there isn't any movies with no links and vice-versa.

So, when building our application, we will be able to leverage these URL and enhance our user experience with external links.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Movies - Check the genres)]

Just like with the links dataset, the movies dataset doesn't include any transaction kind of details that could be used directly to link users together.

Only the ***genre*** column could be used to link movies together. By default the column comes as a pipe-separated list, which will require some transformation if we want to use it.

Anyway, let's check if all movies have genres with the following SQL:

```SQL
select count(1)
from "aa.movielens.db.hdb::data.movies"
where genres is null or length(genres)=0;
```

Based on the result, it seems that all movies have at least a genre.

Now, let's get the list of genres used across our 9125 movies with the following SQL:

```SQL
do begin
  declare genrearray nvarchar(255) array;
  declare tmp nvarchar(255);
  declare idx integer;
  declare sep nvarchar(1) := '|';
  declare cursor cur for select distinct genres from "aa.movielens.db.hdb::data.movies";
  declare genres nvarchar (255) := '';
  idx := 1;
  for cur_row as cur() do
    select cur_row.genres into genres from dummy;
    tmp := :genres;
    while locate(:tmp,:sep) > 0 do
      genrearray[:idx] := substr_before(:tmp,:sep);
      tmp := substr_after(:tmp,:sep);
      idx := :idx + 1;
    end while;
    genrearray[:idx] := :tmp;
  end for;
  genrelist = unnest(:genrearray) as (genre);
  select genre from :genrelist group by genre;
end;
```
Here, I used the some of the SAP HANA *SQL Script* functions like `SUBSTR_BEFORE` & `SUBSTR_AFTER` to split the genre string into pieces.

Based on the result returned by the above SQL statement, provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Movies - Get the movie count per genre)]

Now let's get the number of movies associated with each genres by adjusting the previous SQL:

```SQL
do begin
  declare genrearray nvarchar(255) array;
  declare tmp nvarchar(255);
  declare idx integer;
  declare sep nvarchar(1) := '|';
  declare cursor cur for select distinct genres from "aa.movielens.db.hdb::data.movies";
  declare genres nvarchar (255) := '';
  idx := 1;
  for cur_row as cur() do
    select cur_row.genres into genres from dummy;
    tmp := :genres;
    while locate(:tmp,:sep) > 0 do
      genrearray[:idx] := substr_before(:tmp,:sep);
      tmp := substr_after(:tmp,:sep);
      idx := :idx + 1;
    end while;
    genrearray[:idx] := :tmp;
  end for;
  genrelist = unnest(:genrearray) as (genre);
  select genre, count(1) from :genrelist group by genre;
end;
```

You can see that 18 distinct genres are used across the 9125 movies.

Based on the result returned by the above SQL statement, provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Movies - Get the genre count per movie)]

Now, let's get the number of genres associated with each movies using the following SQL:

```SQL
select
    movieid
  , title
  , occurrences_regexpr('[|]' in genres) + 1 as genre_count
  , genres
from "aa.movielens.db.hdb::data.movies"
order by genre_count asc;
```

You can see that many movies have only one genre.

Using the above SQL statement, and changing the **order by** direction from **`ASC`** to **`DESC`**, provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Movies - Conclusion)]

As seen in the previous step, there are many movies with only one genre.

Let's count the movies per genre count using the following SQL:

```SQL
select
  genre_count, count(1)
from (
  select occurrences_regexpr('[|]' in genres) + 1 genre_count
  from "aa.movielens.db.hdb::data.movies"
) group by genre_count order by genre_count;
```

The result should be 2793 movies with one genre, which means almost a third of the movie set have one genre only.

This means that these movies will be linked to another movie by at most one link, which will cause all relations between movies to be more or less equal in term of strength (the more links between nodes, the stronger the relationship is).

You could also decide to simply exclude the movies with one genre and only keep the other but this would mean that you won't provide results for them which would require to address them using an alternative approach.

So, based on the elements gathered over the last steps, you can consider that the genre extracted from the movies on its own is not a good candidate to build a solid recommendation engine.

Moreover, the genre data can only be used to address a content-based filtering approach.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Tags - Check the distribution)]

Now let's have a look at the tags distribution using the following SQL:

```SQL
select count(1)
from (
  select movieid, count(1) as tag_count
  from "aa.movielens.db.hdb::data.tags"
  group by movieid
);
```

Only 689 movies have one or more tag.

Now let's determine the tag count distribution per movies using the following SQL:

```SQL
select tag_count, count(1)
from (
  select movieid, count(1) as tag_count
  from "aa.movielens.db.hdb::data.tags"
  group by movieid
)
group by tag_count order by tag_count;
```

You can notice that out of the 689 movies with at least a tag, you have 483 movies with only one tag.

Based on the elements gathered over the last steps, you can consider that the tag dataset on its own is not a good candidate to build a solid recommendation engine.

Also the tag data can only be used to address a content-based filtering approach.

> **Idea**: you could eventually join the tags and the genres data together to expand the movie set coverage

Using the results provided by the previous SQL statements, provide an answer to the question below then click on **Validate**.

[VALIDATE_4]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Ratings - Check the movie distribution)]

Now let's determine the rating count distribution per movies using the following SQL:

```SQL
select rating_count, count(1) as movie_count
from (
  select movieid, count(1) as rating_count
  from "aa.movielens.db.hdb::data.ratings"
  group by movieid
)
group by rating_count order by rating_count asc;
```

This time, the list is a bit long to extract insights.

However, you can notice that 3063 movies have only one rating and 1202 have only 2 ratings.

Instead of browsing the results for insights, you can use some aggregates like the min, max, average, count, median and standard deviation using the following SQL:

```SQL
select distinct
  min(rating_count) over( ) as min,
  max(rating_count) over( ) as max,
  avg(rating_count) over( ) as avg,
  sum(rating_count) over( ) as sum,
  median(rating_count) over( ) as median,
  stddev(rating_count) over( ) as stddev,
  count(*) over( ) as category_count
from (
  select movieid, count(1) as rating_count
  from "aa.movielens.db.hdb::data.ratings"
  group by movieid
)
group by rating_count;
```

And the result is:

     Aggregation | Value
----------------:|------
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

[ACCORDION-BEGIN [Step 9: ](Ratings - Check the user distribution)]

Now let's determine the rating count distribution per user using the following SQL:

```SQL
select rating_count, count(1) as user_count
from (
  select userid, count(1) as rating_count
  from "aa.movielens.db.hdb::data.ratings"
  group by userid
)
group by rating_count order by 1 desc;
```

You can notice that one user rated 2391 movies, and the top 10 users all rated more than 1000 movies.

Again her, instead of browsing the results for insights, you can use some aggregates like the min, max, average, count, median and standard deviation using the following SQL:

```SQL
select distinct
  min(rating_count) over( ) as min,
  max(rating_count) over( ) as max,
  avg(rating_count) over( ) as avg,
  sum(rating_count) over( ) as sum,
  median(rating_count) over( ) as median,
  stddev(rating_count) over( ) as stddev,
  count(*) over( ) as category_count
from (
  select userid, count(1) as rating_count
  from "aa.movielens.db.hdb::data.ratings"
  group by userid
)
group by rating_count order by 1 desc;
```

And the result is:

     Aggregation | Value
----------------:|------
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

[ACCORDION-BEGIN [Step 10: ](Ratings - Check the rating notation distribution)]

Now let's determine the rating notation distribution using the following SQL:

```SQL
select rating, count(1) as rating_count
from "aa.movielens.db.hdb::data.ratings"
group by rating order by 1 desc;
```

And the result is:

`rating` | `rating_count`
--------:|--------------:
  5      |       15095
  4.5    |        7723
  4      |       28750
  3.5    |       10538
  3      |       20064
  2.5    |        4449
  2      |        7271
  1.5    |        1687
  1      |        3326
  0.5    |        1101

Now let's determine the users distribution per rating notation using the following SQL:

```SQL
select rating,  count(1) as users_count from (
  select userid, rating, count(1) as rating_count
  from "aa.movielens.db.hdb::data.ratings"
  group by userid, rating
)
group by rating order by 1 desc;
```

And the result is:

`rating` | `users_count`
--------:|--------------:
  5      |        644
  4.5    |        339
  4      |        669
  3.5    |        346
  3      |        655
  2.5    |        286
  2      |        554
  1.5    |        199
  1      |        409
  0.5    |        177

Now let's determine the movies distribution per rating notation using the following SQL:

```SQL
select rating,  count(1) as movie_count from (
  select movieid, rating, count(1) as rating_count
  from "aa.movielens.db.hdb::data.ratings"
  group by movieid, rating
)
group by rating order by 1 desc;
```

And the result is:

`rating` | `movie_count`
--------:|-------------:
  5      |        3127
  4.5    |        2454
  4      |        5141
  3.5    |        3612
  3      |        4771
  2.5    |        2409
  2      |        3130
  1.5    |        1204
  1      |        1959
  0.5    |         868

Using the results provided by the previous SQL statements, provide an answer to the question below then click on **Validate**.

[VALIDATE_7]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Ratings - Conclusion)]

Here are a few insights that you can gather based on the previous results:

- the distribution for the rating count per movie is "better" spread compared to the rating count per user
- at least a user has rated 2391 movies, and the top 10 users have rated more than 1000 movies each
- 3063 movies have a single rating
- users are mostly rating movies they liked compared to the one they dislike
- only one third of the rating notations uses half star notations

Using the rating count distribution per users results, especially the average and median metric and the standard deviation, you can assess that the distribution is skewed and that we have some outliers users (remember the top 10 users rated more than 1000 movies each, when the average is 274, and the median is 174).

> ### **Note:**
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

Based on the elements gathered over the last few steps, and despite some of the phenomenon assessed in the data, we can consider that the rating dataset on its own is a possible candidate to build a solid recommendation engine.

Again, we could eventually combine it to the tags and the genres and improve the overall recommendation results by increasing the result coverage in terms of users or movies.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Final conclusion)]

As a final conclusion, you can consider that the rating dataset on its own is the most promising candidate to build a recommendation engine and despite some of the phenomenon assessed for the rating dataset.

However, while using this data will, you will need to pay attention to the following algorithm parameters:

- minimum support: a substantial population of movies have only one rating
- mega-hubs: 10 users have rated more than a 1000 movies each (out of 100,000 ratings).

> ### **Note:**
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

[DONE]
[ACCORDION-END]
