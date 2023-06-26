---
parser: v2
author_name: Christopher Kollhed
author_profile: https://github.com/chriskollhed
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-hana-database, programming-tool>sql]
primary_tag: software-product>sap-hana-cloud
---

# Query the Database Using SQL Statements
<!-- description -->Learn how to create new tables, view table details, join tables, and extract specific data from tables using SQL statements in the SAP HANA database explorer.

## Prerequisites
- You have access to [SAP HANA Cloud trial](hana-cloud-mission-trial-2) or [SAP HANA Cloud free tier](hana-cloud-mission-trial-2-ft), or a production environment of SAP HANA Cloud, SAP HANA database
- You have completed the tutorial to [provision an instance of SAP HANA Cloud, SAP HANA database](hana-cloud-mission-trial-3)
- You have completed the tutorial to [import the sample data needed for this mission](hana-cloud-mission-trial-5)
- Optional: You can [download the code snippets](https://github.com/SAP-samples/hana-cloud-learning/blob/4ac0be770033d3425cc30a2f22f8f5c0823bb810/Mission:%20SAP%20HANA%20Database%20in%20SAP%20HANA%20Cloud/Tutorial%206/Tutorial%206%20Queries.txt) used in this tutorial from our public GitHub repository


## You will learn
- How to query your database using the SAP HANA database explorer using SQL statements
- How to create new tables
- How to join tables
- How to extract specific data from tables


## Intro
>
> ![Alex Banner](banner-alex.png)
>
> Reminder: This tutorial is part of a mission, in which you will help Alex, the CEO of Best Run Travel, to answer a concrete business question with SAP HANA Cloud, SAP HANA database.
>
> *Alex needs to know the top 5 partners of their agency and wants to find out the days with maximum booking of each partner.*

---

### Open the SQL console and set the schema


1.	Go to the Instances tab in SAP HANA Cloud Central and open SAP HANA database explorer. 

    ![Open DBx from HANA Cloud Central](open-dbx-hcc.png)

    Then open the SQL Console by clicking the icon in the left-hand top corner.

    ![database explorer with highlight on Open SQL Console](ss-01-database-explorer-open-sql-console.png)

    > You can also open the SQL console by right-clicking on a database name and selecting **Open SQL console** or by using the shortcut `Ctrl+Alt+C`.
    

2.	Once the SQL console loads, ensure that the current schema is `SFLIGHT` by checking the top right-hand side of the console.

3.	If not, copy and paste the following statement to the console and run it:

    ```SQL
    SET SCHEMA SFLIGHT;
    ```  

    The current schema will then display `SFLIGHT`.  

    ![SQL Console, with highlight on Current Schema](ss-02-sql-console-current-schema.png)


### Query the most popular travel agents

Let's find out which of the Best Run Travel agents are most popular. For this, we will need the agency number and the booking details. This means we need to use the tables `SBOOK` and `STRAVELAG`.

1.	First, our goal is to extract the total number of bookings made per agency. To achieve this, we will start by creating a new table `SAGENCYDATA` from the existing tables `SBOOK` and `STRAVELAG`.

2.	The following query will create a new table and order the agencies based on their number of bookings. Copy and paste query to the console and then click on the **Run** button:

    ```SQL
    CREATE TABLE SAGENCYDATA as (select SBOOK.AGENCYNUM, count(SBOOK.AGENCYNUM) as NUMBOOKINGS FROM SBOOK, STRAVELAG WHERE SBOOK.AGENCYNUM=STRAVELAG.AGENCYNUM group by SBOOK.AGENCYNUM ORDER BY count(SBOOK.AGENCYNUM) desc)
    ```

3.	You can view the contents of this table by running the following query:

    ```SQL
    SELECT * FROM SAGENCYDATA;
    ```

    This will show you the contents of the new table:

    ![View contents of SAGENCYDATA](ss-03-view-contents-SAGENCYDATA.png)



### Join tables to find out which agency makes the most bookings


Join the tables `STRAVELAG` and `SAGENCYDATA` based on the column `AGENCYNUM` and extract the top 5 agencies from the result. This will give you the list of agency numbers, names, and the number of bookings for the top 5 agencies.

You can use the following query that will join the tables and select the top 5 entries:

```SQL
SELECT TOP 5 SAGENCYDATA.AGENCYNUM, STRAVELAG.NAME,SAGENCYDATA.NUMBOOKINGS FROM SAGENCYDATA INNER JOIN STRAVELAG on SAGENCYDATA.AGENCYNUM = STRAVELAG.AGENCYNUM;
```

In the results panel, you can now see that the travel agency that makes the maximum bookings is `Rainy, Stormy, Cloudy` with a total of **27870 bookings**.

![Top 5 agency details](ss-04-top-5-agency-details.png)




### Find out which days have the most bookings


Since Alex also wants to know on which **days of the week** the top 5 travel agencies make most bookings, we need to use a few more queries.

To find the top booking days, we will first create two new tables:

-	`STOPAGENCY`: name, agency number, and count of the bookings made by top 5 agencies
-	`SAGBOOKDAYS`: details on the number of bookings made per day for each of the agencies

1.	First create the `STOPAGENCY` table by storing the result of the previous query in a new table. Run this query in your console:

    ```SQL
    CREATE TABLE STOPAGENCY AS (SELECT TOP 5 SAGENCYDATA.AGENCYNUM, STRAVELAG.NAME,SAGENCYDATA.NUMBOOKINGS FROM SAGENCYDATA INNER JOIN STRAVELAG ON SAGENCYDATA.AGENCYNUM = STRAVELAG.AGENCYNUM);
    ```

2.	To view all contents of this table, just copy and paste the following query into the SQL console and run it:

    ```SQL
    SELECT * FROM STOPAGENCY;
    ```

3.	Next, create the table `SAGBOOKDAYS` to store the daily bookings for each of the agencies. Use the following query:

    ```SQL
    CREATE TABLE SAGBOOKDAYS AS (SELECT AGENCYNUM, dayname(ORDER_DATE) as ORDERDAY, count(dayname(ORDER_DATE)) AS DAYCOUNT FROM SBOOK GROUP BY AGENCYNUM, dayname(ORDER_DATE))
    ```

4.	To view all contents of this new table, you can again use the `SELECT * FROM` query:

    ```SQL
    SELECT * FROM SAGBOOKDAYS;
    ```

5.	Now that you have created the 2 tables, join these tables based on the agency number (column `AGENCYNUM`). You also need to extract only the day with maximum number of bookings for each of the top 5 agencies. For this, use the following nested queries:

    ```SQL
    SELECT SAGBOOKDAYS.AGENCYNUM, STOPAGENCY.NAME, SAGBOOKDAYS.ORDERDAY, SAGBOOKDAYS.DAYCOUNT FROM SAGBOOKDAYS INNER JOIN STOPAGENCY ON SAGBOOKDAYS.AGENCYNUM=STOPAGENCY.AGENCYNUM WHERE SAGBOOKDAYS.DAYCOUNT IN (SELECT max(DAYCOUNT) FROM SAGBOOKDAYS GROUP BY AGENCYNUM)
    ```

6.	Now you can see that the most bookings for the top 5 agencies have been done on **Thursdays**.

    ![Maximum days](maximum-days.png)

*Well done!*

You have completed the sixth tutorial of this mission! You learned how to create new tables, view table details, join tables and extract specific data from tables using SQL statements in the SAP HANA database explorer. This way you were able to help Alex get the business inside they were looking for.

Since Alex needs to make these insights available to other departments in Best Run Travel, the next step is to create a calculation view to share these findings with other departments.  

Learn in the next tutorial how to get started with SAP Business Application to start creating a calculation view.



### Knowledge Check






---
