---
title: Internet of Things (IoT) Check your data
description: Part 4 of 10, Check your SAP HANA tables for the newly added data
tags: [products>sap-hana, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Internet of Things (IoT) Posting data with a REST Client](http://go.sap.com/developer/tutorials/iot-part3-posting-data-hana.html)


## Next Steps
 - [Internet of Things (IoT) Using the Tessel to post data](http://go.sap.com/developer/tutorials/iot-part5-inserting-tessel-data.html)

## Details
### You will learn  Checking the data you have inserted is simple. in this section you will view the data in the catalog and work with a few SQL queries.

### Time to Complete
**10 Min**.

---
1.  Launch the **Catalog** by clicking the Link button in the editor and choosing **Catalog** which will open a new window.

     ![Catalog selection](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_1.png)


2. This window will show all the SCHEMAS for which your user has access. Unroll the **Catalog** folder, unroll the folder with `your name` (e.g. `JOHNDOE`, then **Tables**.

     ![Tables](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_2.png)

3. From there right-click and select **Open Content** which will display the data you have inserted via Postman.

     ![Open Content](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_3.png)

4. Depending on how many POSTs you sent, your screen will look similar to this:

     ![Display Content](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_4.png)

5. When you selected **Open Content**, the system executed the following SQL command to display the data:    ```sql    SELECT TOP 1000
    "ID",
    "TIMESTAMP",
    "TEMPERATURE",
    "HUMIDITY",
    "BRIGHTNESS"
    FROM "JOHNDOE"."CODEJAMMER.johndoe.myiot::mydata.Data";    ```
    To view the SQL window, click the **Edit SQL statement** button.      ![SQL console](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_5.png)6. To see how many records there are (handy for later), you can modify the SQL to something like:

     ```sql     SELECT count(*)     FROM "JOHNDOE"."CODEJAMMER.johndoe.myiot::mydata.Data";     ```
     Which will show these results:

     ![results](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_6.png)
￼     You’ll only have as many records in there as you added with Postman, but after you insert data directly from the Tessel, you can return to this and section and check again.
## Next Steps
 - [Internet of Things (IoT) Using the Tessel to post data](http://go.sap.com/developer/tutorials/iot-part5-inserting-tessel-data.html)
