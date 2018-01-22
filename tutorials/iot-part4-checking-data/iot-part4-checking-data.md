---
title: Internet of Things (IoT) Check your data
description: Part 4 of 10, Check your SAP HANA tables for the newly added data
primary_tag: topic>internet-of-things
tags: [products>sap-hana, topic>big-data, topic>internet-of-things, tutorial>beginner ]

---

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Internet of Things (IoT) Posting data with a REST Client](https://www.sap.com/developer/tutorials/iot-part3-posting-data-hana.html)


## Next Steps
- [Internet of Things (IoT) Using the Tessel to post data](https://www.sap.com/developer/tutorials/iot-part5-inserting-tessel-data.html)

## Details
### You will learn  Checking the data you have inserted is simple. in this section you will view the data in the catalog and work with a few SQL queries.

### Time to Complete
**10 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Launch catalog)] ￼

Launch the **Catalog** by clicking the Link button in the editor and choosing **Catalog** which will open a new window.

![Catalog selection](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_1.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](View schemas)] ￼

This window will show all the SCHEMAS for which your user has access. Unroll the **Catalog** folder, unroll the folder with `your name` (e.g. `JOHNDOE`, then **Tables**.

![Tables](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](View data inserted using Postman)] ￼

From there right-click and select **Open Content** which will display the data you have inserted via Postman.

![Open Content](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_3.png)

Depending on how many POSTs you sent, your screen will look similar to this:

![Display Content](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](View SQL statement)] ￼

When you selected **Open Content**, the system executed the following SQL command to display the data:```sqlSELECT TOP 1000
"ID",
"TIMESTAMP",
"TEMPERATURE",
"HUMIDITY",
"BRIGHTNESS"
FROM "JOHNDOE"."CODEJAMMER.johndoe.myiot::mydata.Data";```
To view the SQL window, click the **Edit SQL statement** button.![SQL console](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_5.png)[ACCORDION-END][ACCORDION-BEGIN [Step 5: ](Get record count)] ￼To see how many records there are (handy for later), you can modify the SQL to something like:

```sqlSELECT count(*)FROM "JOHNDOE"."CODEJAMMER.johndoe.myiot::mydata.Data";```
Which will show these results:

![results](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/iot-part4-checking-data/p4_6.png)
￼You'll only have as many records in there as you added with Postman, but after you insert data directly from the Tessel, you can return to this and section and check again.
[ACCORDION-END]## Next Steps
- [Internet of Things (IoT) Using the Tessel to post data](https://www.sap.com/developer/tutorials/iot-part5-inserting-tessel-data.html)
