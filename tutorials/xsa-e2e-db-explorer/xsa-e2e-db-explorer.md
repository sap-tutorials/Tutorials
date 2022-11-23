---
parser: v2
auto_validation: true
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, products>sap-web-ide ]
---

# Use the database explorer to check the data in your tables
<!-- description --> You will use the Database Explorer on SAP Web IDE for SAP HANA

## Prerequisites  
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
 - **Proficiency:** Beginner

## You will learn  
Describe what the user will learn from your tutorial and what the outcome will be.
## Time to Complete
**15 Min**

---

### Check the database explorer


Go into the database explorer using the button in the middle of the left side bar:

![DB explorer](db.png)

You will be asked if you want to add a Database. Click on **Yes**

![Add DB](yespng.png)

Choose your container. You will know because it will have your username (`XSA_DEV`) and the name of our project (in this case,  `TECHED`).

![Add DB](db2.png)


### Check your data


Once your database container appears, click on the **Tables**. You will see your entity converted into a run time object.

![Explore DB](table.png)

Right-click on the table and choose Open Data:

![Explore DB](open.png)

You will see the data from the CSV file is now loaded:

![Data is now loaded](data.png)



### Check the Select Statement


Click on the **SQL** button on the top of the data preview

![Check the SQL statement](sql.png)

Copy the full SELECT statement into the validation below

![Copy the SQL statement](sql2.png)

And click on **Validate**



### Go back into the Development view


Open a SQL console and leave it open. You will use it in the next step:

![Leave a SQL console open](sql3.png)

Use the code symbol to go back to the development view:

![Go back to the development perspective](dev.png)

---
