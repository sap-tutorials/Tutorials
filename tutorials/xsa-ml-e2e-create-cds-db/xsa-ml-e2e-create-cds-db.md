---
title: Create a Database Module
description: Create database objects and CDS views
auto_validation: true
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>big-data, products>sap-hana ]
---

## Prerequisites
 - This tutorial is designed for SAP HANA on premise and SAP HANA, express edition. It is not designed for SAP HANA Cloud.
 - **Proficiency:** Beginner

## Details
### You will learn  
You will create a database module with Core Data Services artifacts.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Create a database module)]

Back to your project, right click on your project and then click on `New->SAP HANA Database Module`:

![create db](1.png)

Name your module `db` and click on **next**

![create db](2.png)

Remove the namespace, add a name to the schema, click on **Build module after creation** and the click on **Finish**

![create db](3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a CDS artifact)]

You will now use Core Data Services to create a table. You will then use other entities to combine the data.

Begin by creating a folder under `db->src`:

![create folder](4.png)

Call it `data`:

![create folder](5.png)

Create a CDS artifact in your new folder

![create folder](6.png)

Call it `PO`

![create folder](7.png)


[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 3: ](Create your Entities using the CDS graphical editor)]

You can now explore the graphical Core Data Services editor briefly.

Right-click on the entity and choose **Graphical Editor**.

Double-click on the context to create an entity:

![create CDS](context.png)

**Click** on an entity and drop it in the editor:

![create CDS graphical](10.png)

Call it `APPROVAL_STATUS`:

![create CDS](11.png)

Double click on the node you have just added (inside the white rectangle) and click on the **+** sign to add a new field for your entity:

![add fields](12.png)

Create two fields as follows:

>Hint: If you haven't already, close the `Git` pane.

![create CDS fields](13.png)

**Save** and close the Graphical editor.

Open the **Text Editor** again by right-clicking on `PO.hdbcds`

![see text editor](open.png)

Copy the definition of the entity (blurred out below) and click on **Validate**:

![validate CDS definition](14.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Load data into your entity)]

You will now add data into your new entity. **Build** the db module first:

![Build db](15.png)

Create a comma-separated values file called `status.csv` in the `data` folder:

![create csv](16.png)

Add the following contents to it:

```text
ID,TEXT
I, In process
A, Approved
R, Rejected
```

**Save** the file.

![create csv](17.png)

Now you need to add a new file to indicate how that file loads your new table. Create a file called `load.hdbtabledata`

![create csv](new_file.png)


Add the following contents to it:

```json
{
    "format_version": 1,
    "imports": [
        {
            "target_table": "PO.APPROVAL_STATUS",
            "source_data": {
                "data_type": "CSV",
                "file_name": "status.csv",
                "has_header": true
            },
            "import_settings": {
                "import_columns": [
                    "ID",
					          "TEXT"
                ]
            }
        }
    ]
}     
```

**Save**:

![load csv](18.png)

 **Build** the module.

![Build module](build_db.png)

**Save and build** the `db` module.  Wait until the build finished to answer the following question.


[VALIDATE_2]

[ACCORDION-END]
