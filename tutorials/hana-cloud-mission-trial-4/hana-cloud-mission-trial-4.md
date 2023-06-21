---
parser: v2
author_name: Christopher Kollhed
author_profile: https://github.com/chriskollhed
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud\,-sap-hana-database, software-product>sap-business-application-studio]
primary_tag: software-product>sap-hana-cloud
---

# Tools to Manage and Access the SAP HANA Cloud, SAP HANA Database
<!-- description --> To get started with SAP HANA Cloud, SAP HANA database, you will need to use a few different tools. Learn here what you can use them for.

## Prerequisites
- You have access to [SAP HANA Cloud trial](hana-cloud-mission-trial-2) or [SAP HANA Cloud free tier](hana-cloud-mission-trial-2-ft), or a production environment of SAP HANA Cloud, SAP HANA database
- You have completed the tutorial to [provision an instance of SAP HANA Cloud, SAP HANA database](hana-cloud-mission-trial-3)


## You will learn
- How to use SAP HANA Cloud Central
- How to access SAP HANA cockpit
- How to access SAP HANA database explorer
- How to access SAP Business Application Studio
- How to obtain the SQL Endpoint of your instance



## Intro
>
> ![Alex Banner](banner-alex.png)
>
> Reminder: This tutorial is part of a mission, in which you will help Alex, the CEO of Best Run Travel, to answer a concrete business question with SAP HANA Cloud, SAP HANA database.
>
> *Alex needs to know the top 5 partners of their agency and wants to find out the days with maximum booking of each partner.*

In this tutorial, you will get to know the tools you need when interacting with your SAP HANA database in SAP HANA Cloud.

---

### Get to know SAP HANA Cloud Central


SAP HANA Cloud Central is your main administration area for all SAP HANA Cloud instances. Here you can provision new instances, scale your existing instances, deal with alerts and issues in existing instances, and more.

**How to open SAP HANA Cloud Central**

-	In SAP BTP cockpit, open SAP HANA Cloud Central under Instances and Subscriptions.

    ![BTP Manage SAP HANA Cloud](hcc-app.png)

-	SAP HANA Cloud Central will open in a new tab, where you can manage this instance.

**What you can do in SAP HANA Cloud Central**

-	*Get an overview of all SAP HANA Cloud instances in a subaccount*

-	*Create SAP HANA Cloud instances*

-	*Find an instance using the instance ID*

-	*Check the status of an instance*

-	*Review notifications*

-	*Check the memory, compute, and storage consumption*

-	*Start and stop instances*

-	*Manage and delete instances*

-	*Open the SAP HANA database instance in SAP HANA cockpit and SAP HANA database explorer*

-	*Open the SAP HANA Cloud, data lake instance in SAP HANA database explorer*

-   *View alerts in the Alerts tab*

-   *Run queries in the SQL console tab*


**How to find your instances**

-	In SAP HANA Cloud Central you can see all your instances. If you want to manage and maintain multiple instances, you can use the filters and search options on the top center area of the screen. Use **Adapt Filters** to modify the types of filters displayed.   

    ![HCC filters](hcc-filters.png)


**Manage your instances**

-	You can open many options by clicking on the **three dots** under the **Actions** column to each instance on the list. This includes options to manage configurations, start or stop the instance, or delete it. From this menu, you can also open the other tools you can use with your instances, such as SAP HANA database explorer.

-	One of the most important options you can get is the **SQL Endpoint** of your instance. To do so, click **Copy SQL Endpoint**. You will need this for multiple tasks, such as connecting to other systems.

    ![HCC SQL Endpoint](hcc-sqlend.png)

To learn about more options to create and manage instances with SAP HANA Cloud Central, you can refer to the [**SAP HANA Cloud Administration with SAP HANA Cloud Central Guide**](https://help.sap.com/viewer/9ae9104a46f74a6583ce5182e7fb20cb/LATEST/en-US/48e1b509c9494d61a6f90e7eaa6f225b.html).





### Get to know SAP HANA cockpit

SAP HANA cockpit is a tool that can be used to **monitor and manage SAP HANA databases**. Through the SAP HANA cockpit, for example, it is possible to monitor the amount of CPU and storage that is being used in your SAP HANA Cloud instance. Based on this information, you can make important decisions on how to manage your storage and optimize performance.

Another important feature of the SAP HANA cockpit is **User Management**, where you can create customized access roles and assign them to new users that should access the SAP HANA Cloud instance.

Key functionality from the SAP HANA cockpit is now being included in SAP HANA Cloud Central.

> **Two cockpits?**
>
> SAP HANA cockpit and SAP BTP Cockpit have similar names but are different tools for different purposes:
>
> - **SAP BTP Cockpit** is where you can globally manage instances of different SAP HANA Cloud components and your account, but it also contains many other cloud services.
>
> - **SAP HANA cockpit** is specific to SAP HANA Cloud, SAP HANA databases and is used to only monitor and manage SAP HANA databases in SAP HANA Cloud.    

**What you can do in SAP HANA cockpit**

In SAP HANA cockpit, you can monitor…

-	*overall database health*

-	*status and resource usage of individual database services*

-	*database performance across a range of key performance indicators related to memory, disk, and CPU usage*

-	*comparative memory utilization of column tables*

-	*memory statistics of the components of database services*

-	*alerts occurring in the database to analyze patterns of occurrence*

Furthermore, you can administer and manage…

-	*Security settings*

-	*Database users and roles*

-	*Replication services*

-	*Performance improvements*

**How to open SAP HANA cockpit**

1.	To open the SAP HANA cockpit, go to SAP HANA Cloud Central.

2.	Click on the row of the instance you want to view additional details for.

    ![Cockpit functionality within HANA Cloud Central](cockpit-hcc.png)

    If this is the first time you are accessing a database instance, you will need to **Choose Authentication** and enter the credentials of your DBADMIN user.

    ![Choose authentication](choose-auth.png)

    >Alternatively, you can access the SAP HANA cockpit by clicking on the **three dots** in the **Actions** column. Then, click on **Open in SAP HANA Cockpit**.
    >
    >![HCC Open HANA Cockpit](hcc-open-cockpit.png)
    >
    >SAP HANA cockpit will open on a new tab.
    >
    >![HANA Cockpit Overview](cockpit-db-overview.png)

You can check out the [technical documentation](https://help.sap.com/viewer/9630e508caef4578b34db22014998dba/LATEST/en-US/6a42679ed8574fb79e94f3e03e6d57bf.html)  on all details about SAP HANA cockpit.



### Get to know the SAP HANA database explorer

SAP HANA database explorer allows you to interact with SAP HANA databases, as well as use the SAP Graph and Spatial engines. You also have access to the SQL console.

>**Note**: The SQL console can now be accessed directly in HANA Cloud Central by clicking on the SQL console tab.
>
>![SQL console tab](sql-console-tab.png)
>
>After connecting to a database instance, you can execute SQL queries.
>
>![SQL console UI from HANA Cloud Central](sql-console-ui.png)
>
>Learn more about how to use the SQL console in HANA Cloud Central by visiting [this tutorial](hana-dbx-hcc).


**What you can do in SAP HANA database explorer**

![Generic DBX](ss-06-database-explorer-generic.png)

The SAP HANA database explorer offers a graphical interface and the SQL console, allowing you to freely access and manage your data.


In SAP HANA database explorer, you can:

-	*Browse the database catalog*

-	*Execute SQL statements*

-	*Debug stored procedures*

-	*Add, remove, or manage remote sources*

-	*Import, and export data*

-	*View diagnostic files*

If you want to view, add, or manage any of the catalog items, right click on the item and choose from the available options.

An important part of the SAP HANA database explorer is the **Catalog** browser. Each database on the SAP HANA database explorer has its own catalog that allows you to view data, execute SQL commands, and manage remote sources, among other tasks.

![Database Explorer Catalog](ss-07-database-explorer-catalog.png)

**How to open SAP HANA database explorer**

1.	Open SAP HANA Cloud Central.

2.	In the row of the SAP HANA Cloud database instance you want to open in SAP HANA database explorer, click on the **three dots** in the **Actions** column.

3.	Then, click on **Open in SAP HANA database explorer**.

    ![HCC Open DBX](hcc-open-dbx.png)

4.	The SAP HANA database explorer will open on a new tab. If this is the first time you are accessing it, you will need to enter the credentials of your DBADMIN user.

> In this mission, you will use the SAP HANA database explorer for many tasks, so we recommend you bookmark it for easy access.

For more information on how to use the SAP HANA database explorer, you can also check out the tutorial group [Get Started with the SAP HANA database explorer](group.hana-cloud-get-started) or refer to the [technical documentation](https://help.sap.com/viewer/a2cea64fa3ac4f90a52405d07600047b/LATEST/en-US/7fa981c8f1b44196b243faeb4afb5793.html).




### Get to know SAP Business Application Studio


SAP Business Application Studio is a development environment available for users with SAP HANA Cloud, SAP HANA database. There, you can create your development projects and model your data, including calculation views. This is also the tool you can use to build custom applications that connect and make use of your SAP HANA Cloud databases.

Using SAP Business Application Studio is not strictly necessary to use your trial instance, but if you would like to use calculation views and create applications it is strongly recommended. In this mission, you will learn to use it.

**What you can do in SAP Business Application Studio**

The SAP Business Application Studio provides tools specific to building business applications within the SAP ecosystem, covering the end-to-end development cycle. You can:

-	*Create development spaces*

-	*Clone an existing project*

-	*Create a new project using a template*

-	*Use editors for SAP-specific technologies*

-	*Test your application while consuming services from remote sources*

-	*Build and deploy you application as a multi-target application*  


> To use SAP Business Application Studio, you need be subscribed to this service within the SAP BTP Cockpit. You must also have Cloud Foundry enabled to add the SAP Business Application Studio entitlement to your trial account.
>
> If you are using a *trial account*, you can subscribe automatically via the **quick tool access**.
>
> If you are **not** using a trial account or you have added SAP HANA Cloud to an existing SAP BTP trial, you need to **subscribe manually**.
>
> Select the option that applies to you by clicking on the options below the step title.

[OPTION BEGIN [Quick tool access]]

**Quick tool access in trial**

1.	Go to the [SAP BTP Cockpit trial home page](https://account.hanatrial.ondemand.com/trial/#/home/trial).

    ![Trial Home Page Quick Access BAS](ss-10-Trial-home-page-quick-access-BAS.png)

2.	After logging in, click on the **SAP Business Application Studio** button under the **Quick Tool Access** area.

5.	A new tab will open with SAP Business Application Studio.

6.	Click **OK** to accept the privacy statement if this is your first time accessing SAP Business Application Studio.

7.	We recommend that you bookmark this URL so you can easily return to the SAP Business Application Studio.

    > You can learn more about the SAP Business Application Studio by visiting the documentation [here](https://help.sap.com/docs/bas/sap-business-application-studio/what-is-sap-business-application-studio).

[OPTION END]
[OPTION BEGIN [Subscribe manually]]

**Manually subscribe to SAP Business Application Studio**

1.	Navigate to your **Subaccount**.

2.	Click on **Service Marketplace** on the right side of the screen.

    ![BTP Marketplace](ss-11-BTP-marketplace.png)

3.	Scroll down or use the search bar to find **SAP Business Application Studio**.

4.	On the SAP Business Application Studio tile, click on the **three dots** and select **Create** to add a subscription. If you can see the option **Go to Application**, you are already subscribed.

    ![Add BAS in Marketplace](ss-12-add-BAS-marketplace.png)

5.  Click on **Users**.

    ![Users](users.png)

     Select the user that will be using the Business Application Studio and add the role collection **`Business_Application_Studio_Developer`**.

     ![Assign role collection](role-collection.png)

6.	Open the SAP Business Application Studio.

    ![Open BAS](start-bas.png)


7.	Click on **OK** to accept the privacy statement if this is your first time accessing SAP Business Application Studio.

8.	We recommend that you bookmark this URL so you can easily return to the SAP Business Application Studio.

    > You can learn more about SAP Business Application Studio [here](https://help.sap.com/docs/bas/sap-business-application-studio/what-is-sap-business-application-studio).

[OPTION END]

Well done!

You have completed the third tutorial of this mission! Now you know how to access the tools you need to make the best use of your SAP HANA Cloud, SAP HANA database instances. Learn in the next tutorial how to import data into your SAP HANA Cloud database.




### Knowledge Check






---
