---
title: Creating basic/interface views in Open Core Data Services
description: Creating basic view  in Open Core Data Services also know as interface view
tags: [  tutorial>beginner ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Setting and starting an S4/HANA CAL instance and connecting to a Window instance on Amazon Web Services. ](http://go.sap.com/developer/tutorials/hana-setup-cloud.html)

## Next Steps
[Creating consumption view based on basic/interface view. ](http://go.sap.com/developer/tutorials/s4hana-cds-creating-consumption-view.html)


## Details
You will learn  
- How to create a basic view on top of data content in the  demo table SCARR from ABAP system.

### Time to Complete
 Beginners might take **15-25 minutes** to execute this tutorial.

---

1. To prepare the ABAP perspective, make sure that your Eclipse framework from the S/4HANA instance is opened in the ABAP perspective:

    ![Basic view from the ABAP perspective](BasicView1_Abapperspective.png)

2. With the ABAP Eclipse perspective displayed, make sure you are in the Project Explorer tab. Here you will create in the `TMP-SHA` Package a new repository ABAP object for type DDL source.

    Select the package `TMP-SHA`. Right-click on the selected package, select `New` and then press `Others ABAP Repository Objects`, in the opened  ABAP Repository Object window scroll down to the folder `Core Data Services`. Expand the folder `Core Data Services`, select `DDL Source` and press `Finish`.  

    ![ Basic view](BasicView2.png)

    ![Basic view](BasicView3.png)

3. In the new **DDL Source (Specify a value for field Name)** window, confirm the values for **Project** and **Package** are as shown in the table below. .

    Field Name        | Value
    :---------------- | :-------------
    Project           | `S4H_100_sha_en`
    Package           | `$TMP`


    ![Basic view](BasicView4.png)

    Enter the values for **Name** and **Description** then click **Finish**
    
    Field Name        | Value
    :---------------- | :-------------
    Name              | `ZXSHI_AIRLINE`
    Description       | `Northwind OData Service`
    
    ![Basic view](BasicView5.png)

4. In the **New DDL Source (Templates Select one of the available templates)** window, select **Define View** and press **Finish**.

    ![Basic view](BasicView6.png)

5. Code for the new created view named `ZXSHI_AIRLINE` is generated:

    ![Basic view](BasicView7.png)

6. At the line 5 of the generated code, replace `data source name` with the ABAP table `scarr`.  To achieve this delete  the text `data source name` and then enter `sca` followed by the combination `Crtl + Space bar`. Doing so a window will appear when you will select `scarr-data`.

    ![Basic view](BasicView8.png)

7. Open the ERP System (by clicking the SAP Log On icon) `S4H Client 100`.
Enter the User and Password values below and click **Enter**.

    Field Name        | Value
    :---------------- | :-------------
    Name              | `sha`
    Description       | `Welcome1`
    
    ![Basic view](BasicView9.png)   

8. In the next window enter the ABAP transaction `se16` and afterward press the `Enter` button.

    ![Basic view](BasicView10.png)

9. In the next window enter `scarr` as Table Name and press the Table Contents icon.

    ![Basic view](BasicView11.png)

    The content of the table `scarr` will be displayed. Confirm that the table has 18 entries.

    ![Basic view](BasicView12.png)

10. Now switch back to the project explorer view of the Eclipse ABAP perspective. You will now enhance the code by inserting the columns of the table `scarr`. To achieve this, position the mouse pointer before the bracket near the table name `scarr`, press `Ctrl + Space bar`. Within the displayed window select **Insert all elements** and click **Enter**.

    ![Basic view](BasicView13.png)

11. All the columns of the table `scarr` are now inserted in the previous code. Enhance each column with an Alias as following:

    Line number       | Value
    :---------------- | :-------------
    Line 6:           | `scarr.carrid` as `Airline`,
    Line 7:           | `scarr.currcode` as `AirlineLocalCurrency`
    Line 8:           | `scarr.url` as `AirlineURL`

12. Expand the folder **`$TMP-SHA`** in the left panel, then  expand its subfolder **`Core Data Services`**. Doing so, you will see in the subfolder **`Data Definitions`** the newly created basic view named **`ZXSHI_AIRLINE`**.

    ![Basic view](BasicView14.png)

13. To display the content of the basic view `ZXSHI_AIRLINE`, in the left panel, select and right-click on the view **`ZXSHI_AIRLINE`**, select **Open With >  Data Preview**.  

    ![Basic view](BasicView15.png)

14. In the newly opened window, the content of the basic view **`ZXSHI_AIRLINE`** is displayed , showing 18 rows in three columns.

    ![Basic view](BasicView16.png)

15. Switch back to the generated code of the basic view. There you will enhance the lines marked in blue in the image below.

     ![Basic view](BasicView17.png)

16. In the code area, replace texts as follows:

    Line number       | Value
    :---------------- | :-------------
    Line 1:           | `sql_view_name` with `ZXSHIAIRLINE`
    Line 4:           | `Airline, private..` with `Airline`

    After Airline, insert a new annotation as follows: 
    
    ```abap
    @VDM.view.Type:#BASIC
    ```

    ![Basic view](BasicView18.png)

    ![Basic view](BasicView19.png)

    ![Basic view](BasicView20.png)

    ![Basic view](BasicView21.png)

17. After line 5, insert line 6 with a new annotation:

    ```abap
    @Analytics: DataCategory: #DIMENSION
    ```

    ![Basic view](BasicView22.png)

    ![Basic view](BasicView23.png)

    ![Basic view](BasicView25.png)

18. Enhance the code by inserting two new annotations as follows:

    - (1) after the line including `scarr.carrid` as `Airline` insert `@Semantics.currencyCode: true`
    - (2) after the line including `scarr.currcode` as `AirlineLocalCurrency` insert `@Semantics.url: true`

     ![Basic view](BasicView26.png)

19. In the line 3 of the code, replace `CHECK` with `NOT_REQUIRED`

    ![Basic view](BasicView27.png)

20. In the right panel, select and right-click on the view **`ZXSHI_AIRLINE`**, select **Open With > Data Preview**

    ![Basic view](BasicView28.png)

    ![Basic view](BasicView29.png)

    > Note: You can copy this content from here and paste it to the code editor of your ABAP perspective as well instead of enhancing the generated code on your own:

    ```abap
    @AbapCatalog.sqlViewName: 'ZXSHIAIRLINE'
    @AbapCatalog.compiler.compareFilter: true
    @AccessControl.authorizationCheck: #NOT_REQUIRED
    @EndUserText.label: 'Airline'
    @VDM.viewType: #BASIC
    @Analytics.dataCategory: #DIMENSION
    define view ZxshI_Airline as select from scarr{
    key scarr.carrid as Airline,
    @Semantics.currencyCode: true
    scarr.currcode as AirlineLocalCurrency,
    @Semantics.url: true
    scarr.url as AirlineURL    
    }
    ```

**Notes**
> Although SAP offers trial editions for free you will still have to cover the costs for running these trial editions on AWS!

### Related information
This tutorial is part of the S/4HANA Core Data Services

1. [Amazon Web Services](http://aws.amazon.com/)
2. [SAP Cloud Appliance Library (CAL)](https://scn.sap.com/community/cloud-appliance-library)
3. [Alternative AWS Deployment for SAP Trials provided as Virtual Appliance](https://scn.sap.com/docs/DOC-46908)
4. [Virtual Private Cloud with VPN Access for SAP Trials provided as Virtual Appliance](https://scn.sap.com/docs/DOC-46629)
