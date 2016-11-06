---
title: Core Data  Services, Preparing views for Associations/Joins
description: Core Data  Services, Preparing views for Associations/Joins
tags: [  tutorial>beginner ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Creating basic view. Creating consumption view. ](http://go.sap.com/developer/tutorial-navigator.html)

## Next Steps
[Creating views association. ](http://go.sap.com/developer/tutorials/s4hana-cds-associations.html)


## Details
You will learn  
- How to prepare a basic view for Association, meaning for joining with other views.

### Time to Complete
 Beginners might take **15-25 minutes** to execute this tutorial.

---

**Preparing the ABAP perspective**

1. Make sure that your Eclipse Framework from the S/4HANA instance is opened in the ABAP perspective and that the basic view (`ZXSHI_AIRLINE`) from the previous S/4HANA tutorials is available:

     ![The prerequisite](Prerequisite01.png)

2. You are now in the ABAP Eclipse perspective. Make sure you are in the Project Explorer tab. Here we will create in the `TMP-SHA` Package a new ABAP object for type DDL source.
Select the package `TMP-SHA`, expand it. After doing that, expand the folder `Code Data Services` as well. From here select the subfolder `Data Definitions`(`1`).  Right-click on the subfolder Data Definitions (`2`),  select `DDL Source` and press `Finish`.  

    ![Create DDL](BuildDDL_02.png)


3. In the new window DDL Source (Create a DDL Source), Project and Package are name respectively as `S4H_100_sha_en` and `$TMP`. Enter `Airline text, private view, VDM interface view`  as Description and `ZXSH_I_AIRLINETEXT` as Name and press `Finish`.

    ![Create the DDL view](BuildDDL_03.png)

    ![Create the DDL view](BuildDDL_04.png)

**Generating a Code for the view**

4. A code for the new created view named `ZXSH_I_AIRLINETEXT` is generated:

    ![Generate the code](GeneratedCode_05.png)

5. In the generated code, we will replace `sql_view_name name`(`1`) with  `ZXSH_IAIRLINET`. Delete line 3 (`2`). Replace `#CHECK` with `#NOT_REQUIRED`(`3`), `Airline text ....` with `Airline Name` (`4`), `data source name` with `scarr` (`5`)

    ![Code enhancement](ReplaceWith_06.png)

    ![Code enhancement](ReplaceWith_6_1.png)


**Adding Annotations (Part I)**

- Let's consider our generated coding again. We will enhance the code by inserting new annotations:
``` abap
      @ObjectModel.dataCategory: #TEXT
```
(`1`) and (`2`) after line (`4`).

    ![Set the data category text](DataCategoryTEXT_08.png)

    ![Set the data category text](DataCategoryTEXT_10.png)

 ``` abap
      @Analytics.dataExtraction.enabled: true
```

  ![Annotation](AnnotationDataExtraction_11.png)

  ![Set the annotation equal  2](AnnotationDataExtractionTrue_12.png)

 ``` abap
      @VDM.viewType: #BASIC`
 ```
  ![Set the annotation view type](AnnotationViewType_13.png)

  ![Set the view type equal Type Basic](AnnotationViewTypeBASIC_14.png)

- In the generated code, we will modify the view name after the `define view`statement as following:  
Change `Zxsh_I_Airlinetext` with `ZXSH_I_AirlineText`

 ![Modify the view name](ViewNameUpperCase_15.png)

- We will enhance the generated code with assignment of the following key columns:

 ``` abap
     key carrid as Airline,
  key spras as Language,
 ```    

  ![Assign the key column](KeysColumnsAssignment_16.png)


**Adding Annotations to the select statement (Part II)**   

- Let's enhance our generated coding again by inserting following new annotations:

  - In the line after the expression `key carrid...` insert:
``` abap
       `@Semantics.language: true`
```
      ![Enhancement with the annotation for language ](AddAnnotationSemanticsForLanguage_17.png)

  - In the line after the expression   `key spras ...` insert:
    ``` abap
       `@Semantics.text: true`
    `@Semantics.text: true`
    ```
       ![Enhancement with the annotation for text and end user](SemanticsTextAndEndUserAnnotation_19.png)

       ![Enhancement with the annotation for text and end user](SemanticsTextAndEndUserAnnotation_20.png)

**Prefix the columns name with table names**

-  We will enhance the generated coding by prefixing the columns name with the ERP table name `scarr`:

  - Line 9:  key `scarr.carrid` as Airline,
  - Line 14: `scarr.carrname` as `AirlineName`

      ![Insert table name ](InsertTableName_21.png)

      ![Insert table name](InsertTableName_22.png)

**Adding Annotation for Object representative key**

- We will insert after line 7 an additional annotation for object representative key:

 `@ObjectModel.representativeKey: 'Airline'`

    ![Annotation for object representative key](AnnotationObjectRepresentativeKey_26.png)

- The inserted annotation for the language was just to demonstrate the possibilities  we have with annotations. Since we only have one language  it won't make sense to use this annotation and therefore comment out the lines 11(`@Semantics.language: true`) and 12 (`key spras as language`) . Save and activate the generated enhanced code. The final version should be identical to this:

    ![Coding](FinalTextViewCode_25.png)

**Display the content of the text view `ZXSH_I_AIRLINETEXT`**

- We can now preview the content of the created text view. On the left panel, select and right-click on the view `ZXSH_I_AIRLINETEXT`, select `Open Data Preview` and then press `Enter`

    ![Preview the data](TexViewDataPreview_27.png)

**Help:** You can copy this content from here and paste it to the code editor of your ABAP perspective as well instead of enhancing the generated code on your own by tipping:

``` abap
@AbapCatalog.sqlViewName: 'ZXSH_IAIRLINET'
@ClientDependent: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Airline Name'
@ObjectModel.dataCategory: #TEXT
@Analytics.dataExtraction.enabled: true
@VDM.viewType: #BASIC
@ObjectModel.representativeKey: 'Airline'
define view ZXSH_I_AirlineText as select from scarr {
 key scarr.carrid as Airline,
//  @Semantics.language: true
//  key spras as Language,  
 @Semantics.text: true
 @EndUserText.label: 'Airline Name'
 scarr.carrname as AirlineName
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
