---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-fiori-tools]
primary_tag: products>sap-fiori
---

# Refine the Object Page with Annotations
<!-- description --> Display data in fields and tables on the object page using annotations.

## Prerequisites
- You have prepared your OData service as described in the previous tutorial [Prepare the RAP-Based Travel Service](fiori-tools-rap-prepare-service).
- Ensure that you have finished all steps in previous tutorials:
  [Prepare the RAP-Based Travel Service](fiori-tools-rap-prepare-service)
  [Generate an SAP Fiori Elements Application based on a RAP-based Service](fiori-tools-rap-create-application)
  [Refine the List Report with Annotations](fiori-tools-rap-modify-list-report)

## You will learn
  - How to add a title and a subtitle to the object page header
  - How to add key data to the object page header
  - How to add a section and use field groups to structure data
  - How to add a table
  - How to include pictures in a table
  - How to modify an SAP Fiori elements application to add features like flexible column layout and  initial load of data
  - How to update the metadata in your frontend application

## Intro
>Whenever your unique suffix for creating objects is needed, the object names within this tutorial are named with suffix "######". For the screenshots the suffix "000100" was used.

---

### Add title and subtitle to object page header


Clicking on any item of the list report will show the object page for this item. The object page currently shows only some standard buttons and does not contain any further fields or actions.
 In this step you will add annotations to show a title and a subtitle in the object page header.

1. Open the metadata extensions for the Travel view `ZC_FE_TRAVEL_######`. In the previous tutorial [Refine the List Report with Additional Annotations](fiori-tools-rap-modify-list-report) you already defined header information in step 3. Now you will enhance the annotation `@UI.headerInfo` with a `title` and a `description` property.

    ```CDS
    ...

    @UI: {
        headerInfo: {
            typeName: 'Travel',
            typeNamePlural: 'Travels',
            title: {
                type: #STANDARD, value: 'Description'
            },
            description: {
                value: 'TravelID'
            }
        },
    ...

    ```

    Choose **Save** and **Activate**.

2. Refresh the app preview. You will see an object page containing a title and a subtitle in the header and an empty content section.

    <!-- border -->![App object page title](t3-app-object-page-title.png)




### Add data points to object page header


In this step you will add some key information to the object page header using data points.

1. As in the steps before, open the metadata extensions file for the Travel view `ZC_FE_TRAVEL_######`.

    Add the `@UI.facet` annotation with two objects of type `#DATAPOINT_REFERENCE`.
    ```CDS
    ...
    annotate view ZC_FE_TRAVEL_###### with
    {

        @UI.facet: [
          {
              id: 'TravelHeaderPrice',
              purpose: #HEADER,
              type: #DATAPOINT_REFERENCE,
              position: 10,
              targetQualifier: 'PriceData'
          },
          {
              id: 'TravelHeaderOverallStatus',
              purpose: #HEADER,
              type: #DATAPOINT_REFERENCE,
              position: 20,
              targetQualifier: 'StatusData'
           }
      ]

      @UI.lineItem: [{ position: 10}]
      TravelID;  
      ...      
    ```

2. Annotating properties `TotalPrice` and `OverallStatus` with `@UI.datapoint` using the `targetQualifier` from the facet definition in step 1 will assign the properties to the header facet accordingly.

    ```CDS
    ...

    @UI.lineItem: [{ position: 70}]
    @UI.dataPoint: { qualifier: 'PriceData', title: 'Total Price'}
    TotalPrice;

    @UI.lineItem: [{ position: 80, criticality: 'OverallStatusCriticality' }]
    @UI.selectionField: [{ position: 30}]
    @UI.textArrangement: #TEXT_ONLY
    @UI.dataPoint: { qualifier: 'StatusData', title: 'Status', criticality: 'OverallStatusCriticality' }
    OverallStatus;

    ...

    ```
    Choose **Save** and **Activate**.

3. Refresh the app preview. The two new data points show up in the object page header. The labels are taken from property `title`, the color of **Status** from property `criticality` of the `datapoint` annotations .

    <!-- border -->![App Data Points](t3-app-data-points.png)



### Add new section with title


In this step you will add a section to the content area of the object page. The section will contain a form with three data fields.

1. Open the metadata extensions file for the Travel view `ZC_FE_TRAVEL_######` and enter the facet annotations that define the section **General Information** as a collection facet, using the type `Collection`. Add a second facet as a child of **General Information** with facet type `#IDENTIFICATION_REFERENCE` to create a form with title **General**. Add the code from line 8 to line 21 to your existing UI facet definition.


    ```CDS
    ...
    annotate view ZC_FE_TRAVEL_###### with
    {

      @UI.facet: [
      ...

        {
          label: 'General Information',
          id: 'GeneralInfo',
          type: #COLLECTION,
          position: 10
        },
        {
          label: 'General',
          id: 'Travel',
          type: #IDENTIFICATION_REFERENCE,
          purpose: #STANDARD,
          parentId: 'GeneralInfo',
          position: 10
        }
      ]

    ...

    }
    ```

2. Add a new property `Description` and annotate this and the properties `AgencyID` and `CustomerID` with `@UI.Identification` to position these fields under **General**.

    ```CDS
    annotate view ZC_FE_TRAVEL_###### with
    {

    ...

      @UI.lineItem: [{ position: 10}]
      TravelID;

      @UI.identification: [{ position: 10 }]
      Description;

      @UI.lineItem: [{ position: 20}]
      @UI.selectionField: [{ position: 10}]
      @UI.identification: [{ position: 30 }]
      AgencyID;

      @UI.lineItem: [{ position: 30}]
      @UI.selectionField: [{ position: 20}]
      @UI.identification: [{ position: 20 }]
      CustomerID;
    ...
    }
    ```

    Choose **Save** and **Activate**.

3. Refresh the app preview. The new form **General** is shown in section **General Information** containing the three fields.

    <!-- border -->![App section GeneralInfo](t3-app-section-General-Information.png)



### Add 2 field groups to section


A field group contains one or more data fields inside a UI container. In this step you define two field groups in the section **General Information**.

1. Open the metadata extensions file for the Travel view `ZC_FE_TRAVEL_######`.

    First, define a field group for the beginning and end date of a travel item and for the prices. The facet type for a field group is `#FIELDGROUP_REFERENCE`. Add the code from line 8 to line 25 to the end of the `@UI.facet` section.

    ```CDS
    annotate view ZC_FE_TRAVEL_###### with
    {

      @UI.facet: [
        {

        ...
        {
          id: 'Dates',
          purpose: #STANDARD,
          type: #FIELDGROUP_REFERENCE,
          parentId: 'GeneralInfo',
          label: 'Dates',
          position: 30,
          targetQualifier: 'DatesGroup'
        },
        {
          id: 'Prices',
          purpose: #STANDARD,
          type: #FIELDGROUP_REFERENCE,
          parentId: 'GeneralInfo',
          label: 'Prices',
          position: 20,
          targetQualifier: 'PricesGroup'
        }
      ]
    ...

    }
    ```

2. Annotate the properties `BeginDate` and `EndDate `with `@UI.fieldGroup`. Make sure you use the same field group qualifier `DatesGroup` but different positions in each annotation. Apply the same for the properties `BookingFee` and `TotalPrice` using field group annotations with qualifier `PricesGroup`.

    ```CDS
     ...

     @UI.lineItem: [{ position: 40}]
     @UI.fieldGroup: [{ qualifier: 'DatesGroup', position: 10 }]
     BeginDate;

     @UI.lineItem: [{ position: 50}]
     @UI.fieldGroup: [{ qualifier: 'DatesGroup', position: 20 }]
     EndDate;

     @UI.lineItem: [{ position: 60}]
     @UI.fieldGroup: [ { qualifier: 'PricesGroup', position: 10} ]
     BookingFee;

     @UI.lineItem: [{ position: 70}]  
     @UI.fieldGroup: [{ qualifier: 'PricesGroup', position: 20 }]
     TotalPrice;

     ...
    ```

    Choose **Save** and **Activate**.

3. Refresh the app preview. There are two additional field groups showing price and date information.

    <!-- border -->![App field Groups](t3-app-field-groups.png)




### Show Bookings Table in new section


In this step you will add a new section that contains a table with booking information. This requires access to another entity `Booking` via an association and an additional metadata extensions file.

1. Open the metadata extensions file for the Travel view `ZC_FE_TRAVEL_######`. In the facet annotation block, add a new facet `Booking` with type `#LINEITEM_REFERENCE`. Add the code from line 7 to line 14 to the end of the `@UI.facet` section. Choose **Save** and **Activate**.

    ```CDS
    ...
    annotate view ZC_FE_TRAVEL_###### with
    {
      ...
      @UI.facet: [
        ...
        {
          id: 'Booking',
          purpose: #STANDARD,
          type: #LINEITEM_REFERENCE,
          label: 'Bookings',
          position: 20,
          targetElement: '_Booking'
        }
      ]
    ...

    ```
    The property `targetElement: _Booking` references the association to the booking table that will be shown in the booking section. You can look up the definition of Booking in the projection view of Travel `ZC_FE_TRAVEL_######`.

2. In the project explorer open the folder `Data Definitions`, right-click on projection view `ZC_FE_BOOKING_######` and create a new metadata extensions file from the context menu. Enter **`ZC_FE_BOOKING_######`** as name and **`Metadata Extension for Booking view`** as description.

    <!-- border -->![add Metadata Extension for Booking](t3-booking-metadata-extension-popup.png)

    Choose **Next** and then **Finish**.

3. In the metadata extensions file `ZC_FE_BOOKING_######` use `@UI.lineItem` annotations to add some fields from the Booking view `ZC_FE_BOOKING_###### Projection View for Booking` to the bookings table. Replace the content of `ZC_FE_BOOKING_######` by the following code:

    ```CDS
    @Metadata.layer: #CORE

    annotate view ZC_FE_BOOKING_######
      with
    {
        @UI.lineItem: [ { position: 10 } ]
        BookingID;

        @UI.lineItem: [ { position: 20 } ]
        BookingDate;

        @UI.lineItem: [ { position: 30 } ]
        CustomerID;

        @UI.lineItem: [ { position: 40 } ]
        CarrierID;

        @UI.lineItem: [ { position: 50 } ]
        ConnectionID;

        @UI.lineItem: [ { position: 60 } ]
        FlightDate;

        @UI.lineItem: [ { position: 70 } ]
        FlightPrice;

    }
    ```
    Choose **Save** and **Activate**.

4. Refresh the app preview. The booking table is now displayed in the new **Bookings** section of the object page.

    <!-- border -->![App booking table](t3-app-booking-table.png)


5. Instead of showing IDs for the fields Customer ID and Airline ID, one would preferably show descriptions or names.

    This will be made possible by using specific annotations which are implemented within the projection view `ZC_FE_BOOKING_######`. Therefore, open the projection view which contains the root view definitions for the booking entity.

    Add the `@ObjectModel` and `@EndUserText` annotations to the fields as shown in the coding fragments below.

    Annotation `@EndUserText.label` defines the column label for the related fields. Using annotation `@ObjectModel.text.element` controls the source of the content shown for the related field. Fields `CarrierID` and `CustomerID` will get their content through the corresponding association.

    ```CDS
    @EndUserText.label: 'Customer'
    @ObjectModel.text.element: ['LastName']
    CustomerID,
    _Customer.LastName as LastName,
    ```

    ```CDS
    @EndUserText.label: 'Airline'
    @ObjectModel.text.element: ['CarrierName']
    CarrierID,
    _Carrier.Name as CarrierName,
    ```
  Choose **Save** and **Activate**.

6. Refresh the app preview. The booking table is now displayed in the new **Bookings** section of the object page with descriptions for **Customer** and **Airline**.

    <!-- border -->![App booking table](t3-app-booking-table-descriptions.png)



### Add airline pictures in Bookings table

In this step you will add the airline logo in a new column at the beginning of the booking table.

1. To achieve this, open the metadata extensions file `ZC_FE_BOOKING_######` and add the following code lines to the annotation structure.

    ```CDS
        ...
        @UI.lineItem: [ { position: 05, label: ' ', value: '_Carrier.AirlinePicURL' } ]
        _Carrier;
        ...
    ```

    Choose **Save** and **Activate**.

2. Refresh the app preview. The booking table is now displayed with the airline logo in the first column.

    <!-- border -->![App airline logos](t3-app-booking-airline-logos.png)



### Activate flexible column layout


With the flexible column layout you can have the list report and the object page open at the same time without the need to navigate back and forth between them.

1. In the SAP Business Application Studio open the context menu by right clicking on your `webapp` folder and select the menu entry **Show Page Map**.

    <!-- border -->![Right-click on webapp folder](SetFCL_1a.png)

2. In the left area of the page map you see the UI structure of your application listing the tiles of the list report and the object page. In the right area you can see the page settings. Choose option **Flexible Column Layout** and then select the **Mid-Expanded** option for the two columns layout. Leave the default for the three columns layout unchanged.

    <!-- border -->![The Page Map with page map and layout settings](SetFCL_2a.png)

3. Refresh the application and choose **Go** to load data into the list report table. Select any of the items within the table to open the object page.

    <!-- border -->![Click go on list report after refresh](SetFCL_3.png)

    Now the object page is displayed together with the list report. When you select another item in the list report, the object page is updated.

    &nbsp;



### Activate initial loading of data


As a last step of the tutorial you will activate the initial load feature that will trigger the loading of data within the list report automatically, i.e. without having to choose **Go**.

1. Open the page map once again as shown in the previous step by choosing the **Show Page Map** option in the context menu of your application folder.

2. In the UI structure of your application switch to edit mode of the **List Report** tile.

    <!-- border -->![Configure Page icon on list report block](InitialLoad_1a.png)

3. Now the structure of the list report is shown. Open the properties of the list report table by selecting the **Table** component.

    <!-- border -->![Click on table in page editor](InitialLoad_2a.png)

4. In the list of table properties select the value **Enabled** for the table property **Initial Load**. This setting is immediately active without the need of any confirmation.

    <!-- border -->![Select true for initial load in drop down field](InitialLoad_3a.png)

5. Your application is updated and you will see that the data of the list report table is loaded immediately without choosing **Go**.

    <!-- border -->![Restart the app and see the initial load of data](InitialLoad_4.png)

    &nbsp;



### Update the metadata in your frontend application

You will need to update the local copy of the metadata in order to work with the annotations locally in Fiori tools extensions such as XML Annotation LSP, Guided Development or Page Editor. 

>As long as your application preview is based on the real service metadata, you will see the changes you made in the backend without refreshing the local metadata.

You will now learn how to update the metadata with the next steps.

1. Switch to the SAP Business Application Studio with your generated application from second tutorial [Generate an SAP Fiori Elements Application based on a RAP-based Service](fiori-tools-rap-create-application). If the application preview is already running in other browser tab, then you do not need to stop it. 
   
    From the hamburger menu, open **View->Command Palette...**, type **`Service Manager`**, and select **Fiori: Open Service Mananger**.

    In the main service row click the `edit button`.

    <!-- border -->![Start Service Manger](ServiceManager_1.png)

2. Choose **Refresh** to update local metadata of your application.
   
    <!-- border -->![Refresh Service](ServiceManager_2.png)
   
    The Service Manager has updated the service metadata in the application’s **webapp/localService** folder.



---

Over the past four tutorials, you have used the SAP ABAP RESTful Application Programming Model, SAP Fiori tools and SAP Fiori elements for OData V4 to build this application. You have learned how to:

- Use the wizard-style approach of SAP Fiori tools to generate an application based on an existing service

- Refine a RAP based service with additional annotations to improve the user interface

- Configure the application using the Page Map and Page Editor
  
- Refresh the local copy of service metadata in SAP Business Application Studio with Service Manager



All of these tools (and more) can be used with any of the available SAP Fiori elements page types. Enjoy your future projects!

---
