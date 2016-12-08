---
title: SAP HCP predictive services, Expose your predictive demo dataset as an OData service
description: Expose the demo data to be used with the SAP HCP predictive services as an OData services
tags: [ tutorial>beginner, products>sap-hana, products>sap-hana-cloud-platform, topic>odata ]
---

## Prerequisites
  - **Proficiency:** Beginner
  - **Tutorials:** [Import a predictive demo dataset in your SAP HANA](http://www.sap.com/developer/tutorials/hcpps-hana-dataset-import.html)

## Next Steps
  - [Enable, deploy and configure the SAP HCP predictive services](http://www.sap.com/developer/tutorials/hcpps-ps-configure.html)

## Details
### You will learn
  - How to use the create an ***OData*** service to expose a table stored in the HANA HCP instance

### Time to Complete
  **5 minutes**

---

1. Open the ***SAP HANA Web-based Development Workbench*** on your trial HANA instance connected `HANA User Account`, click on **Editor**.

    ![SAP HANA Web-based Development Workbench](1.png)

1. Right click on **Content**, then click on **Create Application**.

    ![SAP HANA Web-based Development Workbench](2-1.png)

1. Complete the form following properties:

    - ***Package***: `public.timeseries.odata`

    Click on **Create**

    ![New Application](5.png)

1. Expand the tree structure and reach the `public.timeseries.odata` package.

    Right click on `odata`, then navigate the menu **New** > **File**.

    ![SAP HANA Web-based Development Workbench](6.png)

1. Complete the form following properties:

    - ***File Name***: `timeseries.xsodata`

    Click on **Create**

    ![New Package](7.png)

1. Enter the following code in the file:

    ```
    service {
      "DEMO"."TIME_SERIES" as "TimeSeriesData";
    }
    settings {
      support null;
    }
    ```

    This is the OData Service declaration for the `TIMESERIES` table located in the `DEMO` schema which we will now reference as `TimeSeriesData`

    Click on the ![save](0-save.png) button in the top menu bar

    ![XS OData](8.png)

1. You can now test your service by clicking on the ![run](0-run.png) button in the top menu bar.

    This will list all the services defined in your `xsodata` file.

    ```xml
    <service xml:base="https://trialXYZtrial.hanatrial.ondemand.com:443/public/timeseries/odata/timeseries.xsodata/">
      <workspace>
        <atom:title>Default</atom:title>
        <collection href="TimeSeriesData">
          <atom:title>TimeSeriesData</atom:title>
        </collection>
      </workspace>
    </service>
    ```

    ![OData Service List](9.png)

    Later, you will be using this OData service, so you should save the highlighted URL in a notepad as your ***OData Service URL***.

    Note that the URL include your HCP Account Identifier (represented as `XYZ`).

## Next Steps
  - [Enable, deploy and configure the SAP HCP predictive services](http://www.sap.com/developer/tutorials/hcpps-ps-configure.html)
