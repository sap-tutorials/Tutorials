---
parser: v2
author_name: Ramakrishnan Raghuraman
author_profile: https://github.com/r3ksk
auto_validation: true
time: 30
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

# Build Your First Automation Using Excel SDK of SAP Process Automation
<!-- description --> In this tutorial, you will be developing basic automations using Excel SDK, In the end, you know how to read Excel data for a given range using Excel Cloud Link, or using a traditional approach. In this tutorial, you will also learn, how you can apply filter on the Excel data, copy the filtered data onto a new Excel sheet and save the file with a new name.


## Prerequisites
- Complete the tutorial: [Subscribe to SAP Process Automation Using Booster in SAP BTP Free Tier](spa-subscribe-booster)
- [Install and Setup the Desktop Agent](spa-setup-desktop-3-0-agent)
- Basic understanding of SAP Process Automation

## You will learn
  - How to use Excel SDK of SAP Process Automation (SPA) to read and process data in an Excel

---

### About Excel SDK

SAP Process Automation has native integration to several Microsoft Office products including Outlook and Excel SDK. In this Tutorial, you will explore how to use Excel SDK to your automations.

The Excel SDK is a collection of activities allowing you to create automations using Microsoft Excel.
It comes with rich set of functionality. For example,

- Create new Excel Workbook
- Create new Excel Worksheet
- Read and write data from an Excel sheet, for a given data range
- Apply Filter on the a column
- etc

To design your automation, you will need an Excel file filled with the sales orders data. You have the possibility to create it yourself using the below sample data:

| Order Number| Order Amount | Order Date | Shipping Country         | Expected Delivery Date | Order Status
|  :----------| :------------|:-----------| :------------------------|:-----------------------|:-----------
|  PO7991     | 410418.22    | 1/21/2022	| United States of America | 1/29/2022	            | In Time
|  PO7918     | 150935.13	   | 1/22/2022	| United Kingdom           | 1/27/2022	            | Urgent
|  PO7375     | 313977.82	   | 1/23/2022	| United Kingdom	         | 2/20/2022              | In Time
|  PO7311     | 755055.4	   | 1/24/2022	| United Kingdom	         | 3/30/2022              | In Time
|  PO6858     | 429358.4     | 1/25/2022	| United Kingdom	         | 2/20/2022	            | In Time
|  PO6368     | 43739.82	   | 1/26/2022	| United Kingdom	         | 3/25/2022	            | In Time
|  PO6189     | 483574.12	   | 1/27/2022	| Germany	                 | 2/5/2022	              | In Time
|  PO3115     | 2739956	     | 1/28/2022	| Germany	                 | 3/10/2022	            | In Time
|  PO2686     | 220887.56	   | 1/29/2022	| Germany	                 | 3/5/2022	              | In Time
|  PO8282     | 436955.64	   | 1/30/2022	| United States of America | 3/30/2022	            | In Time

or you can use an Excel sample of your choice


### Use Excel Link in your Excel Automations


1.  From the SAP Process Automation Lobby, create a new automation.
2.  Provide an appropriate name to your project and relevant automations.
3.  Select your active desktop agent version to be used during design and development of your automation.
4.  By Default Excel and Core SDK are added to the automations and your automation opens in a new browser tab. You have couple of ways to read Excel Data.
      - Using the activity **Excel Cloud Link** to achieve Excel Data Mapping
      - Traditional step by step approach to read / write Excel data

      In this tutorial, you will explore both ways.

      **What is Excel Data Mapping?**

      Excel Data Mapping allows you to transform columns-based data from an Excel sheet into datatype that can be used in your automation. The data from the Excel sheet stays the same but the structure becomes a data type structure, making it possible to use throughout your project.

5.  As a first step, Locate and add **Open Excel Instance** activity to the automation flow
    <!-- border -->![Add Open Excel Instance](01-AddOpenExcelInstance.png)

6.  Similarly locate and add **Excel Cloud Link** from the activity pane
    <!-- border -->![Add Excel Cloud Link](02-AddExcelCloudLink.png)

7.  Now you edit the properties of **Excel Cloud Link** activity. In the flow chart, select the **Excel Cloud Link** activity, on the right side menu, click **Edit Activity** button
    <!-- border -->![Edit Excel Cloud Activity](03-ExcelLinkEditActivity.png)

8.  Within the edit activity screen, you can either use the file picker or drag and drop your test Excel.
    <!-- border -->![Import Excel](04-ExcelCloudLink_AddExcel.png)

9.  You can see, the Excel cloud link capabilities, it already pre-selects the first sheet, it groups data by column names, it also finds out in few seconds the complete range of the Excel
    <!-- border -->![Browse the data](05-ExcelData.png)

10. While Excel cloud link is great in processing the data, it still needs for runtime reference, the path of your Excel. Typically you needed the full path of the Excel. i.e. `folder path + file name`. Please provide your full path location into the path field.
    <!-- border -->![Provide Excel path](06-ExcelCloudLink_pasteExcelPath.png)

11. SAP Process Automation has data type functionality to logically group and process your data. You can create a data type out of the added Excel by using the Excel data structure (2)
    <!-- border -->![Create Data type out of Excel](07-ExcelCloudLink_AddExcel.png)

12. Provide a data type name, As you can see **Excel Cloud Link** already identifies field names using the first row of each column in your Excel and determines the field type based on the type of data present in each column below the first row.
    <!-- border -->![Provide Data type](08-ExcelCloudLink_DataType.png)

13. All that **Excel Cloud Link** does is, it is a helper function to read given range of data in your Excel. this will be returned through the **Output Parameter**. If you prefer you can edit output parameter name on the right side
    <!-- border -->![Edit Output Parameter](09-EditOutputParameter.png)

14. Now you see the benefit of using Excel cloud link by printing output parameters. For this you will use a loop activity **For Each**  
    <!-- border -->![Add For Each](10-AddForEach.png)

15. As a loop variable, you can add the **Output Parameter** from **Excel Cloud Link** activity
    <!-- border -->![Add Loop Variable](11-AddLoopVariable.png)

16. Now you can print the customer name by adding **Log Message**. if you are using the sample data provided in this tutorial you can print **Order Number**
    <!-- border -->![Add Log Message](12-AddLogMessage.png)

17. Initially you add **Customer.name**
    <!-- border -->![Add Customer Name](13-AddCustomerName.png)

18. At first it takes 0-th index. You use **index** variable so for each loop iteration, it can print the corresponding value
    <!-- border -->![Add Loop Index to the string](14-AddIndex.png)

19. Add **Close Excel Instance** as you added **Open Excel Instance** in the first step
    <!-- border -->![Add Close Excel Instance](14-1-AddCloseExcelInstance.png)

20. Now you can test the output after saving the project.
    <!-- border -->![Save and Run project to validate result](15-Output.png)

> - SAP Help [Documentation]([IRPA SDK V2 (sap.com)](https://help.sap.com/doc/b8b5c9bbac3846a0a57aedab3f412880/Cloud/en-US/modules/helper.html)) on Excel Cloud Link
> - Excel Cloud Link [How to use the Excel Cloud Link activity helper](https://www.vimeo.com/458204726?embedded=true&source=video_title&owner=122956519)
> - Community [Quick automation with SAP Intelligent RPA – Excel SDK | SAP Blogs](https://blogs.sap.com/2021/05/31/quick-automation-with-irpa-Excel-sdk/) on Excel SDK

In the next step, you take a more traditional approach to process the Excel data.



### Process Excel Data


1. Let us start by adding new automation to the project and add **Open Excel Instance** to the flow.
  <!-- border -->![Add Open Excel Instance](16-AddOpenExcelInstance.png)

2. You create couple of string variables to create base folder and file name. from the activity search for **String** and add it to the flow
  <!-- border -->![Create String Variables FolderPath & FileName](17-CreateStringVariable.png)

3. One of the Variable will point to the base `folder path` and the second variable will point to the `file name`. Creating them in this form, helps to use it in subsequent activity types.
  <!-- border -->![Create String Variables FolderPath & FileName](18-Created2VariableNames.png)

4. You call **Get File Name** activity to fetch the fileName. you can avoid this by creating a string variable for the file name
  <!-- border -->![Get Excel File Name](19-AddGetFileName.png)

5. You add **Open Workbook** activity and point it to the full path (folder + file name) of the test Excel file. This will open the specific Excel file.
  <!-- border -->![Add Open Workbook](20-AddOpenWorkbook.png)

6. Subsequently you activate the workbook and the first worksheet. Input for the activate work book activity is the file name from the previous step and the input of the activate worksheet activity is the first sheet name. You can use variables or hard coded values for now. If you are trying a different Excel, use these values appropriately
  <!-- border -->![Activate Worksheet](21-ActivateWorkbookWorksheet.png)

7. Now that Excel sheet is open, you can apply filters, hide columns or get values from a range of cells or add a new sheet and copy a range of values to the newly created sheet.
  <!-- border -->![Activate Worksheet](22-ActivateWorkbookWorksheet.png)

8. You can add **Get Values (Cells)** to fetch specific range cells and subsequently you can validate the step using output parameter through `LogMessage`. The input to **Get Values (Cells)** can be a single cell or a range of Excel cells. When you use **Excel Cloud Link**, reading Excel data is pretty quick and simple. the traditional approach is in few steps.
  <!-- border -->![Add Get All Values](23-AddGetAllValues.png)

9.  Now Add **Filter Range** activity. In the **Filter Range**, you specify the data range, where the filter needs to be applied, along with the column on which the filter needs to apply with a filter condition.
You add a new Excel sheet to your workbook and copy this filtered data onto that. just for demonstration purpose.
    <!-- border -->![Add Filter Range](24-FilterColumn.png)

10. Subsequently you can add Hide column to hide a specific column in the result
    <!-- border -->![Add Hide Column](25-AddHideColumns.png)

11. You copy the result of filter to a new sheet. For this you add a sheet through **Add Worksheet**  and give it a name **Result**
    <!-- border -->![Add a New Worksheet](26-AddAddWorksheet.png)

12. This will activate the new worksheet, but for us to copy from the filter results to this new sheet, you switch to the first sheet and copy its values.
    <!-- border -->![Activate original sheet](27-ActivateBackFirstSheet.png)

13. Add **Copy Range** activity and provide the source range, destination worksheet and workbook and starting cell for copy.
    <!-- border -->![Add Copy Range](28-AddCopyRange.png)

14. Activate the new sheet now, in order to save it
    <!-- border -->![Activate Result Sheet](29-AddActivateWorksheet4Result.png)

15. Now add **Save As Workbook** to save the new sheet as another file. You have option to save workbook as an Excel file or CSV file. In order to save as a new CSV file, you will be needing a file path to store this CSV file, This will be of the format `folderName` + `fileName` without extension where this needs to be saved.
    <!-- border -->![Save As Workbook](30-SaveAsWorkbook.png)

16. Finally you need to include **Close Excel Instance** to conclude the step
    <!-- border -->![Add Close Instance](31-AddCloseExcelInstance.png)

17. You can save the project and test it to validate the result.
    <!-- border -->![Run the project to test and validate](32-ValidationResult.png)

18. In the end, when this tutorial was tested, you used this below source data
    <!-- border -->![Source Excel Data](33-OriginalExcelData.png)

19. After Automaton is executed, it has filtered the data (Town starting with a), hide a column (Column F with title `storey_range`) and resultant Excel will look like this
    <!-- border -->![Final Excel Data](34-AfterAutomationProcessing.png)

As you can review, Excel SDK of SAP Process Automation offers wide variety of activities you can consider for business processes.

> - Do review the below resources
    > - Microsoft Excel Best Practices from the SAP Process Automation [documentation](https://help.sap.com/docs/IRPA/8e71b41b9ea043c8bccee01a10d6ba72/76d8129ef8984e4184294ae02b30ed58.html).
    > - Excel Email Best Practices Automation from the [SAP Process Automation Store](https://irpa.store.sap.com/#/package/d4c960f9-30ba-4f20-b641-7de878f6f5d5)
