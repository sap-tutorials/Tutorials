---
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
title: Create an automation to extract Invoice Details
description: Extract invoice document using Document Extraction Template to send the invoice details to the process
auto_validation: true
time: 25
tags: [ tutorial>intermediate , software-product>sap-business-technology-platform , tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

## Prerequisites
 - Complete the tutorial of creating an [Invoice Approval Process](spa-dox-create-process)
 - A Windows PC
 - If you are using a MAC, please install a VDI
 - [Install and Setup the Desktop Agent](spa-setup-desktop-agent)
 - Download the [Invoice Document](https://github.com/sap-tutorials/Tutorials/blob/master/tutorials/spa-dox-create-automation/invoice.pdf) to your local machine

## Details
### You will learn
  - How to extract data using Document Extraction Template
  - How to bind parameters between process and automation

---

[ACCORDION-BEGIN [Step 1: ](Create Automation)]

1. In the process **Get Invoice Details**:
    - Choose !![00-01](AddButtonProcess.png).
    - Select **Automation** > **New Automation**.

    !![New Automation](1.png)

2. A pop up will appear to configure the Desktop Agent version. Do the following in the pop up:
    - From the dropdown, select the version of the Desktop Agent installed on your machine.
    - Choose **Confirm**.

    > It would be with suffix as **Registered**.

    !![2-png](2.png)

3. A new pop-up will appear to create the automation. Do the following in the pop-up:

    -  Enter **Name** of the automation as **Extract Invoice Data**.
    -  Enter **Description** of your choice, if needed.
    -  Choose **Create**.    

    !![3-png](3.png)

    An automation **Extract Invoice Data** will be created successfully.

    !![4-png](4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Data Types)]

1. Create an artifact of the type **Data Type**.

    !![21-png](21.png)

2. Enter the name of the data type as **Invoice** and choose **Create**.

    !![22-png](22.png)

3.  Navigate to Data Types and add new fields as following and choose **Save**.

    |  Field Name     | Type
    |  :------------- | :-------------
    |  `DocumentNumber`| String
    |  `GrossAmount`   | Number
    |  `SenderName`    | String

    !![23-png](23.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create Input and Output Parameters)]

    Input and output parameters allow you to exchange data in the workflow of your automation between activities, screens, and scripts.

1.  Click on the canvas and select the **Input/Output** section in **Automation Details**.

    !![00-05](Step11-InputOutput.png)

2.  Add Input and Output parameters as following:

    |  Parameter Name       | Data type        | Parameter Type | Description
    |  :---------------     | :-------------   | :------------- | :---------------
    |  `FilePath`      | **`String`**     | Input          | Path where the invoice document is stored  
    |  `InvoiceDetails`    | **`Invoice`** | Output         | Extracted Invoice Details

    !![Input-Output](InputandOutputParameters.png)

3. Choose **Save**.    

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create Document Template)]

1. Create an artifact of type **Document Template**.

    !![7-png](7.png)

2. Enter the following details:

    - Choose **Create a new template**.
    - Enter the **Name** of the template.
    - Upload the invoice document which you have downloaded from prerequisites.
    - Choose **Next**.

    !![8-png](8.png)

3. Select **Invoice** as the document type of your template and choose **Next**.

    !![9-png](9.png)

4. Choose your extraction schema.

    - Enter the **Name** of the schema.
    - Choose the fields that you would extract the data from the Invoice document.
    - In this scenario you would select `documentNumber`, `grossAmount`, `senderName`.
    - Choose **Add**.

    !![10-png](10.png)

5. Document Information Extraction SDK would be added as dependency to your project and the schema, template are created successfully.

6. Choose **Open in a new tab**.

    !![11-png](11.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Annotate and Activate the Document Template)]

1. You are now navigated to Document Information Extractor. It allows you to edit and annotate the template. To work on Document Information Extractor, please check if you have been assigned to `Document_Information_Extraction_UI_Templates_Admin` role in SAP BTP Cockpit. Otherwise, add the role manually. You would get an error as below if you do not have DOX roles.

    !![DOX roles error](DOXroleserror.png)

2. Select the **Document Name**.

    !![12-png](12.png)

3. Choose  **Annotate**.

    !![14-png](14.png)

4.  Choose **Edit**.

    !![Link text e.g., Destination screen](15.png)

      - Select the data in your invoice document which you would like to extract the information.

        In this scenario, you will read the Document Number, Gross Amount and Sender Name.

      - Select the field  **174228**  in the document and map to the field `documentNumber`.
      - Choose **Apply**.

        !![16-png](16.png)

      - Select the value **ABC Communication** in the document and map to the field `senderName`.

        !![17-png](17.png)

      - Select the value **220** in the document and map to the field `grossAmount`.

        !![18-png](18.png)

  5. The mapping of the data in the invoice and fields are mapped. Choose **Save** and **Activate** the template.

    !![Save](SaveActivate.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Build the Automation)]

1. Extract data template
    - Search for the activity **Extract data** and add the activity **Extract Data (Template)** to the automation flow.
    > ### What is going on?
    > You can extract data with Document Information Extraction using the chosen document template and given PDF file.

    !![19-png](19.png)

    - Maintain the parameters for the activity as shown below.

    !![20-png](20.png)

2. You have already created the data type **Invoice** in Step 2. Now, you will create variable of the type **Invoice**.

    - Search for the datatype **Invoice**. You will find it under the **Data** section.

      !![24-png](24.png)

    - Drag and drop  the  data type **Invoice** into the automation flow and select **Create Custom Data** in the parameter **value**.

      !![25-png](25.png)

    - Choose the Edit button (pencil icon) to map the parameter `DocumentNumber` to the extracted invoice data from the activity **Extract Data (template)**.

      !![26png](26.png)

    - Repeat the same for `GrossAmount` and `SenderName` and map it to the corresponding fields of
      `extractedData`.

    - Rename the output parameter to `myinvoiceData`.

    The final input and output parameters of **Create Invoice Variable** looks as below.

    !![My-invoic-eData](MyinvoiceData.png)

3. Print the invoice data using  the activity **Log message**.

4. Add **Log Message** activity and the `myinvoiceData` to the value.

    > ### What is going on?
    >  With this activity you generate a log message within the tester and the trace file. Useful for setting up an automation. By default, a log will be "Information".

    !![Log-Message](LogMessage.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Passing the Parameters Outside the Automation)]

1. Select the **End** and pass the variable `myinvoiceData` to the output parameter `InvoiceDetails` which you have created in Step2.

    !![End-parameter](Endparameter.png)

2. **Save** the automation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test the Automation)]

1. Select Test button and enter the `Filepath` where the invoice document is stored locally on your machine.

    !![Link text e.g., Destination screen](Test.png)

2. The automation opens the Invoice Document, extracts data and prints the details i.e Document number, Gross amount and Sender name.

    !![Test-Results](TestResults.png)

3. Your automation is built successfully.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Mapping of Parameters to the Automation and Process )]

1. Navigate to the process **Get Invoice Details** and select the automation **Extract Invoice Data**.

2. Map the input parameter of the automation to the form parameter `FilePath`.

    !![Mapping-Parameters](MappingParameters.png)

3. Your process looks like below once you complete this tutorial.

    !![Process-automation](Process-Automation.png)

[VALIDATE_1]
[ACCORDION-END]

---
