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
    - Choose **+**,
    - Select **Automation**,  **New Automation**.

    !![Automation](001.png)

2. A pop up will appear to configure the Desktop Agent version. Do the following in the pop up:
    - From the dropdown, select the version of the Desktop Agent installed on your machine.
    - Choose **Confirm**.

    > It would be with suffix as **Registered**.

    !![Automation](002.png)

3. A new pop-up will appear to create the automation. Do the following in the pop-up:

    -  Enter **Name** of the automation: **Extract Invoice Data**,
    -  Enter **Description** of your choice,
    -  Choose **Create**.    

    !![Automation](003.png)

    An automation **Extract Invoice Data** will be created successfully.

    !![Automation](004.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Data Types)]

1. Go to the **Overview** Tab. Choose the **Create** button. Create an artifact of the type **Data Type**.

    !![Automation](005.png)

2. A new pop-up will appear:
    - Enter **Name** of the data type: **Invoice**,
    -  Enter **Description** of your choice,
    - Choose **Create**.

    !![Automation](006.png)

3.  In the Data Type **Invoice** and add new fields as following.

    |  Field Name     | Type
    |  :------------- | :-------------
    |  `DocumentNumber`| String
    |  `GrossAmount`   | Number
    |  `SenderName`    | String

    !![Automation](007.png)

4. Choose **Save**.

    !![Automation](008.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create Input and Output Parameters)]

    Input and output parameters allow you to exchange data in the workflow of your automation between activities, screens, and scripts.

1. Go to the **Overview** tab. Open **Extract Invoice Data** Automation.

    !![Automation](009.png)

2.  In **Automation Details**, select the **Input/Output** section.

    !![Automation](010.png)

3. Add Input and Output parameters.

    !![Automation](012.png)

4. Edit Input and Output parameters as follows:

    |  Parameter Name       | Data type        | Parameter Type | Description
    |  :---------------     | :-------------   | :------------- | :---------------
    |  `FilePath`      | String     | Input          | Path where the invoice document is stored  
    |  `InvoiceDetails`    | Invoice| Output         | Extracted Invoice Details

    !![Automation](011.png)

3. Choose **Save**.    

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create Document Template)]

1. Go the **Overview** Tab. Choose **Create** and **Document Template**.

    !![Automation](013.png)

2. In the **Create Document Template** window, under **Details**:
    - Choose **Create a new template**,
    - Enter the **Name** of the template,
    - Upload the invoice document which you have downloaded from prerequisites,
    - Choose **Next**.

    !![Automation](014.png)

3. Select **Invoice** as the document type of your template and choose **Next**.

    !![Automation](015.png)

4. Under **Choose Schema**:
    - Choose **Create New**,
    - Enter the **Name** of the schema,
    - Choose the Header fields that you would extract the data from the Invoice document. In this scenario you would select `documentNumber`, `grossAmount`, `senderName`,
    - Choose **Add**.

    !![Automation](016.png)

5. Document Information Extraction SDK would be added as dependency to your project. The schema and template are created successfully.

6. Once Upload will be completed, choose **Open in a new tab**.

    !![Automation](017.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Annotate and Activate the Document Template)]

1. You are now navigated to Document Information Extractor. It allows you to edit and annotate the template. To work on Document Information Extractor, please check if you have been assigned to `Document_Information_Extraction_UI_Templates_Admin` role in SAP BTP Cockpit. Otherwise, add the role manually. You would get an error as , if you do not have DOX roles.

    !![Automation](018.png)

2. Select the **Document Name**.

    !![Automation](019.png)

3. Choose **Annotate**.

    !![Automation](020.png)

4. Choose **Edit**.

    !![Automation](021.png)

5. Select the data in your invoice document which you would like to extract the information. In this scenario, you will read the Document Number, Gross Amount and Sender Name.
    - Select the value  **174221** in the document and map to the field `documentNumber`. Choose **Apply**.

        !![Automation](022.png)

    - Select the value **ABC Communication** in the document and map to the field `senderName`. Choose **Apply**.

        !![Automation](023.png)

    - Select the value **220** in the document and map to the field `grossAmount`.

        !![Automation](024.png)

6. Once mapping is done. Choose **Save** and **Activate** the template.

    !![Automation](025.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Build the Automation)]

1. Navigate back to Application Development and select **Extract Invoice Data** automation tab.

    !![Automation](026.png)

2. Extract data template. In Automation Details, under Tools, in search field, type **Extract data**.

    > You can extract data with Document Information Extraction using the chosen document template and given PDF file.

    !![Automation](027.png)

3. Drag and drop the activity **Extract Data (Template)** to the automation flow.

    !![Automation](028.png)

4. Select the activity. Maintain the parameters for the activity as following:
    - Under Document Schema: select your Schema
    - Under Document Template: select your Template
    - Under `documentPath`: choose `FilePath`

    !![Automation](029.png)

5. You have already created the data type **Invoice** in Step 2. Now, you will create variable of the type **Invoice**.

6. Click on the canvas. Search for the datatype **Invoice**, located under the **Data Types** section.

    !![Automation](030.png)

7. Drag and drop the data type **Invoice** into the automation flow.

    !![Automation](031.png)

8. Select **Create Invoice variable**. In the Input Parameters, under value, choose **Create Custom Data**.

    !![Automation](032.png)

9. Choose the Edit button to map the parameters:
    - `DocumentNumber` to the extracted invoice data from the activity Extract Data (template),
    - Repeat the same for `GrossAmount` and `SenderName` and map it to the corresponding fields of `extractedData`.

    !![Automation](033.png)

10. Rename the output parameter to `myinvoiceData`.

11. The final input and output parameters of **Create Invoice Variable** looks as below.

    !![Automation](034.png)

12. Print the invoice data using the activity **Log message**.

    >  With this activity you generate a log message within the tester and the trace file, which is useful for setting up an automation. By default, a log will be "Information".

13. Click on canvas, search for **Log Message** activity and drag and drop it into the automation flow.

    !![Automation](035.png)

14. Select **Log Message** activity. In Input Parameters under message add `myinvoiceData` value.

    !![Automation](036.png)

15. Select **Save**.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Passing the Parameters Outside the Automation)]

1. Select the **End**. Pass the variable `myinvoiceData` to the output parameter `InvoiceDetails`, which you have created in Step 2.

    !![Automation](037.png)

2. **Save** the automation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test the Automation)]

1. Select Test button and enter the `Filepath` where the invoice document is stored locally on your machine.

    !![Test](038.png)

2. The automation opens the Invoice Document, extracts data and prints the details i.e Document number, Gross amount and Sender name.

    !![Test](039.png)

3. Your automation is built successfully.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Mapping of Parameters to the Automation and Process )]

1. Navigate to the **Get Invoice Details** process. and select the automation **Extract Invoice Data**. Map the input parameter of the automation to the form parameter `FilePath`.

    !![Mapping](040.png)

2. Select **Save**.

3. Once this tutorial is completed, the process looks as following.

    !![Mapping](041.png)

[VALIDATE_1]
[ACCORDION-END]

---
