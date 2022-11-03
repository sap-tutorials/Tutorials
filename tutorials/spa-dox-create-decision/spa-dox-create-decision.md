---
parser: v2
author_name: Stephan Schluchter
author_profile: https://github.com/SchluchterStephan
auto_validation: true
time: 20
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform ,tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

# Create a Decision for Invoice Approval Process
<!-- description --> Create a decision to determine the approver of the invoices

## Prerequisites
 - You have [created the process](spa-dox-create-process), the form to trigger the process and the [automation](spa-dox-create-automation) to extract the invoice data


## Intro
With a Decision you can include several policies, guidelines, business rules and so on into your process flow.
## You will learn
  - How to create a decision
  - How to make use of data types and how to create them
  - How to maintain a decision table and include it in the process flow

---

### Add a decision and create a data type


You will now add a decision to the flow of your process. With this you are able to include business logic.

1. Choose **+** following the automation **Extract Invoice Data**, in the menu select **Decision** > **New Decision** to add a new one.

    <!-- border -->![add decision](01.png)

2. Maintain the **Name** **`Determine Approver`** and also a **Description**, the **Identifier** will be created automatically. Choose **Create**.

    <!-- border -->![Decision Name](02.png)

3. The Decision **Determine Approver** is now in the process, choose the three dots and select **Open Editor**.

    <!-- border -->![Open Editor](03.png)

4. You see a **Decision Diagram** showing the flow of the data within the Decision and also the **Inputs**. Change there the **Type** to **`Invoice`**.

    > "Invoice" is the data type which has been created in the Automation and contains all relevant extracted data.

    <!-- border -->![Change Input](04.png)

5. The data type for **Output** needs to be created. Therefore, select **+** on the left-hand side to create one.

    <!-- border -->![Create Data Type](05.png)

6. Select **Create > Data Type**.

    <!-- border -->![Create new Data Type](07.png)

7. Define the **Name** **`Approver`** and also add a **Description**, again, the **Identifier** will be created automatically. Choose **Create**.

    <!-- border -->![Name data type](08.png)

8. Create a **New Field**.

    <!-- border -->![New Field](09.png)

9. Maintain the **Name** **`eMail`**, leave the **Type** as **`String`**.

    <!-- border -->![eMail](10.png)

10. The new data type containing the email address of the approver is now created. **Save** your work.

    <!-- border -->![Save new data type](11.png)

11. Go back to your Decision **Determine Approver** and select as **Output** the newly created data type **`Approver`**.

    <!-- border -->![Select new data type](12.png)

12. Open the **Default Policy** and select **>**.

    <!-- border -->![Open default policy](13.png)



### Create a decision table


There are many ways to express a business rule, in this case you will create a decision table to determine the approver of the invoice based on certain criteria.

1. Choose **Add Rule**.

    <!-- border -->![Add Decision Table](14.png)

2. Set the **Rule Name** to **DT Determine Approver** and the **Description** to **Decision Table to determine approver**. Choose **Next Step**.

    <!-- border -->![Decision Table Name](15.png)

3. You will now configure the conditions. Under **Data Types**, choose **Determine Approver Input** and select **Sender Name**.

    <!-- border -->![Determine Approver Input](16.png)

4. Choose **Next Step** to configure the results.

    <!-- border -->![Next Step](16bis.png)

5. Under **Data Types**, select **Determine Approver Output**.

    <!-- border -->![Determine Approver Output](17.png)

6. Select **eMail**.

    <!-- border -->![Select Email](18.png)

7. Select **Next Step** to review.

    <!-- border -->![Next Step](19.png)

8. Choose **Create** to create the rule.

    <!-- border -->![Create](20.png)

9. You may edit the rule you just created by selecting the pencil icon.

    <!-- border -->![Edit](21.png)

10. Define the attributes for **Determine Approver1 Input.SenderName** **`EXISTSIN['ABC Communication']`** and **eMail** **`<your SAP BTP user ID, e.g. diana.smith@mail.com>`**. You can also make use of the value help, by pressing the space bar before maintaining the expression in the first column.

    This means, if the company name in the invoice is "ABC Communication", then the approval request will be sent to you. Otherwise...follow the next step.

    <!-- border -->![If Then](22.png)

12. In case the **Determine Approver1 Input.SenderName** is defined as **`EXISTSIN['Telecommunications']`**, the approval request should be sent to **`'jane.doe@sap.com'`** or whatever recipient you might want to choose. Select the first row and **Add Row** to **Insert After**. Then maintain the row accordingly.

    <!-- border -->![Add Row](23.png)

13. **Save** your work, your decision table is ready.

    <!-- border -->![Decision ready](24.png)



### Maintain input and output of the decision


Though the Decision is ready, you need to connect it to the data flow of your process and define which data should be the input and output here. Also to get rid of this error marker.

1. Go back to the process and select the Decision.

    <!-- border -->![Error](28.png)

    > You might not see entries in the Input. This is due to a bug. As a workaround, click on the three-vertical-dot and delete the decision. Add the decision again in the process.

    > Please refer to [the Knowledge Base Article](https://launchpad.support.sap.com/#/notes/3207153) for the complete workaround.

2. Define the input, select the text field of **Document Number**.

    <!-- border -->![Input Decision](29.png)

3. Assign the related data from the **Process Content**, here select **Document Number**.

    <!-- border -->![Document Number](30.png)

4. Repeat this also for **Gross Amount** and **Sender Name**, both are part of **Invoice Details**. The input mapping is done.

    <!-- border -->![Invoice Details](31.png)

5. Just check the **Output**, it is **eMail**, as you have defined it in the decision itself. **Save** your work.

    <!-- border -->![Decision Output](32.png)

You have now defined who should approve the invoice, based on the company name. Next you will use the outcome of the business rule, the email address, as input for the approval.

