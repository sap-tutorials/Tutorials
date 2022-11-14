---
parser: v2
author_name: Shukla Archana
author_profile: https://github.com/ArchanaShukla
auto_validation: true
time: 20
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform, tutorial>free-tier ]
primary_tag: software-product>sap-process-automation
---

# Create a Decision
<!-- description --> Create a decision to determine approvers who will be authorized to approve sales order based on complex rules

## Prerequisites
 - [Create an Automation to Extract Data](spa-create-automation)

## You will learn
 - Create & configure decision in the process
 - Create & configure data types
 - Model decision tables with rules expressions

---

### Add a Decision to the Process


A **decision** consists of one or more policies. Each policy consists of a collection of rules. They are used to automate the decision-making parts of a business process. After you create a decision, define your business logic by adding rules to the policy. There are two types of rules:

- **Decision Table Rule**: A decision table is a collection of input and output rule expressions in a tabular representation.
- **Text Rule**: A text rule is the collection of rule expressions in a simple if-then format.

A **Decision Diagram** is a flow chart that describes the execution flow of the decision logic from the input to the output. Using a decision diagram, you can view the input, output, policies of a decision and the rules of the policies. You can also access each component of a decision, like a policy or a rule, via the decision diagram itself.

> To add a decision, you must first identify the set of rules that you need to add to the decision-making process. Once you have identified your rules, then define the data types relevant for designing the rules. This is an important step before you start modelling your decision because these data types will contain all necessary fields that will be needed to model the rule.

In this section, you will create and configure a decision which will be used to determine the relevant approvers based on the sales order fields.

1. In the process builder:
    - Choose **+** of the **default conditional flow**,
    - Select **Decision** then **New Decision**.

    <!-- border -->![001](001.png)

2. In the Create Decision window, do the following:

    -	In the Name field enter **Determine Approver**,
    -	In the Description field enter **Rule to identify the potential approvers for sales order**,
    -	Choose **Create** button.

    <!-- border -->![002](002.png)

3. Now you have to model the decision. For that, choose the 3 dots next to **Determine Approver** decision, to open the menu and choose **Open Editor**.

    <!-- border -->![002](003.png)

4. A decision editor opens. You can see the decision diagram on the left panel and configuration option for Input and Output on the right panel. Notice the default policy that is pre-created with the decision.

    > A Policy is a collection of rules to be executed in strict order, meaning that they will run in the order in which they are added to the policy, and only the results of the last rule execution will be given as the final output of the decision.  

    <!-- border -->![002](004.png)


### Create Data Types


A data type describes the data structure that can be used as an input and/or output parameter in an automation, a decision or processes. Data types enable you to formalize the data used as input/output parameters for steps, activities, skills processes, scenarios, triggers, or notifiers. Data types facilitate the manipulation and validation of data.

> You can create a data type manually, but some data types can also be created automatically when SDK packages or pre-packaged scenarios are imported or after running an automation.

Now, you have to map the Input and Output of the decision to the actual data object available with the process. In this section, you will use Sales Order as the Input data object and Approver as the Output data object. While the Sales Order was already created in the automation step, you will create an Approver data object for decision output.

1. Go back to the **Overview** tab, choose **Create** and select **Data Type**.

    <!-- border -->![002](005.png)

2. In the Create Data Type window, do the following:
    - In the Name field enter **Approver**,
    - In the Description field enter **Approver details who will approve the order from supplier side**,
    - Choose **Create** button.   

    <!-- border -->![002](006.png)

3. In the **Approver** data type screen, choose **New Field** to add a new attribute to the data object.

    <!-- border -->![002](007.png)

4. In the Field Details section on the right, in the **Name** field enter **Email**. Keep the **Type** as **String**.

    > You can choose the **Type** dropdown list to see the different kind of data types that are supported like Number, Password, Date, Time, Boolean etc.

    <!-- border -->![002](008.png)

5. Similarly, add another attribute `UserGroup` of **Type** **String** to the data type.

    <!-- border -->![002](009.png)

6. **Save** changes.

    <!-- border -->![002](010.png)


### Configure Decision


After the data types are created, you will now configure the decision:

- With Input and Output data types.
- Create decision table rule to the business policy.

1. First, add this newly created data object as the decision output.

2. Go back to **Determine Approver** decision tab.

    <!-- border -->![002](011.png)

3. In the  Determine Approver section:
    - select **Add Input Parameter** button,
    - select **Add Output Parameter** button.

    <!-- border -->![002](012.png)

4. Configure Input Parameter:
    - In Name enter: **Sales Order Input**,
    - In Description enter: **business rules input**,
    - In Type choose: **Sales Order**.

    <!-- border -->![002](013.png)

5. Configure Output Parameter:
    - In Name enter: **Approver Output**,
    - In Description enter: **business rules output**,
    - In Type choose: **Approver**.

    <!-- border -->![002](014.png)

6. **Save** changes.

7. Then you will create the actual decision-making parts that make the decision in the process. Under Determine approver, select **Rules**.

    <!-- border -->![002](015.png)

8. Select **Add Rule**.

    <!-- border -->![002](016.png)

9. In the Create Rule window:
    - Under Rule Type select **Decision Table**,
    - In the Rule Name enter **Determine Approver**,
    - In the Rule Description enter **Rule to identify the potential approvers for sales order**,
    - Choose **Next Step** button.

    <!-- border -->![002](017.png)

    > A decision table is a tabular representation of the rule with If and Then header and row columns. If-header columns contain the expressions, which are evaluated, and Then-header columns contain the result structure that will be returned after the decision is run.

10. You will now configure the conditions. Under **Data Types**:
    - Choose **Sales Order Input**,
    - Select **`shippingCountry`**,
    - Select **`orderAmount`**,
    - Choose **Next Step** button.

    <!-- border -->![002](018.png)

11. Configure the output or result of the decision table. Under Data Type:
    - Choose **Approver**,
    - Select `UserGroup`,
    - Select **Email**,
    - Choose **Next Step** button.

    <!-- border -->![002](019.png)

12. Review and choose **Create** button to create the rule.

    <!-- border -->![002](020.png)

    > You can use the Settings option to easily define these If and Then header expressions with inline suggestions or free-flow typing.

13. In the newly created **Decision Table**, add values to condition and result columns.

    <!-- border -->![002](021.png)

14. Click in the first field (first column)

    <!-- border -->![002](022.png)

15. Type EXISTSIN, and choose **exists in** from Array Operators.

    <!-- border -->![002](023.png)

16. Continue typing, and write this expression: **EXISTSIN ['United Kingdom' , 'India' , 'Germany']**. After you have finished, press Enter key or click outside the input field to confirm.

    > You can either type-in the entire expression as free-flow or use the context help to write the expression.

    > For all *String* type of data object attribute, you have to mandatory add single-quote (') before and after the text.

    <!-- border -->![002](024.png)

    > Remember that for all String type data object attributes, you must add a single quote (') before and after the text.

17. Choose the input field of **Order Amount** column (second column of the decision table) and enter **<= 100000**.

    <!-- border -->![002](025.png)

18. Similarly, enter the following expressions for the respective result column (or **Then** section):
    - Under `UserGroup` enter: `'SO_APPROVER'`
    - Under Email enter: `'your user email'`
    > Do not forget to put single-quote (') for string type values

    <!-- border -->![002](026.png)

    User Group is a role collection or group created in the BTP cockpit or in your respective user management system. These groups have users who are responsible for certain jobs. The advantage of using groups is that you can add/remove users from these groups without the need to change the decision.

    To ensure that the decision table rule returns definite output for all kinds of sales orders, add one more row to the decision table to return the list of approvers who can approve the sales order for value greater than 100000.

    So, for example, for all sales orders coming from India, Germany, or the United Kingdom (the defined shipping countries) which have a value smaller or equal than 100,000 – the first row will run. For all other sales orders, whose value is greater than 100000, the second row will return; and for any other combination that does not match the rows – an empty result will be returned.

13. To add a new row to the decision table, do the following:
    - Choose the check-box of the first row,
    - Choose **Add Row**,
    - From the dropdown options, select **Insert After**.

    <!-- border -->![002](027.png)

14. Similarly, enter the following values for the new row:

    |  Condition Column    | Value
    |  :------------- | :-------------
    |  Shipping Country    |    |
    | Order Amount         |>100000 |

    |  Action Column   | Value
    |  :------------- | :-------------
    |  `UserGroup`        | `'SO_MGMNT'`
    |  `Email`       | `'your user email'`|

    <!-- border -->![002](028.png)

15. Choose **Save** button.

    > Save will both save and activate the decision table. If there are any validation issues in the decision table, then Save will not happen and the errors will be shown in the **Design Console**



### Configure Decision in Process Builder


After you have created and configured the decision, next you have to map the input fields of the decision with the actual process content fields from the process builder.

1. Open the process builder, choose **Determine Approver** decision and do the following. Choose **Inputs** tab.

    > You might not see entries in the Input, please refer to [the Knowledge Base Article](https://launchpad.support.sap.com/#/notes/3207153) for the complete workaround.

    <!-- border -->![002](029.png)

2. Map the following decision table input with the process content:

    |  Decision Input Field   | Process Content
    |  :------------- | :-------------
    | `expectedDeliveryDate`           |`selectedOrder` > `expectedDeliveryDate`
    | `orderAmount` | `selectedOrder` > `orderAmount`
    | `orderDate` | `selectedOrder` > `orderDate`
    | `orderNumber` | `selectedOrder` > `orderNumber`
    | `orderStatus` | `selectedOrder` > `orderStatus`
    | `shippingCountry` | `selectedOrder` > `shippingCountry` |

    <!-- border -->![002](030.png)

2. **Save** the process.    

    <!-- border -->![002](031.png)

    > You might see an error symbol on your decision. This is because the outbound connection from the decision is still dangling and not connected to any activity. You may connect it to the end activity.



### Update the Process

1. You will now adapt the business process one last time to fully automate your approver selection by matching the recipients of the approval form to the one returned from the decision table.

2. Select **Approval form**:
    - Match `orderNumber` from **Get Order Details** automation in the Subject,
    - Under Recipients > Users select `Email` from **Determine approver** decision,
    - Under Recipients > Groups select `UserGroup` from **Determine approver** decision.

    >You can modify your selection as needed.

    <!-- border -->![002](032.png)

---
