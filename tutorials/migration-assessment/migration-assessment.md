---
author_name: Vishakha Deshmukh
author_profile: https://github.com/vishu2804
parser: v2
auto_validation: true
time: 20
tags: [  software-product>sap-integration-suite, software-product>cloud-integration, software-product>sap-process-integration, software-product>sap-process-orchestration, tutorial>license, tutorial>beginner  ]
primary_tag: software-product>sap-integration-suite
---

# Use the Migration Assessment Application
<!-- description --> Use the Migration Assessment Application to automatically assess the SAP Process Orchestration scenarios you want to migrate to SAP Integration Suite.

## Prerequisites
- You’ve completed the tutorial [Use the Migration Assessment Application](https://developers-qa-blue.wcms-nonprod.c.eu-de-2.cloud.sap/tutorials/migration-assessment-enablement.html) or refer [Activating Capabilities](https://help.sap.com/docs/integration-suite/sap-integration-suite/activating-and-managing-capabilities).
- Your user has the required access rights. See [Configuring User Access](https://help.sap.com/docs/integration-suite/sap-integration-suite/configuring-user-access). 
  


## You will learn
- How to use the Migration Assessment Application
- How to connect the Migration Assessment Application and SAP Process Orchestration
- How to add an SAP Process Orchestration system
- How to extract data from the SAP Process Orchestration system
- How to evaluate the integration scenarios using the information from data extraction requests


---
### Configure and Connect the Migration Assessment Application

To connect the Migration Assessment application with your SAP Process Orchestration system, you can use Cloud Connector to securely expose the endpoints listed below:

1. Ensure that Cloud Connector is connected to your SAP BTP subaccount. See [Establish Connections to SAP BTP](https://help.sap.com/docs/CP_CONNECTIVITY/cca91383641e40ffbe03bdc78f00f681/db9170a7d97610148537d5a84bf79ba2.html#establish-connections-to-sap-btp).

2. Create a new HTTP destination (cloud to on-premise) with back-end type `SAP Process Integration`. See [Configure Access Control (HTTP)](https://help.sap.com/docs/CP_CONNECTIVITY/cca91383641e40ffbe03bdc78f00f681/e7d4927dbb571014af7ef6ebd6cc3511.html).
   

      <!-- border -->![Image](Images/2023-01-23_23-54-52.jpg)

3. Make sure that your SAP Cloud Connector exposes the following API paths correctly. They're used to extract data from your SAP Process Orchestration system:

    -  Directory Content
        - `/CommunicationChannelInService`
        - `/IntegratedConfigurationInService`
        - `/SenderAgreementInService`
        - `/AlertRuleInService`
        - `/IntegratedConfiguration750InService`
        - `/ValueMappingInService`
        - `/ConfigurationScenarioInService`
        - `/BPMFacadeBeanImplService`
        - `/ReceiverAgreementInService`

    - ESR Content
        - `/rep/read/ext`
        - `/dir/read/ext`
        - `/rep/support/SimpleQuery`


4. Limit access to the previously mentioned endpoints and subpaths by changing **Access Policy** to **Path and All Sub-Paths**.
     
      <!-- border -->![Image](Images/Picture4-4.png)

You can also refer to the Prerequisites section of [Add an SAP Process Orchestration System](https://help.sap.com/docs/SAP_INTEGRATION_SUITE/51ab953548be4459bfe8539ecaeee98d/5f7672334ca74f90843d38375220d757.html) before adding a system.


### Add an SAP Process Orchestration System

1. Open the Migration Assessment application and navigate to **Settings**.

      <!-- border -->![Image](Images/Picture1.png)

2. In the Process Orchestration Systems table, choose **Add** and enter the **System Name** and **Description**.

      <!-- border -->![Image](Images/Picture2.png)

3. Configure a new destination with following details specific to your system:

    | Field	          | Content
    | :---------------|:----------------------------------------------------------
    | Name	          | **`systemname`**
    | Type            | **`HTTP`**
    | Description     | Add a meaningful description
    | URL             | Add your system URL
    | Proxy Type      | **`OnPremise`**
    | Authentication  | **`Basic Authentication`**
    | Location ID     | Add your location ID
    | User            | This should be a technical user – do not use personal credentials.
    |                 | **Note**: The user must be assigned to the following roles in your SAP Process Orchestration system.
    |                 | **`SAP_XI_API_DISPLAY_J2EE`**
    |                 | **`SAP_XI_API_DEVELOP_J2EE`**
    |                 | **`SAP_XI_MONITOR_J2EE`**     
    | Password        | Add the user’s password 
                 
        
4. (Optional) If you’re not using a central ESR, you can select the option **Same as Integration Directory** for **ES Repository**.
    
      <!-- border -->![Image](Images/Picture3.png) 

5. (Optional) If you’re using a central Enterprise Services Repository (ESR), create a second destination with the details listed in the following table.
  
    | Field	          | Content
    | :---------------| :------------------------------------------------------------------------------------------------
    | Name	          | **`systemname`**
    | Type            | **`HTTP`**
    | Description     | Add a meaningful description 
    | URL             | Add your system URL 
    | Proxy Type      | **`OnPremise`**
    | Authentication  | **`Basic Authentication`**
    | Location ID     | Add your location ID
    | User            | Add the user’s ID
    |                 | **Note**: The user must be assigned to the following roles in your SAP Process Orchestration system.
    |                 | **`SAP_XI_API_DISPLAY_J2EE`**
    |                 | **`SAP_XI_API_DEVELOP_J2EE`**
    |                 | **`SAP_XI_MONITOR_J2EE`**    
    | Password        | Add the user’s password


      <!-- border -->![Image](Images/Picture4.png)

6. After adding the system, you can test the connection if it is successful or not. To change the connection details, edit it.

      <!-- border -->![Image](Images/Picture5.png)

   In the Migration Assessment Application, the page now displays information about the Integration Directory and, optionally, the Enterprise Services Repository you connected to your previously created system. 

   Now, let’s take a tour of the Migration assessment application.

   **Rules**

 In the Migration Assessment Application, navigate to **Configure** > **Rules**. Here you can find a list of rules predefined by SAP. Rules are a set of characteristics according to which the application evaluates whether an integration scenario can be migrated and what effort you can expect.

  Currently, you can't add custom rules or edit the standard rules. You can only view the standard rules.

   <!-- border -->![Image](Images/Picture11-1.png)

 As an example, let’s select the rule **SenderAdapterType**. Here, you can see all the parameters of the rules, such as **Rule Match Value**, **Assessment Category**, and the **Weight** assigned to each rule match value. Based on these weights, the application calculates the estimated effort, which means that some parameters, and therefore rules, have a bigger influence on the final estimation than others.

   <!-- border -->![Image](Images/Picture12-2.png)

---

### Create a Data Extraction Request


You're now ready to retrieve data from the connected SAP Process Orchestration system using a data extraction request.

1. In the Migration Assessment Application, navigate to  **Request** > **Data Extractions**.

      <!-- border -->![Image](Images/Picture13-1.png)

2. Enter a **Request Name** and select the **System** you want to connect to, then choose **Create**.

      <!-- border -->![Image](Images/Picture14-1.png)

      The data extraction starts. Once the extraction finishes, the new request appears in the list of data extraction requests with the status **Completed**.

      **Note**:
      There's a size limitation of 2 GB on the data extraction. If the data extraction causes any issues regarding size, please contact SAP support.
      Only rerun an extraction if you have done any changes in the SAP Process Orchestration system, such as changed an existing integration scenario or added a new integration scenario or integration flow. During your migration project, you should avoid adding more scenarios to the SAP Process Orchestration system and create new scenarios directly in Cloud Integration. In this case, there's no need to run an extraction again for the SAP Process Orchestration system.

3. Choose **Log** to view the data extraction log, which provides you with details about the data extraction.

      <!-- border -->![Image](Images/Picture15-1.png)

      <!-- border -->![Image](Images/Picture16-1.png)

4. After extracting the necessary data from your system, you can assess the integration scenarios associated with your data using    scenario evaluation requests, as described in the next step.Delete extractions you no longer need by selecting the data extraction and choosing **Delete**. Refresh the page to update the list.
    
      <!-- border -->![Image](Images/Picture17-2.png)

---
### Create a Scenario Evaluation Request

To evaluate your integration scenarios using the information from data extraction requests, you must create at least one data extraction request.

1. In the Migration Assessment Application, navigate to **Request** > **Scenario Evaluation**.

2. Choose **Create**.

3. Enter a **Request Name** and choose a **Data Extraction Request** that you created previously.

4. For this specific run of your scenario evaluation, enter an **Evaluation Run Name** and a **Description**. You can start a new evaluation run anytime, for example, for the purpose of comparing new and old data. This can be achieved with the help of **Trigger Analysis**, which is described later on. 

5. Choose **Create**.

      <!-- border -->![Image](Images/Picture18-1.png)

      The new request appears in the list of scenario evaluation requests and the evaluation runs.

      <!-- border -->![Image](Images/Picture19-1.png)

6. By choosing **Delete**, you can delete the unwanted scenario evaluation. Refresh the page to update the list of evaluations.	

      <!-- border -->![Image](Images/Picture20-1.png)

The following additional actions can be performed for a scenario evaluation request:
 
**Open Dashboard**: It displays an analysis of your scenario evaluations with details specific about your integration flows such as assessment categories, adapters, and an overview of the rules used in the evaluation, which can be accessed and downloaded. You can switch between the data of all runs performed for the scenario evaluation request so far.

   <!-- border -->![Image](Images/Picture21-1.png)

   <!-- border -->![Image](Images/Picture22-1.png)

---

### Views available under overview tab in Dashboard

Under the **Overview** tab on the dashboard page, you can see four different views:

1. **Scenarios by Assessment Categories**

      <!-- border -->![Image](Images/Picture25.png)

    **Assessment Categories**: A classification that indicates if your integration scenarios are ready to be migrated to the SAP Integration Suite or if they need some significant changes and how you can proceed.
   
      - **Ready to migrate**: These integration scenarios match to the scenarios offered in SAP Integration Suite. They can be moved to SAP Integration Suite manually or semi-automatically. No manual changes or minor configuration steps might be required to get to a running state.
   
      - **Adjustment required**: These integration scenarios partially match to the scenarios offered in SAP Integration Suite. They can be moved to SAP Integration Suite manually or semi-automatically. Further adjustments to the end-to-end integration process based on best practices are required (for example, File to SFTP conversion, external JMS connection).
   
      - **Evaluation required**: The integration scenarios cannot be migrated to the SAP integration Suite at the moment. Evaluation is needed to see if the interfaces are still required, or if they can be modified to run in the SAP Integration Suite (for example, B2B scenarios). Analyze interfaces that are categorized as **Evaluation required** and verify what aspects of these interfaces were identified as current limitations on the SAP Integration Suite. Check on possible solutions using the SAP Integration Suite documentation or the [Integration Flow Design Guidelines](https://help.sap.com/docs/CLOUD_INTEGRATION/368c481cd6954bdfa5d0435479fd4eaf/6803389050a0487ca16d534583414d2b.html). Also check out the [roadmap](https://roadmaps.sap.com/board?PRODUCT=000D3A47875C1EDB98A8A910864AC24B&range=CURRENT-LAST&q=Migration#Q1%202023) to find out if and when potential limitations might be resolved.

2. **Scenarios by Effort Estimation**

      <!-- border -->![Image](Images/Picture26.png)
   
      For our example, the previous chart shows that there are 1,535 small (S) integration scenarios and 85 medium (M) integration scenarios. An estimation of efforts required to migrate these integration scenarios from the SAP Process Orchestration system to SAP Integration Suite needs to be performed. You'll learn more about these sizing classifications - S, M, L, XL - in the section on **Evaluation Results**.

3. **Sender Adapters**
   
      This section shows the number and types of sender adapters in your on-premise system.

      <!-- border -->![Image](Images/Picture27.png)

4. **Receiver Adapters**
   
      This section shows the number and types of receiver adapters in your on-premise system.

      <!-- border -->![Image](Images/Picture28.png)

---

### Options available under Integration Scenarios tab in Dashboard

On the dashboard under the **Integration Scenarios** tab, you can see the list of all the integration scenarios from your SAP Process Orchestration system. Here, you can also download all the integration scenarios, sizes of the integration scenarios (S, M, L, XL), and assessment categories assigned to each integration scenario, either in .pdf format or in .xslx format.

   <!-- border -->![Image](Images/Picture29.png)


**Trigger Analysis**:  With this functionality, you can schedule an additional evaluation run based on current extracted data. For example, you can trigger an analysis if there's been a rule update. By choosing **Trigger Analysis**, you can rerun an evaluation without having to create a new scenario  evaluation request. This way, you can have multiple evaluation runs for one evaluation request. If a new extraction request is created, you have to run another evaluation request.

   <!-- border -->![Image](Images/Picture30.png)

To compare the old and new evaluations, add the date on the evaluation run name so you can easily distinguish between the two evaluations and compare them in the dashboard later on.

   <!-- border -->![Image](Images/Picture31.png)

   <!-- border -->![Image](Images/Picture32.png)

Under **Scenario Evaluation Run**, you can see all evaluation runs that were triggered for the same extracted data.

**Download**: Evaluation Reports can be downloaded either in English or Simple Chinese language. It is possible to download the reports in **.xslx** file and **.pdf** file formats:

   <!-- border -->![Image](Images/Picture33.png)


**Evaluation Results**

- The **.xslx** file contains all the integration scenarios that were part of the request with migration effort according to the sizing classification, rules applied to them, weight, assessment categories, and status.
  
      <!-- border -->![Image](Images/Picture34.png)

- The **Full Evaluation Results** sheet displays information about rules that were applied for each integration scenario as defined by SAP. Each line includes the rule ID, the value assigned to it, the weight considered for it, and the evaluation assessment category.

      <!-- border -->![Image](Images/Picture35.png)

- In **Eval by Integration scenario** sheet, you can see the list of integration scenarios summarized by total weight evaluated, the t-shirt size, and the overall assessment category. The overall assessment category is classified based on the set of rules triggered to the respective integration scenario. For example, if the integration scenario has at least one rule with the assessment category as “Evaluation required”, then the overall category for that integration scenario is also “Evaluation required”.

      <!-- border -->![Image](Images/Picture36.png)

- The option as **.pdf file** features the previously mentioned details about the integration scenarios while also providing a written summary of adapters and the assessment in general, with charts and tables. This file is suited as a report that can be used for example for management. The following screenshot shows the contents of the .pdf report.

      <!-- border -->![Image](Images/Picture37.png)

**Effort Estimation**

- **Sizing classification** is used for project estimation and capacity planning. It helps you track how much time or effort the migration of interfaces will take. Here, the sizing classification is small (S), medium (M), large (L), extra large (XL). To technically migrate the integration scenarios to the SAP Integration Suite, you need an overall effort estimation in days according to these sizing classifications. 
- The **learning curve** or **repeatable factor** reduces the effort based on the consideration that, in general, a group of interfaces with the same size, system, or module are similar and therefore the migration of the subsequent interfaces is performed quicker than previous ones.
- The assessment result gives information regarding functional readiness to move those scenarios to the SAP Integration Suite for each integration scenario.
  
Congratulations! You've successfully connected migration assessment application to SAP Process Orchestration system, created a data extraction request, created a scenario evaluation request.

### Test yourself 






---
