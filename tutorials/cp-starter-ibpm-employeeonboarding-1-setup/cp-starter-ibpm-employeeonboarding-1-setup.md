---
title: Set Up Business Process Management Services in Cloud Cockpit
description: Set up workflow, business rules and process visibility services in cloud cockpit.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform
---

## Details
### You will learn
  - How to enable and configure workflow, business rules and process visibility services

---

[ACCORDION-BEGIN [Step 1: ](Download multitarget application file)]

Download the `BPMServicesFLP.zip` from  [GitHub](https://github.com/SAP-samples/cloud-process-visibility/releases) in your local file system.

![Download MTA](downloadmta.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access SAP Web IDE)]

1. In your web browser, open the [SAP Cloud Platform Trial cockpit](https://account.hanatrial.ondemand.com/cockpit).

2. Choose **Launch Web IDE**.

3. In the navigation area of **SAP Web IDE**, choose the **Preferences** icon.

    ![Open Preferences](choosepreference.png)

4. Select the **Extensions** preference, and search for **workflow**.

5. Toggle the button to switch on the **Workflow Editor** extension, and **Save** the changes.

    ![Enable Workflow Feature](wfextension.png)

6. Select **Cloud Foundry** preference.

7. Enter the following details, and **Save** the changes:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  **API End Point**  | `https://api.cf.eu10.hana.ondemand.com`
    |  **Organization**   | Your trial organization
    |  **Space**          | Your trial space  

    > In the credentials popup, enter your trial user email and password.

    ![Configure Cloud Foundry](cfpreferences.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Import multitarget application)]

1. In the navigation area of the **SAP Web IDE**, choose the **Development** icon.

2. Right-click the **Workspace** root folder, then choose **Import | File or Project**.

    ![Open Web IDE Development](opendev.png)

3. In the **Import** dialog, browse for the `BPMServicesFLP.zip` file that you downloaded in your local system and choose **OK**.

    ![Import MTA](importzip.png)

4. The multi-target application gets imported under the **Workspace** folder and the file structure is shown below.

    >Ensure that have chosen **Show Hidden Files** to be able to view the `app-router` file structure as shown.

    ![Open Hidden Files](openhiddenfiles.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build and deploy)]

When you build and deploy this application, there will be new service instances created for each workflow, business rules and process visibility service in cloud foundry.

1. Right-click on the `BPMServicesFLP` project and choose **Build | Build**.

    ![Build MTA](build.png)

2. After the build completes, navigate to the **`mta_archives` | `BPMServicesFLP_0.0.1.mtar`** file.

    ![Build MTA](build-logs.png)

    ![Build MTA](buildarchives.png)


3. Right-click `BPMServicesFLP_0.0.1.mtar` and choose **Deploy | Deploy to SAP Cloud Platform**.

    ![Deploy MTA](deploymta.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Assign roles)]

1. In your web browser, open the cockpit of [SAP Cloud Platform Trial](https://account.hanatrial.ondemand.com/cockpit).

2. Click **Enter Your Trial Account**

3. Open **Security | Role Collections** and click **New Role Collection** to add new roles.

    ![New Role Collection](newrolecollection.png)

    Enter **Name** as **`BPMServices`** and **Description** as **Roles to access Intelligent BPM services**. Once done, **Save** the changes.

    ![New Role Collection](createrolecollection.png)

4. Click **`BPMServices`** to navigate into role collection to add roles.

    ![Open Role Collection](openrolecollection.png)

5. Click **Add Role** and select these roles to add them to the role collection one by one. In **Add Role** popup, click **Save** after each selection.


    |  Application Identifier     | Role Template and Role
    |  :------------------------- | :------------------------
    |  bpmrulebroker              | RuleRepositorySuperUser
    |                             | RuleRuntimeSuperUser
    |  pvreuserservice            | PVDeveloper
    |                             | PVAdmin
    |                             | PVEventSender
    |                             | PVOperator
    |                             | PVTenantOperator
    | workflow                    | WorkflowAdmin
    |                             | WorkflowContextViewer
    |                             | WorkflowDeveloper
    |                             | WorkflowInitiator
    |                             | WorkflowInitiator

    ![Roles](addrole.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create destination for API Business Hub)]

The sample business rules used in this scenario is published in API Business Hub. This destination will be used in SAP Cloud Platform Business Rules to connect to API Business Hub to discover and import sample rules.

1. Go back to the subaccount **trial** and switch to **Connectivity | Destinations** from the left-hand navigation.

2. Click **New Destination**.

3. Enter the following details:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | `BUSINESSRULES_APIHUB`
    |  URL            | `https://api.sap.com`
    |  Authentication | `BasicAuthentication`
    |  User           | your trial user
    |  Password       | your trial password

    ![Create Destination](createdestination-apihub.png)

4. **Save** the destination.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create destination for SuccessFactors APIs)]

The sample workflow calls SuccessFactors to get the user details of the new hire and the team members. A destination (SFSF) is used in the Workflow model to call these APIs.

Choose **New Destination**, and enter the following data:

|  Field Name     | Value
|  :------------- | :-------------
|  Name           | `SFSF`
|  URL            | API URL based on your SuccessFactors Data Center, see the `[note](https://apps.support.sap.com/sap/support/knowledge/public/en/2215682)`
|  Authentication | `BasicAuthentication`
|  User           | `UserName@CompanyID`. Where `CompanyID` is the SuccessFactors company ID.
|  Password       | given SuccessFactors password for the user name

![Configure Destination](createdestination-sfsf.png)

> With your S-user account, you can get a new SuccessFactors test tenant at: [Partner Demo Request Tool](https://hcmcloudops.successfactors.com/DemoInstance/PartnerDemo).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create destination for Business Rules APIs)]

The sample workflow calls business rules to determine the list of equipment needed for the new hire. A destination (`BUSINESS_RULES`) is used in the Workflow model to call the business rules runtime APIs.

Choose **New Destination**, and enter the following data:

|  Field Name        | Value
|  :---------------- | :-------------
|  Name              | `BUSINESS_RULES`
|  URL               | `https://bpmruleruntime.cfapps.eu10.hana.ondemand.com/rules-service`
|  Authentication    | `OAuth2ClientCredentials`
|  Client ID         | `<use your client ID from the Business Rules service instance>`
|  Client Secret     | `<use your secret from the Business Rules service instance>`
|  Token Service URL | `https://<userid>.authentication.eu10.hana.ondemand.com/oauth/token` where `userid` is your trial account user id

![Configure Destination](createdestination-rules.png)

>To get Client ID, Client Secret and Token Service URL: (a) navigate into **dev** space, (b) select **Service Instances**, (c) search for **rules** instance, and then select **`businessrules`** service instance and (d) navigate into **`businessrules`** to get `clientid`, `clientsecret`, and `url`.
![Get Security Token](getsecurity.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Access applications)]

1. Click on **Spaces** and then navigate into **dev** space.

2. In **dev** space, click on **Applications** and from among the available applications select `BPMServicesFLP_appRouter` application.

    ![Open Application](openapprouter.png)

3. In the **Overview** section, click app-router link to open workflow, business rules and process visibility applications.

    > Logon with your trial user name and password to the application.

    ![Open FLP](approuterlink.png)

    This will open the SAP Fiori launchpad with **Workflow** and **Process Visibility** tiles.

    ![Open BPM FLP](bpmFLP.png)

    In the same link, append `/comsapbpmrule.ruleeditor/index.html` after app-router link to open business rules editor.

    > For example: `https://trial-userid-dev-bpmservicesflp-approuter.cfapps.eu10.hana.ondemand.com/comsapbpmrule.ruleeditor/index.html`

    ![Open Rule Editor](openruleeditor.png)


[DONE]
[ACCORDION-END]

---
