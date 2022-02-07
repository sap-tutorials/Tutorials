---
title: Provision Users into your SAP BTP ABAP Environment
description: Provision and authorize users for ABAP development via Cloud Identity Services in one or more target systems
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>identity-authentication, topic>cloud, tutorial>license]
primary_tag: software-product>sap-btp--abap-environment
---

## Prerequisites
 - You have installed and set up ABAP Development Tools for Eclipse, see <https://tools.hana.ondemand.com/#abap>
 - You have an **SAP Business Technology Platform** customer subaccount and have prepared the following
    - Subscription to **Cloud Identity Services**
    - Established trust to your **SAP Cloud Identity Services - Identity Authentication tenant**, see [ABAP Environment Documentation: Setup of a Custom Identity Service](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/550251abaf49432bbaa65147b65a1f39.html)
    - Created an **ABAP environment** service instance for custom development with
        - An SAP Fiori launchpad business role for custom ABAP development created from template `SAP_BR_DEVELOPER`
        - A service key for ADT integration
 - You have one or more users with authorization for
    - User and Group Management in your **SAP Cloud Identity Services - Identity Authentication tenant**
    - Space development in your ABAP system service instance's Cloud Foundry space

## Details
### You will learn
  - How to create and group developer identities in your **SAP Cloud Identity Services - Identity Authentication tenant** (IAS)
  - How to enable Identity Provisioning in your IAS
  - How to configure and run Identity Provisioning
  - How to connect Eclipse with the ABAP environment

Additional information:

  - In this use case the **SAP Cloud Identity Services - Identity Authentication tenant** is used as an identity provider and not as a proxy to another identity provider.
  - [Documentation: SAP Cloud Identity Services – Identity Provisioning](https://help.sap.com/viewer/f48e822d6d484fa5ade7dda78b64d9f5/Cloud/en-US/2d2685d469a54a56b886105a06ccdae6.html)

---

[ACCORDION-BEGIN [Step 1: ](Create User in IAS Tenant)]
To create your development user's identity in your IAS, log on with your User Management Administrator to your Identity Authentication tenant's administration UI (URL ends with path `/admin`, for example https://rapworkshop.accounts.ondemand.com/admin).

1. Navigate to **Users & Authorizations** > **User Management**

    ![Open User Management in IAS](IAS_user_mgmt_open.png)

2. Select **Add User** to start the creation process of a user.

    ![Press Add button for new user](IAS_user_add.png)

3. Fill the personal information for the user and select **Save**

    ![Configure properties of new user](IAS_user_configure_new.png)

4. The new user is now displayed in the list of users.

    ![List entry for new user](IAS_user_created.png)

>Note that the IAS user will receive an email to activate the account before being able to log on with a local user somewhere for the first time.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create Developer Group and Assign User)]
To bundle developers users, create a corresponding user group in the IAS and assign the users to it.

1. Navigate to **Users & Authorizations** > **User Groups** and select **Create**

    ![Start User Group creation](IAS_group_create.png)

2. In the Create Group dialog enter a **Name** and **Display Name** and select **Create**

    > For the group name, please use the identical name you have set for the business role in your ABAP environment (created from template `SAP_BR_DEVELOPER`, see prerequisites).

    ![Configure properties of new group](IAS_group_configure_new.png)

3. To add users to the group select **Add**.

    ![Start user adding to group](IAS_group_add_user.png)

4. Search for the user that you have created earlier, select it, and choose **Save**

    ![Save User Group](IAS_group_add_user_save.png)

5. The user is now displayed in the user group list.

    ![List entry for user in group](IAS_group_add_user_result.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Authorize Identity Provisioning Manager)]
Authorize an Administrator user for Identity Provisioning Management.

Navigate to **Users & Authorizations** > **Administrators** choose the Administrator user, slide the toggle button for **Manage Identity Provisioning** to **ON** and select **Save**

![Set Identity Provisioning Management](IAS_IPS_admin_configure.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure Access to Source via Technical User)]
In this example the IAS itself is used as a source for users and user groups that can be provisioned to other systems.
To allow identity provisioning to read users and groups from the IAS, you need a technical user with corresponding permissions.

1. Navigate to **Users & Authorizations** > **Administrators**

2. Select **Add** and choose **System**

    ![Start Administrator creation](IAS_admin_add.png)

3. Provide a **Name** for the system, for example `ips_tutorial_admin`.
Make sure to only set authorizations for **Read Users** and **Manage Groups** which are both needed to read users and groups during identity provisioning, **Save** your changes.

    ![Configure Administrator Authorizations](IAS_admin_configure.png)

4. A new dialog to **Configure User ID and Password** is displayed. Set the password and select **Save**. The User ID is generated automatically.

    ![Set Password](ias_admin_set_pw.png)

5. Go back to the **Configure User ID and Password** section and **Copy** the generated **User ID** of the technical user.

    ![Copy Technical User's ID](IAS_admin_copy_user_id.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure Source in Identity Provisioning Service)]
Identity provisioning requires to configure a so-called source system for user and user group data.

1. Log on with your Identity Provisioning Manager user to your Identity Authentication tenant's identity provisioning UI (URL ends with path `/ips`, for example https://rapworkshop.accounts.ondemand.com/ips).

2. Select the **Source Systems** tile

    ![Source System Tile](ips_source_systems_tile.png)

3. To start the creation, select **Add**

    ![Add Source System button](ips_source_system_add.png)

4. To simplify the system creation and to reduce the risk of errors, this tutorial provides a template JSON file for the source system. Download [`ips_system_template_source.json`](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/abap-env-ips/ips_system_template_source.json) locally.

5. Define the system by uploading the JSON file via **Browse** in the IPS system UI.

    ![Browse for source system template file](ips_source_systems_browse.png)

6. Adapt the values to your needs and provide the mandatory values for `URL`, `User` and `Password` as shown below.

    Alternatively, you can configure everything manually.

    Details:

    |  Label     | Value
    |  :------------- | :-------------
    |  Type           | Identity Authentication
    |  System Name           | For example **`My IAS ABAP Developers`**

    Properties:

    |  Name     | Value
    |  :------------- | :-------------
    |  **`Type`**           | **`HTTP`**
    |  **`ProxyType`**           | **`Internet`**
    |  **`URL`**          | your IAS URL, for example <https://rapworkshop.accounts.ondemand.com>
    |  **`Authentication`** | **`BasicAuthentication`**
    |  **`User`**    | User ID of technical user
    |  **`Password`**   | Password of technical user
    |  **`ias.user.filter`**   | **`groups.display eq "BR_IPS_TUTORIAL_DEVELOPER"`**
    |  **`ias.group.filter`**   | **`displayName eq "BR_IPS_TUTORIAL_DEVELOPER"`**

7. **Save** your changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure access to Target via Communication User)]
To enable the IAS to create users and assign business roles in the target system, that system has to provide the corresponding authorization to the IAS.

1. Navigate to **Services > Instances and Subscriptions** in your SAP BTP subaccount, search for your ABAP environment instance and select **Create Service Key**

    ![Start Service Key creation for ABAP instance ](btp_service_key_create.png)

2. In the **New Service Key** dialog, provide a name and copy & paste the following JSON code:


    ```JSON
    {
      "scenario_id":"SAP_COM_0193",
      "type":"basic"
    }
    ```

3. **Create** the new service key.

    ![Configure new service key](btp_service_key_configure_new.png)

    >This service key creation automatically creates a communication user (1), communication system (2) and communication arrangement (3) for communication scenario `SAP_COM_0193` (4) in the ABAP environment instance.
    >
    ![Communication Artefacts for IPS in ABAP instance](ABAP_FLP_CA_created.png)
    >
    Communication scenario `SAP_COM_0193` exposes all the needed services for identity provisioning integration. With the communication user credentials, you can make inbound calls to that system to provision users and assign roles per groups.

4. The credentials created for the communication user are also available in the subaccount. To view them, navigate to **Services > Instances and Subscriptions** and select your service instance.

    ![Open Service Instance Details](BTP_ABAP_env_view_details.png)

5. From the Actions menu, choose **View Credentials**.

    ![View Credentials of service instance](BTP_ABAP_env_view_credentials.png)

6. Choose the credentials that you have set earlier as service key for IPS.
Copy the **`username`**, **`password`** and **`url`** value for the next step.     

    ![Get communication user credentials from service key](btp_service_key_get_credentials.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Configure Target in Identity Provisioning Service )]
Identity provisioning requires to configure a so-called target system for user and user group data.
In this example, the target systems is an ABAP system in SAP BTP.

1. Log on with your Identity Provisioning Manager user to your Identity Authentication tenant's identity provisioning UI (URL ends with path `/ips`, for example <https://rapworkshop.accounts.ondemand.com/ips>).

2. Select the **Target Systems** tile

    ![Source Target Tile](ips_target_systems_tile.png)

3. To start the Creation, select **Add**

    ![Add Target System button](ips_target_system_add.png)

4. To simplify the system creation and reduce the risk of errors, this tutorial provides a template JSON file for the source system. Download [`ips_system_template_target.json`](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/abap-env-ips/ips_system_template_target.json) locally.

5. Define the system by uploading the JSON file via **Browse** in the IPS System UI.

    ![Browse for target system template file](ips_target_systems_browse.png)

6. Adapt the values to your needs and provide the mandatory values for `URL`, `User` and `Password` as shown below.

    Alternatively, you can configure everything manually.

    Details:

    |  Label     | Value
    |  :------------- | :-------------
    |  Type           | SAP BTP ABAP environment
    |  System Name           | For example **`My ABAP instance`**
    |  Description           | For example **`System to receive provisioned Developer Users`**
    |  Source System           | Choose the one created earlier from the dropdown

    Properties:

    |  Name     | Value
    |  :------------- | :-------------
    |  **`Type`**           | **`HTTP`**
    |  **`ProxyType`**           | **`Internet`**
    |  **`URL`**          | The URL of your ABAP environment
    |  **`Authentication`** | **`BasicAuthentication`**
    |  **`User`**    | User name of ABAP instance communication user
    |  **`Password`**   | Password of ABAP instance communication user
    |  **`ips.date.variable.format`**   | **`yyyy-MM-dd`**

7. **Save** your changes.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Run Identity Provisioning)]
After the source and target Systems have been created and connected with each other you can run the Identity provisioning.

1. Switch to **Source Systems**

    ![Navigate from Target System to Source Systems](IPS_target_system_2_source_systems.png)

2. Open your source system and select the **Jobs** tab.

3. Choose **Run Now**

    ![Run Identity Provisioning Job](IPS_source_system_run_job.png)

4. To check the status of the job run, select **Job Logs** from the navigation pane.

    ![Navigate to Job Logs](IPS_Job_logs_open.png)

5. Search for your log by checking the source system name and time and make sure the status is **Success**.

    ![Job finished successfully](IPS_Job_log_success.png)

>If the run did not finish successfully, you can navigate to the log and follow the instructions there to analyze and solve the problem. See also [Guided Answers: Identity Provisioning Troubleshooting](https://ga.support.sap.com/dtp/viewer/#/tree/2065/actions/26547:29111:29114:27412).


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Log On to ABAP Environment in Eclipse)]
Now that the Developer user has been provisioned and authorized in the ABAP environment for ABAP development, you can connect the user to the system by using ABAP Development Tools for Eclipse.

1. Open your Eclipse and navigate to **File > New > Project**.

    ![Start project creation in eclipse](ADT_project_new.png)

2. Choose **ABAP Cloud Project** and select **Next**

    ![Choose to create ABAP Cloud project](ADT_project_new_cloud.png)

3. Choose **SAP BTP ABAP Environment** > **Use a Service Key**  and select **Next**

    ![Choose to create from service key](ADT_project_new_service_key.png)

4. Paste the service key for Eclipse integration (see prerequisites)

    ![Paste Service Key for ADT usage](ADT_project_new_service_key_paste.png)

5. **Copy Logon URL to Clipboard**

    ![Copy Logon URL to Clipboard](ADT_project_new_open_logon.png)

6. Open an incognito browser window and paste the logon URL into the address line.
Choose your right identity provider URL.

    ![Identity Provider choice](ADT_project_new_logon_chose_idp.png)

7. Enter the credentials of the Developer User and log on.

    ![Log on with provisioned developer](ADT_project_new_logon_with_developer.png)

8. A success message is displayed and the browser window can be closed.

    ![Log on for ADT succeeded](ADT_project_new_logon_with_developer_success.png)

9. In the project wizard in Eclipse, check the ABAP environment and user data, that are displayed in the **Service Instance Connection** dialog and select **Finish**

    ![Finish ABAP Cloud project creation](ADT_project_new_finish.png)

10. The new project is displayed and you can start developing.

    ![See ABAP Cloud project in ADT navigation](ADT_project_created.png)




[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 10: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]


---
