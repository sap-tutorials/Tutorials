---
parser: v2
auto_validation: true
time: 30
tags: [ tutorial>beginner, software-product>sap-hana-cloud, software-product-function>sap-hana-cloud\\,-data-lake]
primary_tag: software-product>sap-hana-cloud
---

# Access a Standalone Data Lake in SAP HANA Cloud
<!-- description --> Learn to access your standalone data lake in SAP HANA Cloud.

## Prerequisites
 - You have completed the tutorial [provisioning of a standalone data lake in SAP HANA Cloud](hana-cloud-hdl-getting-started-1) using a free tier, trial, or production account.

## You will learn
  - The different ways to access your standalone data lake instance
  - How to connect your standalone data lake using SAP HANA database explorer
  - How to connect your standalone data lake using SAP Interactive SQL ( **dbisql** )
  - How to connect your standalone data lake using **isql**



## Intro
After completing the provisioning of your standalone data lake in SAP HANA Cloud, you can access the data lake and start storing, querying and manipulating your data. In this tutorial, you will learn some of the ways you can do that, focusing mostly on how to access and interact with your standalone data lake using **SAP HANA database explorer**.

You can also learn how to access the same instance using SAP Interactive SQL (dbisql) and isql, in case you are familiar with those tools. Before you access the data lake instance, it is important to understand the function of users, roles and privileges while interacting with your instance.

---

### Introduction to users, roles, and privileges

SAP HANA Cloud, data lake defines user permissions and privileges using a **role-based security model**. A **role** is a set of **privileges** that can be assigned to a user. As a user, you can grant or revoke these roles or privileges. This means that your privileges as a user change depending on the change in the privileges assigned to your role.

When you create a new data lake instance using the default user **`HDLADMIN`**, you are automatically granted all system and object-level privileges in the data lake. You can also create a new user, thereby granting the user with the public system role automatically. This role provides access to view data, execute system stored procedures, and grant roles and privileges to the user.

When you create a new object, you become the default owner of the object unless you define otherwise. This gives you privileges to modify the structure of the table and grant further privileges to other data lake users. Although, you cannot load data into a table based on being the owner, unless you assign `INSERT` privileges to the role.

To learn more about users, roles and privileges in SAP HANA Cloud, data lake, please see our [technical documentation](https://help.sap.com/viewer/745778e524f74bb4af87460cca5e62c4/LATEST/en-US/a449907984f21015977a8621bb55d909.html).




### Confirm your data lake is running

Ensure that your standalone data lake instance is currently running before trying to connect to it using SAP HANA database explorer.

1.	To do this, navigate to the **SAP HANA Cloud Central** in the subaccount level in SAP BTP cockpit.

2.	Locate your standalone data lake instance details by selecting to view all instances.

3.	If your instance is showing any status other than `RUNNING`, click on the **three dots** button assigned to the instance for more options.

4.	Now, select the option to **Start** the instance and wait until it has the status **`RUNNING`**.

    ![DL Running](dl-running.png)



### Connection options

>You can use different tools to connect to a standalone data lake in SAP HANA Cloud. In this tutorial, we will show you four different options you can choose from:

>* Option 1: SQL Console in SAP HANA Cloud Central
>* Option 2: Interactive SQL (dbisql) - graphical interface
>* Option 3: Interactive SQL command line interface (CLI)
>* Option 4: isql

In the next steps, you can learn how to use each of these connection methods. Once you have decided which option you would like to use to connect, following the other steps is not mandatory.


### Connect to data lake


> **For this step, select your preferred connection method by clicking on the options under the step title.**
>
> ![Options Screenshot](dl-connect-options.png)

[OPTION BEGIN [SQL Console]]
After confirming the `RUNNING` status, click on the **three dots** under the **Actions** column and select the option to **Open SQL Console**. This will take you to the SQL Console tab and open a console connected to your data lake.

>You can also navigate to the same destination by selecting the option to **Open SAP HANA Database Explorer**, then opening a SQL console.


1.	Look for the name of your standalone data lake from the top-right corner of the screen.

2.  You can confirm the connection to your standalone data lake from the connection status provided.

    ![Confirm connection](connection-confirm.png)

[OPTION END]
[OPTION BEGIN [dbisql]]
>Connecting to a data lake instance with Interactive SQL is another option besides using the SAP HANA database explorer. If you want to connect using Interactive SQL (dbisql), you need to have it installed on your local system. You can download the Interactive SQL using the link provided in the [technical documentation](https://help.sap.com/viewer/a895964984f210158925ce02750eb580/LATEST/en-US/2fdb23a4fb364e06ace0eea0c9a4afec.html).

There are two ways to connecting to an instance through an Interactive SQL- from the graphical user interface or from the command line.

These are the steps towards connecting to your instance through the graphical user interface of Interactive SQL:

1.	Launch the SAP Interactive SQL in your system. The Connect dialog box should appear as shown below.

    ![Open dbisql](ss-01-open-dbisql.png)

2.	Select **Change database type** and choose **SAP HANA Cloud Data Lake**.

    ![Change Database type](ss-02-change-database-type.png)

3.	After selecting the database type, ensure that the encryption parameters placed under the security tab have been pre-filled, as shown below.

    ![Check security](ss-03-check-security.png)

4. Enter the credentials for your data lake instance under the Identification section. For example, in the **User ID** field, enter HDLADMIN and then enter your **password** that you set while creating your instance.

    ![User credentials](ss-04-user-credentials.png)


    Next, you must enter your Instance ID and Landscape. These details can be found in the SAP HANA Cloud Central wizard of your instance in the SAP HANA Cloud.

5.	Open the **SAP HANA Cloud Central** wizard and locate your standalone data lake from the list of all instances. Select the three dots under the Actions column for your instance and choose **Copy Instance ID** to copy the Instance ID to the clipboard. Then, paste the Instance ID into the Instance ID field in the Interactive SQL.

    ![Copy Instance ID](hdl-instance-id.png)

6.	Switch back to the SAP HANA Cloud Central window. Again, select Actions to choose **Copy SQL Endpoint**. Paste the SQL Endpoint in a notepad file for viewing. The SQL Endpoint is composed of the Instance ID, Landscape and Port.

7.	Paste the information on the Landscape that you isolated from the SQL Endpoint into the Interactive SQL, and click **Connect**.

    ![Instance ID and Landscape](ss-06-instance-and-landscape.png)

8.	Now, you should be connected to your data lake instance thorough Interactive SQL, as seen in the below image.

    ![Connected to Instance](ss-07-connected-instance.png)

[OPTION END]
[OPTION BEGIN [CLI]]

If you would like to connect to your data lake instance through an Interactive SQL from the command line, please follow the instructions given below.

1.	Connecting to a data lake instance through an Interactive SQL from the command line requires a Connection String. To extract the connection string, ensure all fields on the Identification tab are filled in. Select **Tools**, then choose **Copy Connection String to Clipboard**.

    ![Copy Connection string](ss-08-copy-connection-string.png)

2.	After pasting the connection string in a notepad file, you can the distinguish the parts as seen below.  **The password field must be filled using the password for your data lake instance before using this string to connect from the command line.** The connecting string consists of the port number at the end of Landscape information and is followed by the encryption parameters.

    ![Connection string](ss-09-connection-string.png)


3.	Now, open a Command Prompt window in your system. To begin an Interactive SQL session, execute the following command given below.

    ```Shell
    dbisql -c <CONNECTION_STRING>
    ```

    **For example**, paste the connection string as shown below.

    ```Shell
    dbisql -c "UID=HDLADMIN;PWD=SamplePassword;host=a111111a-1a11-11aa-a11a-1a1a11a1a111.iq.hdl.test.hanacloud.ondemand.com:443;ENC=TLS(tls_type=rsa;direct=yes)"
    ```

    ![Command Prompt](ss-10-command-prompt.png)


4.	An Interactive SQL window should show up on your screen and you are now connected to your data lake instance, as seen below.

    ![Call dbisql](ss-11-call-dbisql.png)

5.	You can also setup the connection without opening the graphical user interface of the Interactive SQL, for which you must simply add **'-nogui'** at the end of your connection string.

    **For example**, the command will look like this:

    ```Shell
    dbisql -c "UID=HDLADMIN;PWD=SamplePassword;host=a111111a-1a11-11aa-a11a-1a1a11a1a111.iq.hdl.test.hanacloud.ondemand.com:443;ENC=TLS(tls_type=rsa;direct=yes)" -nogui
    ```

    ![No GUI dbisql](ss-12-no-gui-dbisql.png)

6.	Your User ID will be displayed after the execution of the above command which indicates that a connection to your data lake instance has successfully been made.

[OPTION END]
[OPTION BEGIN [isql]]

>**Attention**: The following instructions apply to you if you're using isql from the on-premise SAP IQ 16.1 version. Using any other open-source isql versions may lead to errors. Therefore, it's highly recommended to use the same version to avoid any errors.


Set up an open client connection to your database. Open a Linux instance  and use the superuser account. For this example, your `$SYBASE` environment variable points to `/opt/sap` as your root folder.

To use isql, you need to have an **interfaces file**, which you can create in the vim editor. Follow these steps to create the interfaces file:

1.	Navigate to the root folder using command:  `/opt/sap/OCS-16_0/bin`

    ![Root folder](ss-13-root-folder.png)

2.	Enter the vim editor as the superuser account

    a.	`sudo vim interfaces`

    b.	`enter password`

    ![Connected to Root folder](ss-14-connected-root-folder.png)

3.	Once you are in the vim editor, you need the SQL endpoint of your data lake instance. Retrieve it from the SAP HANA Cloud Central as shown earlier.

4.	Enter your respective credentials using the following format:

    ```
    <Instance_Name>

    query tcp ether <InstanceID>.<Landscape> <Port> ssl="CN=hanacloud.ondemand.com"
    ```

    **For example**, as shown below:

    ```
    DEMO_INSTANCE

    query tcp ether a111111a-1a11-11aa-a11a-1a1a11a1a111.iq.hdl.beta-us21.hanacloud.ondemand.com 443 ssl="CN=hanacloud.ondemand.com"
    ```

    >Note: There must be a tab used at the beginning of the second line, prior to query. Using spaces will not work.


    ![SQL Endpoint query](ss-15-sql-endpoint-query.png)

5.	Once you have entered your details, save the interfaces file, and exit the vim editor.

6.	Before connecting to a database, run the command `source SYBASE.sh`

7.	The successful execution of this command can be verified by running the command `which isql`

    ![Verify SQL query](ss-16-verify-query.png)

8.	If the editor shows location of the isql, this indicates that now you can start an isql session.

9.	Start an isql session and connect to your database by running the following command and entering your credentials:

    ```Shell
    isql -U <username> -S <Instance_Name> -I<path to interfaces file>
    ```

10.	You will be prompted to enter your password.

    ![Enter Password](ss-17-enter-password.png)

11.	When the editor shows `1>`, you are connected to your database and may run queries.

    For Example, Test query: `select @@ version`

    ![Test query](ss-18-test-query.png)

12. If you encounter the following error while trying to start an isql session:

    ![Troubleshoot](ss-19-troubleshoot.png)

    Run the following command before trying again:

    ```Shell
    unset LANG
    ```

    >**Attention**: This error was observed while using Ubuntu on a virtual Linux machine. This error will not occur when supported programs are used.


[OPTION END]

>In this tutorial, you have learned how to access your standalone data lake in SAP HANA Cloud. In the next tutorial, you will see how to load data into your standalone data lake.


### Knowledge Check




---
