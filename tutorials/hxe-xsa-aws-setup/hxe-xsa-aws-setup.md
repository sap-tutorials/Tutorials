---
title: Set up SAP HANA, express edition on Amazon Web Services (XS Advanced)
description: Set up your SAP HANA, express edition with XS Advanced on Amazon Web Services.
primary_tag: products>sap-hana\,-express-edition
auto_validation: true
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition ]
time: 20
---

## Details
### You will learn  
This tutorial will walk you through the setup process of SAP HANA, express edition, and the XS Advanced applications platforms from an Amazon Marketplace Image in Amazon Web Services

[ACCORDION-BEGIN [Step 1: ](Sign in and select the AMI)]

Sign in or create an account on [Amazon Web Services Portal](https://portal.aws.amazon.com/billing/signup#/start)

Access the [Amazon Web Services Marketplace](https://aws.amazon.com/marketplace/).

![Amazon Web Services Marketplace](_01.png)

Using the search bar at the top, enter **SAP HANA, express edition** and press **Enter** or click on the search icon.

After you perform the search, you will be presented with two options. The right one will depend on what you are looking to do.

**This tutorial covers the database + XS Advanced application option**.

  -  **SAP HANA, express edition:**

      The core database server includes the in-memory database, to which you can connect using different drivers and connectors for different languages (`JDBC`, `ODBC`, `ADO.NET`, driver for `Python`, `Node.js` modules, etc.).

      You can use the database services and the advanced analytics processing engines (e.g., geospatial, text analytics, Extended Machine Library or Predictive Analytics Library, etc.).

      For details about this option, go to the following tutorial: <https://www.sap.com/developer/tutorials/hxe-aws-setup.html>

  -  **SAP HANA, express edition (server + applications):**

      The database server plus the XS Advanced platform includes the database server and the micro-services oriented, polyglot development platform.

      It includes the SAP Web IDE for SAP HANA, the administration cockpit among other applications like SHINE, Enterprise Architecture Designer and Smart Data Streaming. OData V2 and V4 support, Node.js and Java runtime environments are also included.

      This option requires more resources.

Select the **SAP HANA, express edition (server +  applications)** option.

Scroll down and review the **Pricing** and additional information.

Click on **Continue to Subscribe**

![Amazon Web Services Marketplace](_02.png)

Review and accept the software **Terms and Conditions**.

Click **Continue to Configuration**.

![Amazon Web Services Marketplace](_03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Choose instance type)]

Review the pricing, choose a region and click **Continue to Launch**.

![Amazon Web Services Marketplace](_04.png)

Select:

 - Choose Action:  **Launch from Website**.
 - EC2 Instance Type : **`m4.xlarge`**

![Amazon Web Services Marketplace](_05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure network)]

Select or create a **`VPC`**

![Amazon Web Services Marketplace](_06.png)

Once the **`VPC`** is created or selected, select a **Subnet**

![Amazon Web Services Marketplace](_07.png)

Click on **Create a security group based on Seller Settings**.

![Amazon Web Services Marketplace](_08.png)

Name your security group, add a description and click **Save**

![Configure network](_09.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a key pair)]

Click **Create a key pair**.

![Amazon Web Services Marketplace](_10.png)

The **EC2 Dashboard** will open.

Click on **Create a Key pair**.

![Amazon Web Services Marketplace](_11.png)

Set a name and click **Create**

![Amazon Web Services Marketplace](_12.png)

**Download** the generated file and store it in a secure location.

> ### Note:  **You will need the generated file to access your instance**
>
> This file is your key to access the server. If you lose your `.pem` file you will not be able to access the instance later.
>

Go back to the instance creation page, and click the ***refresh*** buttons.

Choose the newly-create setting.

![Amazon Web Services Marketplace](_13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Launch your instance)]

Once the configuration is completed, click on the **Launch** button.

![Amazon Web Services Marketplace](_14.png)

Click on the **[EC2 Console](https://console.aws.amazon.com/ec2)** link to see your instance

![Amazon Web Services Marketplace](_15.png)

From the **EC2 Console**, you will be able to monitor the instance initialization.

![Amazon Web Services Marketplace](_16.png)

You may need to refresh the page to get the **Status Checks** updated.

Once the instance is ready, you can connect to it using an SSH client.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Connect to your instance)]
There are different methods to connect to your instance depending on the system you are using to connect from.

You can use a **standalone SSH client** like **`PuTTY`**.

> ### **Note:** There is also a option to use **Java SSH Client** directly from your browser which uses **`MindTerm`**.
>
>However this require the use of NPAPI, which is being deprecated and unsupported by most browsers like Chrome, Microsoft Edge or recent version of Firefox.

>Therefore, it is recommended to use a ***standalone SSH client***.

From the **EC2 Console**, select your EC2 instance:

![Amazon Web Services Marketplace](_17.png)

Copy the **`IPv4` Public IP** or the **Public DNS (`IPv4`)** address from the instance information.

Then follow the instruction based on your client platform using the ***Public DNS*** host name as highlighted above.

### **Windows platform**:

If you are using a Windows PC, you will need to download and install:

- an SSH client, such as **`PuTTY`**
- a key pair converter such as **`PuTTYgen`** (one time operation)

#### **Create private key file**

***This is a one time operation.***

Open **`PuTTYgen`**.

Use the **Load** button to import your `X.509` certificate **`.pem`** file.

Use the **Save private key** button to save your private key file as **`.ppk`**.

![PuTTY](_18-00.png)

**Connect**:

Start a new **`PuTTY`** session.

Enter the **`IPv4` Public IP** or the **Public DNS (`IPv4`)** address in the **Host Name (or IP address)** field.

![PuTTY](_18-01.png)

On the left tree, select **Connection > SSH > Auth**, select the generated private key **`.ppk`** file:

![PuTTY](_18-02.png)

Click on **Open**.

When prompted, use **`ec2-user`** as user.

Check the following guide for further information: [Connecting to Your Linux Instance from Windows Using PuTTY](<http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html)

### **Mac or Linux platform**

You can use command `ssh` from a terminal as follows:

```shell
ssh -i /path/my-key-pair.pem ec2-user@public_ip_address
```

For more details about standalone SSH client option, you can check the [Connecting to Your Linux Instance Using SSH]( https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AccessingInstancesLinux.html) guide.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure your server)]

Once you have established an SSH session, use the **`ec2-user`** user to log in:

![PuTTY](_19.png)

The prompt should become:

```
ec2-user@hxehost:~>
```

Change the password for `hxeadm` using the following command:

```shell
sudo passwd hxeadm
```

As follows:

![PuTTY](_20.png)


Switch now to user **`hxeadm`** to start configuring your instance:

```ssh
sudo su - hxeadm
```

![PuTTY](_21.png)

You will be prompted for the master password for your database. **If you lose this password, you will lose access to your instance**.

> ### **Note: The password has to follow these rules**
> The master password you will specify is used for the `<sid>adm` and `sapadm` OS users, the telemetry technical user, and the HANA SYSTEM user.
>
> SAP HANA, express edition requires a very strong password that complies with these rules:
>
> - At least 8 characters
> - At least 1 uppercase letter
> - At least 1 lowercase letter
> - At least 1 number
> - Can contain special characters, but not _&grave;_ (backtick), _&#36;_ (dollar sign),  _&#92;_ (backslash), _&#39;_ (single quote), or _&quot;_ (double quotes)
> - Cannot contain dictionary words
> - Cannot contain simplistic or systematic values, like strings in ascending or descending numerical or alphabetical order
>

Enter the password you want to set for your administration user.

![PuTTY](_22.png)

You will not need a proxy to access the internet, so enter **N** (no) when prompted.

You will also be asked if you want to wait for XSA configuration to finish.

The recommended option is **N** (no), so that the installation continues even if you lose connectivity.

![PuTTY](_23.png)

The setup script will perform different configurations for the next 10 minutes.

After the core database has been set up, you should see a success message.

This means the XS Advanced applications are now being configured.

![PuTTY](_24.png)

Repeat the following command to check if the XS Advanced installation has finished, or wait until you get a success message if you chose to wait until the configuration finishes.

```shell
cat /usr/sap/HXE/home/xsa_config_status
```

You will see `status=success` once it has finished:

![PuTTY](_25.png)

> Note: If the status is not successful, you can check the log to see what errors occurred using the command, for example:
> `cat /var/tmp/hdb_init_config_2017-11-27_19.36.24.log`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check your instance)]

You can now check your instance is running properly. From the console log, use the following command as user `hxeadm` to see which processes your database is running:

```ssh
HDB info
```
You should see (at least) the following processes:

- `hdbnameserver`
- `hdbcompileserver`
- `hdbindexserver`
- `hdbwebdispatcher`

![PuTTY](_26.png)

> ### **Note**: You can run commands `HDB start` and `HDB stop` respectively to start and stop your SAP HANA instance

Although the database is running, the XS Advanced applications and services may still be loading. Check the progress by running the following command in the XS Command Line Interface.

> ### **Note**: You can also execute the CLI locally from your computer. Make sure to download it from the [download site using the Download Manager](https://www.sap.com/developer/topics/sap-hana-express.html) and to map your hosts file as explained in step 9.

```
xs-admin-login
xs apps | grep webide
```

When prompted, use the password you provided at the beginning of the installation script.

Repeat the second command until you see `STARTED` and **`1/1`** for applications `webide`.

![PuTTY](_27.png)

Make sure you update the hosts file as indicated in the next step before you call Web IDE.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Map the host)]

Use the external IP address collected from the Amazon Web Services EC2 console to map it in your computer hosts file.

![Amazon Web Services Marketplace](_17.png)

> ### **Warning:** **You will need administration permissions on your computer to do this.**

  - On **Windows**:

	If your user is not an administrator but has administrator rights, right-click on Notepad and choose **Run as Administrator**.

	![Run As Administrator](_28.png)

	If you are an administrator user, proceed to the next point.

	Open the file or navigate with Windows explorer. You need to edit the file called **`hosts`** in the directory **`C:\Windows\System32\drivers\etc`**.

	![Hosts file](_29.png)


  - On **Mac**:

	Open a terminal ( **Finder -> Applications -> Terminal**) and type

	```shell
	sudo nano /etc/hosts
	```

    ![Hosts file](_30.png)

Add the external IP address followed by the name of the host, **`hxehost`** like this:

```
xxx.xxx.xxx.xxx hxehost
```

**Save** and close the text editor.

> Note: If Notepad asks you to create a new file, it means that you do not have enough permissions to save the file and that you didn't start your program as ***Administrator***.
>Creating a new file will not alter the configuration as intended.

You can now use your browser to call SAP Web IDE for SAP HANA using link **`https://hxehost:53075`**.

Log in with user `XSA_DEV` and the password you provided during the initial setup script.

![Web IDE](_31.png)

> ### **Note:** You may get a message stating that your connection is not private. This is generally expected because you are using a self-signed certificate.
&nbsp;
> You can use address `https://hxehost:39030` to see additional links such as the Administration Cockpit.
&nbsp;
>Make sure you use user `XSA_ADMIN` in different browsers or incognito mode to avoid authorization errors.

[DONE]
[ACCORDION-END]

If anything is going wrong, [check the questions and answers from the community](https://answers.sap.com/index.html).
