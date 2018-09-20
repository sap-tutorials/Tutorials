---
title: Check your environment (Forecast App)
description: Check your environment before starting the Forecast tutorial series for SAP HANA, express edition
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 20
---

## Next Steps
 - [Use Machine Learning to Build a Forecasting application using the XS advanced development model](https://www.sap.com/developer/tutorials.html?/groups/hxe-aa-forecast.html)

## Details
### You will learn
- Which flavor and version of SAP HANA, express edition is needed to complete this tutorial series
- Complete a series of required post-installation task
- Enable the builders for the development space
- Add a connection in the Database Explorer
- Enable the Script Server to allow the execution of `AFL`s functions

[ACCORDION-BEGIN [Step 1: ](Which SAP HANA flavor and version?)]

In order to complete this tutorial series, you need to use as a minimum version:

 - **SAP HANA, express edition 2.0 SPS03**

This version will allow you to complete all the steps described in the series. And thanks to the availability of the SAP HANA Automated Predictive Library (APL), you will be able to compare different predictive libraries.

As you may already know, SAP HANA, express edition comes in two different flavors. In this series, you will be leveraging the SAP Web IDE to complete both the execution of SQL and SAP HANA XSA development tasks. Therefore, you will need:

 - **Server + XSA Applications**

You can check the [SAP HANA, express edition installation flowchart](https://www.sap.com/developer/topics/sap-hana-express.html#flowchart) to find all the installation details.

If you don't have an instance up and running, be aware that you don't need to complete the installation of all optional packages (this will be described when needed).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Setup the XS CLI API endpoint and login)]

In order to complete the next steps, you will be using the XS CLI client which is locally installed with your SAP HANA 2.0, express edition instance.

Therefore, you can run these commands directly from the server using a SSH client like `PuTTY` for example.

If you running the commands remotely using `PuTTY`, make sure to switch to the `hxeadm` user:

```shell
sudo su - hxeadm
```

> ### **Note:** You may prefer to run XS CLI commands remotely (from your local desktop for example). To do so, you can complete the [XS CLI Client installation](https://www.sap.com/developer/tutorials/hxe-ua-install-xs-xli-client.html)

Execute the following series of XS CLI commands.

#### Set the API endpoint:

```shell
xs api https://hxehost:39030
```

> ### **Note:** if you receive the following error when executing the previous command:
>&nbsp;
```
FAILED: SSL connection error (supposedly untrusted connection, check the certificates)
```
>As describe in the [XS CLI Client installation](https://www.sap.com/developer/tutorials/hxe-ua-install-xs-xli-client.html) tutorial, you will need to use the SSL certificate with the **`cacert`** command switch, and issue a command like this instead:
>&nbsp;
```shell
xs api https://hxehost:39030 -cacert <path>/default.root.crt.pem
```
Where the default path for the certificate on the server is:
>
 - `/hana/shared/HXE/xs/controller_data/controller/ssl-pub/router`
>

#### Login to your organization and space:

```shell
xs login -o HANAExpress -s SAP -u XSA_ADMIN
```

> ### **Note:**
>You will be prompted for the ***`XSA_ADMIN`*** password (which is initially set to the master password during the first boot initialization).
>&nbsp;
>You can get the list of organizations using the following command:
&nbsp;
```shell
xs orgs
```
> and the list of spaces using:
&nbsp;
```shell
xs spaces
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Rescale processes)]

If your environment is limited in term of memory resources and in order to ensure a smooth experience, you can execute the following commands to stop certain services, rescale the memory used by some processes and run the memory collector.

#### Stop services:

The following services can be stopped as you won't leverage them in this tutorial series.

```shell
xs stop sap-portal-static-resources
xs stop cockpit-telemetry-svc
```

#### Stop services:

The following services can be scaled up to ensure a better user experience during this tutorial series.

```shell
xs scale di-runner -m 512M -f -w
xs scale di-core -m 512M -f -w
```

#### Run the memory collector script:

The following script can be executed at any time to collected back unused process memory.

```shell
/usr/sap/HXE/home/bin/hxe_gc.sh
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Verify the builders deployment)]

With some installations, the builders are not deployed in the ***development*** space which will prevent you from successfully building and running your projects in the SAP Web IDE.

Before deploying the builder, you will need to start the `di-space-enablement-ui` application which might be stopped be default.

#### Get the current status:

```shell
xs app di-space-enablement-ui
```

The output should look like this:

```
Showing status and information about "di-space-enablement-ui"
  name:             di-space-enablement-ui
  requested state:  STOPPED
  instances:        1
  memory:           16.0 MB
  disk:             <unlimited>
  buildpack:        ***************
  urls:             https://hxehost:510XX
```

#### Start the app:

Run the following command if the ***requested state*** is ***STOPPED***.

```shell
xs start di-space-enablement-ui
```

The output should look like this:

```
Starting app "di-space-enablement-ui"...
 Starting instances with the default OS user
 1 of 1 instances running

Showing status and information about "di-space-enablement-ui"
  name:             di-space-enablement-ui
  requested state:  STARTED
  instances:        1
  memory:           16.0 MB
  disk:             <unlimited>
  buildpack:        ***************
  urls:             https://hxehost:510XX

Instances of droplet 1 created at Apr 4, 2018 5:36:52 PM
index  created                 state    os user
----------------------------------------------------
0      Jun 4, 2018 1:35:36 PM  RUNNING  <not set>
```

#### Check the builder status:

You can now access the application ***`urls`*** displayed in the previous command output.

From the previous output, the URL is: `https://hxehost:510XX` (make sure to adjust the URL based on your current output).

Login using the **`XSA_ADMIN`** credentials.

![Login](02-01.png)

Verify that the status for the ***development*** space is **Enabled**.

If not click, on **Enable**.

![builders](02-02.png)

#### Stop the app:

In order to release the resources used by the `di-space-enablement-ui` application, you should stop if this one was stopped initially.

```shell
xs stop di-space-enablement-ui
```

Based on the outputs returned previously, provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access the SAP HANA XS Advanced Cockpit)]

From the XSA Controller page, access the **SAP HANA XS Advanced Cockpit**.

As a reminder the default URL for the SAP HANA XS Advanced Cockpit is:

 - `https://hxehost:39030`

![XSA Controller](04-01.png)

Login using the **`XSA_ADMIN`** credentials.

![Login](04-02.png)

Once logged in, you will get access to the SAP HANA XS Advanced Cockpit:

![SAP HANA XS Advanced Cockpit](04-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Update the XSA tenant configuration)]

By default, the *SYSTEMDB* will be used when deploying or running your XSA application using HDI containers.

However, in order to leverage the SAP HANA AFL libraries, you will need to enable the *Script Server*, which is not possible on the *SYSTEMDB*.

Therefore, you will need to map the HXE tenant to your development space.

On the left side bar, click on **Tenant Databases**.

![SAP HANA XS Advanced Cockpit](05-01.png)

As you can notice, by default, the HXE tenant is not enabled for XSA and is not mapped for the development space.

![SAP HANA XS Advanced Cockpit](05-02.png)

Click on the **Enable** icon ![SAP HANA XS Advanced Cockpit](00-xsa-tenant-enable.png) for the **HXE** tenant.

Provide the SYSTEM user credentials for the HXE tenant ( ***Tenant Database*** ) and the SYSTEMDB ( ***Physical Database*** ).

![SAP HANA XS Advanced Cockpit](05-03.png)

Wait for the activation process to complete.

Click on the **Map** icon ![SAP HANA XS Advanced Cockpit](00-xsa-tenant-map.png) for the **HXE** tenant.

Set the following details:

Property     | Value
-------------|---------------
Organization | `HANAExpress`
Space        | `development`

![SAP HANA XS Advanced Cockpit](05-04.png)

Click on **Save**.

Once completed, the configuration should look like this:

![SAP HANA XS Advanced Cockpit](05-05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Access the Web IDE)]

From the XSA Controller page, access the ***Web IDE***.

![XSA Controller](06-01.png)

Login using the **`XSA_DEV`** credentials.

![Login](06-02.png)

Once logged in, you will get access to the Web IDE:

![Web IDE](06-03.png)

As a reminder the default URL for the Web IDE is:

 - `https://hxehost:53075`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Add the database connection)]

On the left side bar, click on the Database Explorer icon ![Web IDE](00-dbexplorer-icon.png) icon.

![Web IDE](00-dbexplorer.png)

#### Add the SYSTEMDB connection

If you don't have any database listed you will receive the following popup message:

![Web IDE](00-dbexplorer-firsttime.png)

Click on **Yes**.

If not, then use the ***Add a database to the Database Explorer*** icon ![Web IDE](00-dbexplorer-plus.png).

Select **SAP HANA Database (Multitenant)** as ***Database Type***.

Enter the HXE host name and instance number (default value is 90).

Select **System Database**.

Enter the **SYSTEM** user name and password (which is initially set to the master password during the first boot initialization).

You can also check the ***Save user and password*** to avoid entering credentials in the future.

![Web IDE](07-01.png)

Click on **OK**.

![Web IDE](07-02.png)

#### Add the HXE tenant connection

Use the ***Add a database to the Database Explorer*** icon ![Web IDE](00-dbexplorer-plus.png).

Select **SAP HANA Database (Multitenant)** as ***Database Type***.

Enter the HXE host name and instance number (default value is 90).

Select **Tenant Database** and enter **HXE** as name.

Enter the **SYSTEM** user name and password (which is initially set to the master password during the first boot initialization).

You can also check the ***Save user and password*** to avoid entering credentials in the future.

Click on **OK**.

![Web IDE](07-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the Script Server)]

The **Script Server** is an auxiliary service that is required to execute **Application Function Libraries** ( **AFL** ).

For example, this applies to the SAP HANA AFL component like the ***SAP HANA Predictive Analysis Library*** (PAL) and other similar libraries.

By default, the Script Server is not activated on the **HXE** tenant.

Select the **HXE** tenant connection in the ***Database Explorer*** panel, then click on the **Open SQL Console** icon ![Database Explorer](00-dbexplorer-sql.png) (or press ***CTRL+ALT+C***).

Paste the following SQL statement in the console and click on the ***Run*** icon ![Database Explorer](00-dbexplorer-run.png) icon:

```sql
SELECT SERVICE_NAME, PORT, ACTIVE_STATUS FROM SYS.M_SERVICES ORDER BY 1;
```

If no rows is returned for the `scriptserver`, then it means that the **Script Server** is not enabled on the **HXE** tenant.

To enable it, you will have to run the following SQL statement on the **SYSTEMDB**.

Select the **SYSTEMDB** connection in the ***Database Explorer*** panel, then click on the **Open SQL Console** icon ![Database Explorer](00-dbexplorer-sql.png) (or press ***CTRL+ALT+C***).

Paste the following SQL statement in the console and click on the ***Run*** icon ![Database Explorer](00-dbexplorer-run.png) icon:

```sql
ALTER DATABASE HXE ADD 'scriptserver';
```

Select the **HXE** tenant connection in the ***Database Explorer*** panel, then click on the **Open SQL Console** icon ![Database Explorer](00-dbexplorer-sql.png) (or press ***CTRL+ALT+C***).

Now, you can now verify that the service is started using the following SQL statement:

```sql
SELECT SERVICE_NAME, PORT, ACTIVE_STATUS FROM SYS.M_SERVICES ORDER BY 1;
```

The result should return a list of service names, their associated port numbers and their statuses including an entry for the `scriptserver`.

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]
