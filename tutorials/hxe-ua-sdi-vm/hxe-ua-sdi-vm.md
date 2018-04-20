---
title: Install the Optional SAP HANA Smart Data Integration Package for SAP HANA, express edition
description: Install SAP HANA smart data integration on an SAP HANA, express edition system.
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio7621f586085b4a93898290e1571e560a -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  You have completed [Start SAP HANA, express edition Server](http://www.sap.com/developer/tutorials/hxe-ua-getting-started-vm.html)  
You must have SAP HANA, express edition installed prior to installing this package.


## Details
### You will learn
How to install and run the optional SAP HANA Smart Data Integration Package for SAP HANA, express edition.

### Time to Complete
30 min

---

You need to run a script and an installer executable to complete the necessary steps to install SAP HANA smart data integration. The script enables the Data Provisioning Server on SAP HANA, express edition, as well as deploys the data provisioning delivery unit that enables monitoring and other capabilities. The installer executable that you run installs the Data Provisioning Agent that provides connectivity between SAP HANA, express edition and your remote data sources.

> Note:
> The current version of SAP HANA, express edition supports only one Data Provisioning Agent per machine.
>
>

> Note:
> Use the server's built-in Download Manager (Console Mode) for Linux to download `sdi.tgz`. When logged-in as `hxeadm`, you can access the download manager (`HXEDownloadManager_linux.bin`) in directory `/usr/sap/HXE/home/bin`.
>
>

[ACCORDION-BEGIN [Step 1: ](Run the memory management script.)]

Run the `hxe_gc` memory management script to free up available VM memory

1.   In your VM, log in as `hxeadm` and enter:

    ```bash
    cd /usr/sap/HXE/home/bin
    ```

2.   Execute:

    ```bash
    hxe_gc.sh
    ```

3.   When prompted for System database user (SYSTEM) password, enter the New HANA database master password you specified during SAP HANA, express edition installation.

    The cleanup process runs. The command prompt returns when the cleanup process is finished.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download `sdi.tgz`:)]

In your VM, download `sdi.tgz` using the built-in Download Manager. From the same directory where you ran `hxe_gc` (`/usr/sap/HXE/home/bin`) enter:

```bash
HXEDownloadManager_linux.bin linuxx86_64 vm sdi.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Extract `sdi.tgz`.)]

In your VM, extract `sdi.tgz`.

```bash
tar -xvzf sdi.tgz
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run the installation script.)]

As the `hxeadm` user, run:

```bash
HANA_EXPRESS_20/install_sdi.sh
```

This enables the DP Server on HANA and deploys the monitoring delivery unit.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Install the Data Provisioning Agent.)]

To install the Data Provisioning Agent, run `extract directory``/HANA_EXPRESS_20/DATA_UNITS/HANA_DP_AGENT_20_LIN_X86_64/hdbinst.exe`.

You are prompted to supply the following information (default values are given):

-   Installation path `[/usr/sap/dataprovagent]`

-   Enter User name for Agent service (user must exist)

-   Enter Agent Listener Port [5050]

-   Enter Agent Administration Port [5051]

-   Enter Shared directory for Agent Group (optional)

-   Enter Custom JRE directory (to use bundled JRE, leave it blank)


The SAP JVM is bundled with the Data Provisioning Agent and used as the default Java Runtime Environment. You can choose to update the version of the SAP JVM used by an installed agent, or replace it with a custom Java Runtime Environment.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Authorize the agent user.)]

> Note:
> The default installation location (`/usr/sap/dataprovagent`) requires the agent user to have write access to the `/usr/` directory.
>
>

You must create or use an existing non-root agent user that is authorized to open a display and has full read and write access to the intended installation location.

Prior to installation, grant the agent user the appropriate permissions (use `sudo` to create the `/usr/sap/dataprovagent` directory and grant permissions to the user) or choose a different installation location.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Assign roles and privileges.)]

Add roles and privileges for users to perform various tasks. The following tables list common tasks and roles or privileges that an administrator needs to assign to complete those tasks.

**Data Provisioning Agent and Data Provisioning Adapter Tasks**

A user may need to be assigned specific roles and privileges to accomplish tasks when configuring the Data Provisioning Agent and Data Provisioning Adapters.

> Note:
> Permissions may also be required for accessing a particular database through a data provisioning adapter. See the *SAP HANA Smart Data Integration* documentation for complete information.
>
>

> Note:
> The information given below is for SAP HANA studio and SAP HANA Web-based Development Workbench only.
>
>

|Task|Role or Privilege|Role or Privilege Name|Notes|
|----|-----------------|----------------------|-----|
|Register a DP Agent|**System privilege**|`AGENT ADMIN`| |
|Register an adapter|**System privilege**|`ADAPTER ADMIN`| |
|Configure DP Agent to use HTTP (cloud) protocol|**Role**|`sap.hana.im.dp.proxy::AgentMessaging`|Whoever sets the DP Agent to use HTTP (cloud) in the DP Agent Configuration tool needs to be assigned this role.|
|Create an Agent or adapter when SAP HANA is in the cloud|**Application privilege**|`sap.hana.im.dp.admin::Administrator`|Needed when administrator want to create adapters/agent from agent `config` tool when SAP HANA is on the cloud (or Agent uses HTTP protocol).|

**Monitoring Tasks**

A user may need to be assigned specific roles or privileges to access and perform various tasks through the Data Provisioning monitors, which can be accessed from the SAP HANA cockpit.

|Task|Role or Privilege|Role or Privilege Name|Notes|
|----|-----------------|----------------------|-----|
|Monitoring|**Role**|`sap.hana.im.dp.monitor.roles::Monitoring`|**The Monitoring role includes the following application privileges:**|
| | | | `sap.hana.ide::LandingPage` |
| | | | `sap.hana.im.dp.monitor::Monitoring` |
| | **Application privilege** |`sap.hana.im.dp.monitor::Monitoring`| |
|Operations|**Role**|`sap.hana.im.dp.monitor.roles::Operations`|**The Operations role includes the following application privileges (`sap.hana.im.dp.monitor`):** |
| | | | `AddLocationToAdapter` |
| | | | `AlterAgent` |
| | | | `AlterRemoteSource` |
| | | | `AlterRemoteSubscription` |
| | | | `CreateAgent` |
| | | | `DeleteSchedule` |
| | | | `DropAgent` |
| | | | `ExecuteDesignTimeObject` |
| | | | `NotificationAdministration` |
| | | | `ProcessRemoteException` (This includes both remote source and remote subscription exceptions)|
| | | | `RemoveLocationFromAdapter` |
| | | | `ScheduleDesignTimeObjec` |
| | | | `ScheduleTask` |
| | | | `StartTask` |
| | | | `StopTask` |
| | | | `UpdateAdapter` |

**Remote Source and Remote Subscription Tasks**

A user may need to be assigned specific roles or privileges to create and manage remote sources.

|Task|Role or Privilege|Role or Privilege Name|Notes|
|----|-----------------|----------------------|-----|
|Create a remote source|**System privilege**|CREATE REMOTE SOURCE|If a user can create a remote source (has CREATE REMOTE SOURCE system privilege), that user automatically has CREATE VIRTUAL TABLE, DROP, CREATE REMOTE SUBSCRIPTIONS and PROCESS REMOTE SUBSCRIPTION EXCEPTION privileges; they don't need to be assigned to the user. However, this only applies to remote sources that the user creates themselves. If a remote source is created by someone else, then those privileges must be assigned, for each remote source, in order to perform those tasks.|
|Create a remote source in SAP HANA Web-based Development Workbench| **Role** |`sap.hana.xs.ide.roles::CatalogDeveloper`| |
|Alter a remote source|**Object privilege**|ALTER|In order to alter a remote source, the user must have the ALTER object privilege on the remote source. **Examples of altering a remote source include:** |
| | | | `ALTER REMOTE SOURCE <remote_source_name> SUSPEND CAPTURE` |
| | | | `ALTER REMOTE SOURCE <remote_source_name> RESUME CAPTURE` |
|Drop a remote source|**Object privilege**|DROP|Must be explicitly granted for a remote source created by another user.|
|Search for an object in a remote source|**Object privilege** |ALTER on the remote source to be searched.|In order to search for remote objects (such as tables) in a remote source, the user must have the `ALTER` object privilege on the remote source, so that a dictionary can be created.|
|Add a virtual table|**Object privilege**|CREATE VIRTUAL TABLE|Must be explicitly granted for a remote source created by another user.|

**Replication Task and `Flowgraph` Tasks**

A user may need to be assigned specific roles and privileges to create and run `flowgraphs` and replication tasks.

|Task|Role or Privilege|Role or Privilege Name|Notes|
|----|-----------------|----------------------|-----|
|Create a flowgraph| **Role** | `sap.hana.xs.ide.roles::EditorDeveloper` |Allows creation of `.hdbflowgraph`.|
| | **Object privilege** |EXECUTE| **EXECUTE privilege on:** |
| | | | `"_SYS_REPO"."TEXT_ACCESSOR"` and|
| | | | `"_SYS_REPO"."MULTI_TEXT_ACCESSOR"` |
|Create a flowgraph of type Task|**Object privilege:**|SELECT (for input/output schema)| |
|Create a replication task| **Role** | `sap.hana.xs.ide.roles::EditorDeveloper` |Allows creation of `.hdbreptask`.|
|Activate replication task (`.hdbreptask`)|**Object privileges:**|SELECT on the source schema|Must be granted to `_SYS_REPO`.|
| | |CREATE VIRTUAL TABLE on REMOTE SOURCE (Initial Load Only)|Must be granted to `_SYS_REPO`.|
| | |CREATE REMOTE SUBSCRIPTION on REMOTE SOURCE (for real time scenarios)|Must be granted to `_SYS_REPO`.|
|Activate flowgraph (`.hdbflowgraph`)|**Object privileges:**|SELECT on the source table|Must be granted to `_SYS_REPO`.|
| | |INSERT, UPDATE, and DELETE on the target table|Must be granted to `_SYS_REPO`.|
| | |SELECT on the target schema (only when using a Template Table as a target)|Must be granted to `_SYS_REPO`.|
| | |If sequence is used, then GRANT SELECT on sequence|Must be granted to `_SYS_REPO`.|
| | |History Table:|Must be granted to `_SYS_REPO`.|
| | |GRANT INSERT on History Table|Must be granted to `_SYS_REPO`.|
| | |GRANT SELECT on Target Table.|Must be granted to `_SYS_REPO`.|
|Execute a stored procedure|**Object privilege**|EXECUTE|Needed on the schema where the stored procedure is located.|
|Execute a task|**Object privileges:**|EXECUTE|Needed on the schema where the task is located.|
| | |INSERT|Needed on the schema where the task is located.|
| | |UPDATE|Needed on the schema where the task is located.|
| | |SELECT|Needed on the schema where the task is located.|
| | |DELETE|Needed on the schema where the task is located.|
|Use the JIT (just-in-time) Data Preview option|**Object privilege**|SELECT and EXECUTE with GRANT OPTION|Must be granted to `_SYS_REPO` user. Needed on the schema where the task or stored procedure is located.|

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Connect to SAP HANA.)]

Specify connection information and administrator credentials when the SAP HANA system is located on premise and does not require a secure SSL connection.

1.   Navigate to the `DPAgent_root``/configTool` directory.

2.   Start the configuration tool.

    -   On Windows, run `dpagentconfigtool.exe`.

    -   On Linux, run `./dpagentconfigtool`.


    > Note:
    > Start the configuration tool using the Data Provisioning Agent installation owner. The installation owner is the same user that is used to start the agent service.
    >
    >

3.   Connect to the SAP HANA server.

    -   Click *Connect to SAP HANA*.

    -   Specify the hostname, port, and SAP HANA administrator credentials for the SAP HANA server.


    > Note:
    > The administrator user that you use to connect to the SAP HANA system must have been granted the `AGENT ADMIN` and `ADAPTER ADMIN` system privileges. If the user that you want to use does not already have these privileges, you must grant them before you can connect to the SAP HANA system.
    >
    >

    > Note:
    > To determine the correct port number when SAP HANA is deployed in a multi-database configuration, execute the following SQL statement:
    >
    > ```bash
    > SELECT DATABASE_NAME,SERVICE_NAME,PORT,SQL_PORT,(PORT + 2) HTTP_PORT
    > FROM SYS_DATABASES.M_SERVICES WHERE DATABASE_NAME='<DBNAME>' and
    > ((SERVICE_NAME='indexserver' and COORDINATOR_TYPE= 'MASTER') or (SERVICE_NAME='xsengine'))
    > ```
    >
    >

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Register the agent.)]

Before you can use adapters deployed on the Data Provisioning Agent, you must register the agent with SAP HANA.

**Prerequisites:**

-   The Agent Admin HANA User must have the following roles or privileges.

    |Action|Role or Privilege|Role or Privilege Name|
    |------|-----------------|----------------------|
    |Register adapter|**System privilege** |`AGENT ADMIN`|

-   For SAP HANA on Cloud, the Agent XS HANA User must have the following roles or privileges.

    |Action|Role or Privilege|Role or Privilege Name|
    |------|-----------------|----------------------|
    |Register adapter|**System privilege** |`AGENT ADMIN`|
    |Messaging between the agent and SAP HANA on Cloud|**Application privilege** |`sap.hana.im.dp.proxy::AgentMessaging`|


1.   Start the agent configuration tool and connect to the SAP HANA server.

    The agent configuration tool is located at `DPAgent_root``/configTool`.

2.   Click *Register Agent*.

3.   Specify the agent connection information.

    -   If SAP HANA is not in the cloud, specify the agent name and hostname:

        -   Ensure that the SAP HANA server can communicate with the agent host. Depending on the network configuration, you may need to fully qualify the agent hostname.

        -   Ensure that your firewall settings allow the connection from the SAP HANA server to the agent host on the listener port. By default, port 5050.

    -   If SAP HANA is in the cloud, specify the agent name.

        -   When SAP HANA is in the cloud, the agent service will be restarted to complete the registration process.


4.   Click *Register*.

    The agent is registered with SAP HANA. If SAP HANA is in the cloud, the agent service is automatically restarted.

    To `unregister` the agent, click `Unregister` *Agent*.

    > Note:
    > `Unregistering` the agent from the SAP HANA server performs a cascade drop of the agent. As a result, any remote subscriptions that use the agent will also be deleted, even if they are active.
    >
    >

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Register adapters.)]

Before you can connect to remote sources using an adapter, you must register the adapter with SAP HANA.

**Prerequisites**:

The HANA administrator user must have the following roles or privileges.

|Action|Role or Privilege|Role or Privilege Name|
|------|-----------------|----------------------|
|Register an adapter| **System privilege** |`ADAPTER ADMIN`|
| | **Application privilege** |`sap.hana.im.dp.admin::Administrator`|

> Note:
> This application privilege is required only for SAP HANA in the cloud.
>
>

> Note:
> Before you register the adapter with the SAP HANA system, be sure that you have downloaded and installed the correct JDBC libraries (if necessary). For information about the proper JDBC library for your source, see the *Product Availability Matrix (PAM)*.
>
> Place your JDBC library in `DPAgent_root``/lib`, and you may need to manually create the `/lib` folder.
>
>

1.   Start the Data Provisioning Agent Configuration tool and connect to SAP HANA.

2.   For custom adapters, click *Deploy Adapter* and point to the adapter JAR files.

    > Note:
    > SAP-delivered data provisioning adapters are automatically deployed on the agent during agent installation.
    >
    >

3.   Select the adapter to register and click *Register Adapter*.

4.   If required, configure the source system to which the adapter connects.

    For example, log reader adapters require source configuration to enable real time replication.

The selected adapter is registered with SAP HANA and can be selected when creating a remote source

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Next steps.)]

After installation is complete, you will need to complete some other tasks to access and move data.

When you have completed the installation and connected to HANA, you will want to begin creating remote sources, and replicating or transforming your data.

For complete information about SAP HANA smart data integration, see the SAP Help Portal.

[ACCORDION-END]
