---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>beginner, products>sap-business-technology-platform]
primary_tag: products>sap-business-technology-platform
author_name: Kavya M Gowda
author_profile: https://github.com/KavyaGowda-UA
---


# Set Up a Sample Lead-to-Cash Business Process
<!-- description --> Set up a pre-configured Lead-to-Cash business process to experience the process visibility capability.

## Prerequisites
Set up **SAP Workflow Management** service. For more information, see the [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup) tutorial.

## You will learn
- How to import a sample Lead-to-Cash visibility scenario
- How to import the events for sample Lead-to-Cash business process
- How to gain visibility on the Lead-to-Cash business process
- How to quickly get started with the process visibility capability using a sample Lead-to-Cash business process

## Intro
Lead-to-Cash is a business process that provides sales insights, eliminates process inefficiencies, and ensures accurate reporting on business performance and future trends.

In this tutorial, you can learn how to gain visibility on a sample Lead-to-Cash business process using the process visibility capability. You are provided with a JSON file containing events and a zip file that contain the visibility scenario using which you can easily try out the capabilities of process visibility.

### Download files from GitHub


1. Use the following link to access [GitHub](https://github.com/SAP-samples/cloud-process-visibility/releases).

2. Choose the `LeadToCashEvents.json` file.

    ![GitHub](LeadToCashEvents-json.png)

    >This downloads the `LeadToCashEvents.json` file to your local system.

3. Now, from the GitHub, choose the `LeadToCash.zip` file.

    ![GitHub2](LeadToCash-zip.png)

    >This downloads the `LeadToCash.zip` file to your local system.

    You now have the two downloaded files in your local system with events and visibility scenario relevant to Lead-to-Cash business process.


### Import events

In this step, you will import the `LeadToCashEvents.json` file to the **Event Acquisition** application.

1. Open the **Event Acquisition** tile from the **Workflow Management** dashboard.

    <!-- border -->![Event Aquisition App](event-flp.png)

2. Choose the **Import** icon.

    <!-- border -->![Import event](Import.png)

3. In the **Import From File** dialog, browse for the `LeadToCashEvents.json` file present in your local file system.

    <!-- border -->![Import event](Browse.png)

4. Choose **Import**.  

    <!-- border -->![Import from tile](Browse_Import.png)

5. Check if the events are imported successfully, by choosing the **View Filter Settings** icon.

    <!-- border -->![View Filter](Filter.png)

6. Choose the **Process Definition ID** option to filter the events. In this tutorial, for the Lead-to-Cash business process, provide the value as **`L2C`** and choose **OK**.

    <!-- border -->![Process Definition ID](L2C.png)

    You can view the list of events imported for the **`L2C`** process definition ID.

    <!-- border -->![Process Definition ID list](L2C_List.png)



### Import the visibility scenario


In this step, you will import the `LeadtoCash.zip` file to the **Configure Visibility Scenarios** application.

1. Open the **Configure Visibility Scenarios** tile from the **Workflow Management** dashboard.

    <!-- border -->![config scenario](config-flp.png)

2. Choose the **Import Scenario** icon.

    <!-- border -->![import scenario](CVS-Import.png)

3. In the **Import Scenario** dialog, browse for the `LeadtoCash.zip` file present in your local system and choose **Import**.

    <!-- border -->![browse](L2C-zipfile-import.png)

    You can now see the visibility scenario imported for a Lead-to-Cash business process as shown.

    <!-- border -->![imported scenario](CVS-L2C.png)

4. Choose the **Lead to Cash** visibility scenario from the list and choose **Activate**.

    <!-- border -->![Activate](L2C-Activate.png)

    >You will see a message upon successful activation of the scenario.


### Process the data


In this step, you can process the data of the activated visibility scenarios using the **Monitor Visibility Scenarios** application.

1. Choose the **Monitor Visibility Scenarios** tile from the **Workflow Management** dashboard.

    <!-- border -->![monitor scenarios](MVS.png)

2. Choose the **Lead to Cash** scenario definition, then choose **Process Data**.

    <!-- border -->![Process Data](LTC_MonitorScenarios.png)

3. On successful processing of data, choose the refresh icon to view the processing information listed out under **Processing Information**.

    <!-- border -->![Refresh](ProcessInformation.png)


### Track the visibility scenario


In this step, you will be able to track the progress of the visibility scenario using the visual representation in the **Process Workspace** application.

1. Choose the **Process Workspace** tile from the **Workflow Management** dashboard.

    <!-- border -->![process workspace](PW.png)

2. Choose the **Lead to Cash** visibility scenario.

    <!-- border -->![L2C](PW_Lead-to-Cash.png)

    You can view the overview page for the **Lead to Cash** visibility scenario as shown below. You can view information such as open orders, cycle time, open orders by product category, and so on.

    <!-- border -->![L2C overview](Overview.png)

    >You can customize the cards displayed on the overview page by modifying the visibility scenario in the **Configure Visibility Scenarios** application. For more information on creating and enhancing a scenario, see [Create a Scenario](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/df284fd12073454392c5db8913f82d81.html) documentation.




---
