---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>beginner, products>sap-business-technology-platform]
primary_tag: products>sap-business-technology-platform
author_name: Kavya M Gowda
author_profile: https://github.com/KavyaGowda-UA
---

# Set Up a Sample Design-to-Operate Business Process
<!-- description --> Set up a pre-configured Design-to-Operate business process to experience the Process Visibility capability.

## Prerequisites
 you have set up **SAP Workflow Management** service. For more information, see the [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup) tutorial.

## You will learn
  - How to import a Design-to-Operate visibility scenario
  - How to import the events for sample Design-to-Operate business process
  - How to gain visibility on sample Design-to-Operate business process
  - How to quickly get started with the process visibility capability using the sample Design-to-Operate business process


## Intro
Design-to-Operate is a supply chain centric business process that enables companies to connect digitally to perfect reality across their product and asset lifecycle. This end-to-end business process consists of stages from design, planning, manufacturing, logistics, and operations.

In this tutorial, you can learn how to gain visibility on a sample Design-to-Operate business process using the process visibility capability. You are provided with a JSON file containing events and a zip file that contain the visibility scenario for Design-To-Operate business process using which you can easily try out the capabilities of process visibility.

### Download files from GitHub

In this step, you will download the sample Design-to-Operate visibility scenario and the respective events from GitHub to your local file system.

1. Use the following link to access [GitHub](https://github.com/SAP-samples/cloud-process-visibility/releases).

2. Choose the `DesignToOperateEvents.json` file.

    ![GitHub](screen1-git.png)

    >This downloads the `DesignToOperateEvents.json` file to your local system.

3. Now, from the GitHub, choose the `DesigntoOperate.zip` file.

    ![GitHub2](designzip.png)

    >This downloads the `DesigntoOperate.zip` file to your local system.

    You now have the two downloaded files in your local system with events and a scenario relevant to Design-to-Operate business process.


### Import events


In this step, you will import the `DesignToOperateEvents.json` to the **Event Acquisition** application.

1. Open the **Event Acquisition** tile from the **Workflow Management** dashboard.

    ![Event Acquisition App](event-flp.png)

2. Choose the **Import** icon.

    ![Import event](Import.png)

3. In the **Import From File** dialog, browse for the `DesignToOperateEvents.json` file present in your local file system.

    ![Import event](Browse.png)

4. Choose **Import**.  

    ![Import from tile](D2O-Import.png)

5. Check if the events are imported successfully, by choosing the **View Filter Settings** icon.

    ![View Filter](Filter.png)

6. Choose the **Process Definition ID** option. In this tutorial, for the Design-To-Operate business process, provide the value as **`DESIGN`** and choose **OK**.

    ![Process Definition ID](D2O-Ok.png)

    You can view a list of events imported for this process definition ID.

    ![Process Definition ID](D2O-Eventlist.png)

    >Similarly, you can search for other process definition IDs such as **`DELIVER`** and **`MANUFACTURE`**. You will be able to view the list of events imported for the respective process definition IDs.


### Import the visibility scenario


In this step, you will import the `DesigntoOperate.zip` file to the **Configure Visibility Scenarios** application.

1. Open the **Configure Visibility Scenarios** tile from the **Workflow Management** dashboard.

    <!-- border -->![config scenario](config-flp.png)

2. Choose the **Import Scenario** icon.

    <!-- border -->![import scenario](CVS-Import.png)

3. In the **Import Scenario** dialog, browse for the `DesigntoOperate.zip` file from your local system and choose **Import**.

    <!-- border -->![Browse screen](D2O-zipfile-import.png)

    You can now see the imported visibility scenario as shown.

    <!-- border -->![imported scenario](screen9-d2o.png)

4. Choose the **Design to Operate** visibility scenario from the list and choose **Activate**.

    <!-- border -->![Activate](screen9-activate.png)

    >You will see a message upon successful activation of the scenario.


### Process the data


In this step, you can process the data of the activated visibility scenario using the **Monitor Visibility Scenarios** application.

1. Open the **Monitor Visibility Scenarios** tile from the **Workflow Management** dashboard.

    <!-- border -->![monitor scenarios](MVS.png)

2. Choose the **Design to Operate** scenario definition, then choose **Process Data**.

    <!-- border -->![Process Data](screen10-monitor.png)

3. On successful processing of data, choose the refresh icon to view the processing information listed out under **Processing Information**.

    <!-- border -->![Refresh](screen10-refresh.png)


### Track the visibility scenario


In this step, you will be able to track the progress of the sample Design-To-Operate visibility scenario using the visual representation in the **Process Workspace** application.

1. Open the **Process Workspace** tile from the **Workflow Management** dashboard.

    <!-- border -->![process workspace](PW.png)

2. Choose the **Design to Operate** visibility scenario.

    <!-- border -->![D2O](screen11-processworkspace.png)

    You can view the overview page for the **Design to Operate** visibility scenario as shown below. You can view information such as open orders, cycle time, open orders by product category, and so on.

    <!-- border -->![D2O](screen11-overviewpws.png)

    >You can customize the cards displayed on the overview page by modifying the visibility scenario in the **Configure Visibility Scenarios** application. For more information on creating and enhancing a scenario, see [Create a Scenario](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/df284fd12073454392c5db8913f82d81.html) documentation.


---
