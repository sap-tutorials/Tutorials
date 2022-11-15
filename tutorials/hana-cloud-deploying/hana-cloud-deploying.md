---
parser: v2
time: 15
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
tags: [ tutorial>beginner, products>sap-hana-cloud]
primary_tag: products>sap-hana
---

# Deploy SAP HANA Cloud
<!-- description --> Create an instance of the SAP Hana Cloud in SAP BTP trial or free tier.

## Prerequisites
- You have an SAP Business Technology Platform free trial or free tier account: [Set Up an SAP BTP Account for Tutorials](group.btp-setup)

## You will learn
- How to use the SAP BTP cockpit as a graphical tool to provision your free SAP HANA Cloud instance
## Get to know SAP HANA Cloud
[SAP HANA Cloud](https://developers.sap.com/topics/hana.html) is a complete database and application development platform. It lets you use advanced data processing capabilities — text, spatial, predictive, and more — to pull insights from all types of data.

## Intro
By combining in-memory storage with columnar store, data operations are performed faster than in a traditional database with a disk-based storage. SAP HANA is also `translytical`, which means that developers can perform both transactional and analytical operations from the same structure, in real time, and without creating additional copies of the data such as materialized views.

---

### Add SAP HANA Cloud to your account


1. From the SAP BTP cockpit entry page, click on the subaccount where you will work.

    <!-- border -->![Sub Account](trial1.png)

2. In the Subaccount page, click on the space in which you want to work.

    <!-- border -->![dev Space](trial2.png)

3. Once in your space page, click on **SAP HANA Cloud** in the side menu navigation.  You will then see a page listing all your SAP HANA Cloud instances; of which you should have none.  Click on the **Create** button to begin the wizard to create a new instance.

    <!-- border -->![dev Space](trial3.png)


### Create Database


1. Complete the tutorial steps in [Provision an Instance of SAP HANA Cloud, SAP HANA Database](hana-cloud-mission-trial-2) This wizard used in this tutorial will walk you through the process of creating an SAP HANA Cloud instance. Just one note as you go through this guided tour: Make sure that in the "Advanced Settings" part of the setup, that you select "Allow all IP addresses" in the "Connections" setting. This setting will allow you to develop against your SAP HANA Cloud using a variety of external development tools.

    <!-- border -->![Allow All IP addresses](trial4.png)

2. After completing the previous step, you should now have a new SAP HANA Cloud instance created in the SAP BTP trial or free tier.

    <!-- border -->![HANA Cloud Instance](trial5.png)

3. Once the SAP HANA Cloud instance is created, take note of the admin user needed to connect to the database. This will be needed in subsequent steps in this tutorial.

4. Finally it is important to take note that the SAP HANA Cloud instance in both the free tier and free trial shuts down at the end of each day automatically to save costs from unused systems. Therefore you must return to this SAP HANA Cloud administration screen each day you want to use  SAP HANA Cloud and choose to start the system from the **Action** menu.  If you forget to restart the instance, you will receive HANA connection errors whenever you try to interact with it in later steps.

    <!-- border -->![HANA Cloud stopped](trial6.png)

5. Once the SAP HANA Cloud instance is created, take note of the admin user (DBADMIN) and password you supplied to connect to the database. This will be needed in subsequent tutorials.

6. As an optional step if you are completely new to the SAP HANA Cloud environment, you might want to consider also going through this tutorial: [Tools to Manage and Access the SAP HANA Cloud, SAP HANA Database](hana-cloud-mission-trial-3) in order to familiarize yourself with the various tools that can be used to manage and develop with SAP HANA Cloud.


---
