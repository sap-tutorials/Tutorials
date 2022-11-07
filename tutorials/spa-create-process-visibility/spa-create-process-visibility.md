---
parser: v2
author_name: Stephan Schluchter
author_profile: https://github.com/SchluchterStephan
auto_validation: true
time: 15
tags: [tutorial>intermediate, software-product>sap-business-technology-platform, tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---
# Create Process Visibility Scenario
<!-- description --> Create a Visibility Scenario for your Business Process

## Prerequisites
 - [Subscribe to SAP Process Automation Using Booster in SAP BTP Free Tier](spa-subscribe-booster)
 - [Build Your First Business Process with SAP Process Automation](mission.sap-process-automation) Completed
 - [Get Order Details from Excel File with SPA](spa-create-automation) Completed

## You will learn
  - How to create and configure a Process Visibility Scenario
  - How to define performance indicators of a process by using visibility scenarios
  - Why and how you can gain visibility into your processes to measure performance with SAP Process Automation.

---
The process is created where several forms are defined, an automation to extract data is included in that same process and a condition to control the flow and even improved the process with some decision logic are used. Now it's time to measure success and improvements. In fact, you will gain end-to-end process visibility. There is a comprehensive dashboard with several process performance indicators, like process cycle time, duration of individual steps or status of the running processes – and this view is created automatically, in real time.

### Prepare Process

Before creating a **Visibility Scenario** we need to prepare the process.

1.  In the **Process Builder** open the process and on the right-hand side select the tab **Visibility**.

    <!-- border -->![Find Visibility tab](Step1-1.png)

    > Here you define the connection between the process context and the newly to be created visibility scenario.

2.  Click ![Plus Sign](icon-plus-sign.png) on the right panel to add the relevant attributes from the process.

    <!-- border -->![Plus Sign to add attributes](Step1-2.png)

3.  Add the following attributes:

    - `expectedDeliveryDate`
    - `orderAmount`
    - `orderDate`
    - `orderNumber`
    - `shippingCountry`
    - `Customer Name`

    <!-- border -->![Added Attributes](Step1-3.png)

    > The relevant attributes are the ones that could be used to measure, calculate, or derive some meaningful information in the visibility dashboard.

4.  Click **Save**.

    <!-- border -->![Save](Step1-4.png)



### Create Visibility Scenario

1.  Create a new **Visibility Scenario**.

    <!-- border -->![New visibility scenario](Step2-1.png)

2.  Set **Name** to **Sales Order Visibility Scenario** then click **Create**.

    <!-- border -->![Details visibility scenario](Step2-2.png)

    > Please ensure you use a **unique name**, to make it easier to identify your scenario later on.

The **Visibility Scenario** is created.

3.  Move to **General** tab in **Sales Order Visibility Scenario** and change: **Instances** to **Sales Orders** and **Instance** to **Sales Order**.

    > To make it better understandable what the instances are processing: Sales Orders.

    <!-- border -->![Change Instance(s) label](Step2-3.png)



### Configure Visibility Scenario Process

This step involves adding a process to the visibility scenario we created.

1.  Click ![Plus Sign](icon-plus-sign.png) **> Add Process** in **Process** tab in **Sales Order Visibility Scenario**.

    <!-- border -->![Add Process to visibility scenario](Step3-1.png)

2.  Select **Order Processing** process.

    <!-- border -->![Select Process](Step3-2.png)

    > This will lead to the visibility scenario configuration screen, where these can be viewed and controlled:
    > 1. All the events that could potentially occur during the process execution
    > 2. The context data which is the attributes selected before (Step **1.3**)

3.  Change within the following context data the data types. Click ![Pencil Icon](icon-pencil.png):

    <!-- border -->![Change data types in context data](Step3-3.png)

    | Name                   | Current Data Type | New Data Type |
    | ---------------------- | ----------------- | ------------- |
    | `expectedDeliveryDate` | String            | Date          |
    | `orderAmount`          | String            | Double        |
    | `orderDate`            | String            | Date          |

    <!-- border -->![Details to change data type of expected delivery date](Step3-4.png)


### Define Visibility Scenario Status

This step involves configuring the visibility scenario's status that allows to define under which circumstance a process instance will change the status to require special processing or actions.

1.  Move to **Status** tab in **Sales Order Visibility Scenario**.

2.  Change **Target Type** from **None** to **Constant**.

    <!-- border -->![Change Target Type to constant](Step4-1.png)

3.  Set **Target Value** to **10 Min** and **Threshold** to **50%**.

    <!-- border -->![Change Target Value](Step4-2.png)

    > The means: as soon as processing duration crosses 5 minutes (50% of **Target Value**), the status will switch to "At Risk". After 10 minutes it will change to "Critical".


### Configure Visibility Scenario Performance Indicators

1.  Move to **Performance Indicators** tab in **Sales Order Visibility Scenario** and add additional ones or leave them as is.

    <!-- border -->![Performance Indicators](Step5-1.png)

2.  Click **Save**.

You have now configured a visibility scenario which is related to your process. Based on this configuration a dashboard will be created which you can use to measure the performance, but to also get deep insights into single instances and even trigger actions.

In the next tutorial, you will run the process and will be able to monitor the **Process Visibility Dashboard**.


---
