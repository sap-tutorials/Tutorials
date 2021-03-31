---
title: Enhance Your Visibility Scenario for a Deployed Workflow
description: Enhance your scenario by adding status, phases, performance indicators, and attributes for the workflow to track and analyze business process.
auto_validation: true
time: 15
tags: [tutorial>beginner, products>sap-business-technology-platform]
primary_tag: products>sap-business-technology-platform
author_name: Kavya M Gowda
author_profile: https://github.com/I540620
---

## Prerequisites
- Setup the Workflow Management service. For more information, see the [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup) tutorial.

## Details
### You will learn
-	How to enhance your scenario by adding phases, attributes, and performance indicators
-	How to activate the scenario

By default, a set of performance indicators are generated when you add a Workflow from SAP Workflow service as a participant. You can view the default status, attributes, actions, and performance indicators using various tabs of the Configure Visibility Scenarios application. You can choose to activate the scenario without enhancing it. Alternatively, you can further enhance the scenario by adding phases, status, calculated attributes, and performance indicators.

[ACCORDION-BEGIN [Step 1: ](Create phases)]
1. Navigate to the **Phases** tab and choose the **+** icon.

    !![Add phase](Config-Step2-addphase1.png)

2. Provide the following details to add the phase and click **OK**.

    !![Phase info](Config-Step2-addphase1name.png)

3. In the **Start Events** drop down, select **StartEvent1 Started** and **Change or Confirm equipment Created**.

    !![Phase start](Config-Step2-addphase1details.png)

4. In the **End Events** drop down, select **Change or Confirm equipment Completed**.

    !![Phase end](Config-Step2-addphase1details2.png)

5. Similarly, create another phase named **Approve Equipment**. Then, select the **Approve Equipment Created** option from the **Start Events** dropdown and **Approve Equipment Completed** option from the **End Events** dropdown. Then save the changes.

    !![Add another phase](Config-Step2-addphase2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Configure the target)]
1. Navigate to the **Status** tab.

    !![Status](Config-Step3-status.png)

2. Under the **Target** section, provide the following details and save the changes:

    !![Configure Target](Config-Step3-target.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add visibility scenario attributes)]
1. Choose the **Attributes** tab, where you can view a set of default attributes.

    !![Attribute](Config-Step3-attributes.png)

2. Add a calculated attribute by choosing the **+** icon.

    !![Plus attribute](Config-Step3-addattributes.png)

    Add the following properties in the **Add Calculated Attribute** dialog.

    !![Add attribute](Config-Step3-attributes2.png)

3. **Save** the changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure performance indicators)]
1. Navigate to the **Performance Indicators** tab and choose the **+** icon to add a performance indicator.

    !![Performance indicators](config-Step4-addPF.png)

    In the **Add Performance Indicator** dialog, provide the following details and choose **OK**:

    !![Add performance indicators](config-Step4-addPFname.png)

2. Choose the following options as shown in the **General** and **Data** section of the **Performance Indicators** tab:

    !![PF details](config-Step4-addPFdetails.png)

4. Choose the **+** icon under the **Filters** section of **Performance Indicators** to add a filter.

    !![Add filter](config-Step4-addPFfilter.png)

    In the **Add Filter** dialog, provide the following details:

    !![Filter details](config-Step4-addPFfilter2.png)

6. **Save** the changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Activate the scenario)]
1. Save the scenario and then choose **Activate**.

    !![Activate](config-Step5-activate.png)

2. Once the scenario is successfully activated, navigate back to the **Configure Visibility Scenarios** tile to view status of the scenario is now in **Active**.

    !![After activate](config-Step5-active.png)

[VALIDATE_1]
[ACCORDION-END]

---
