---
title: Prepare an Amazon Web Services account for setting up ABAP on-premise system
description: Learn how to prepare AWS account on which ABAP on-premise system  image is installed next
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>cloud, topic>java, products>sap-cloud-platform, products>cloud-connector-for-sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Get your own free Developer Account for SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
[Setup SAP Cloud Appliance Library account and install preconfigured SAP solution in cloud](https://www.sap.com/developer/tutorials/hcp-prepare-cal-account.html)

## Details
### You will learn  
How to prepare an Amazon Web Services account on which you will setup an ABAP on-premise system later on.

### Time to Complete
**10 Min**.

---
[ACCORDION-BEGIN [Step 1: ](Identity and Access Management)]

1. Navigate to <https://aws.amazon.com> and sign up for an AWS account if you don't have one already

2. Navigate into the **AWS management console**
    ![AWS Management Console](aws_management_console.png)

3. In the AWS console, enter the **Identity and Access Management section (IAM section)**
    ![Identity and Access Management section](iam_section.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new AWS user)]

1. Create an AWS user, e.g. called `sap_cal_user` and download the access key and secret key of this user. These credentials you need to provide later on in the SAP Cloud      Appliance Library in order to be able to access the AWS instance
    ![Create AWS user](create_aws_user.png)

2. Enter the user name
    ![AWS Username](enter_aws_username.png)

3. Download the credentials and click on 'Close'
    ![Download AWS Credentials](download_credentials.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create new AWS group)]

1. Create a group, e.g. called `sap_cal_group`
    ![Create group](create_aws_group.png)

2. Select **Administrator Access as Policy Template** for the newly created group. This will assign administrator permissions to the group.
    ![Administrator Access Policy Step 1](admin_access_policy.png)
    ![Administrator Access Policy Step 2](admin_access_policy2.png)

3. Assign the newly created AWS user to this group.
    ![Assign AWS user to group step 1](assign_user_to_group.png)
    ![Assign AWS user to group step 2](assign_user_to_group2.png)
    ![Assign AWS user to group step 3](assign_user_to_group3.png)


[ACCORDION-END]

## Next Steps
[Setup SAP Cloud Appliance Library account and install preconfigured SAP solution in cloud](https://www.sap.com/developer/tutorials/hcp-prepare-cal-account.html)
