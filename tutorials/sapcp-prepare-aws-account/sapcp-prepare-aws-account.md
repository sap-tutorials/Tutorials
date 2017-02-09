---
title: Prepare an Amazon Web Services account for setting up ABAP on-premise system
description: Learn how to prepare AWS account on which ABAP on-premise system  image is installed next
tags: [  tutorial>beginner, topic>abap-development, topic>cloud, topic>java, products>sap-hana-cloud-platform, products>cloud-connector-for-sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Get your own free Developer Account for SAP HANA Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
How to prepare an Amazon Web Services account on which you will setup an ABAP on-premise system later on.

### Time to Complete
**10 Min**.

---

1. Navigate to <https://aws.amazon.com> and sign up for an AWS account if you don't have one already

2. Navigate into the **AWS management console**
    ![AWS Management Console](aws_management_console.png)

3. In the AWS console, enter the **Identity and Access Management section (IAM section)**
    ![Identity and Access Management section](iam_section.png)

4. Create an AWS user, e.g. called `sap_cal_user` and download the access key and secret key of this user. These credentials you need to provide later on in the SAP Cloud      Appliance Library in order to be able to access the AWS instance
    ![Create AWS user](create_aws_user.png)

5. Enter the user name
    ![AWS Username](enter_aws_username.png)

6. Download the credentials and click on 'Close'
    ![Download AWS Credentials](download_credentials.png)

7. Create a group, e.g. called `sap_cal_group`
    ![Create group](create_aws_group.png)

8. Select **Administrator Access as Policy Template** for the newly created group. This will assign administrator permissions to the group.
    ![Administrator Access Policy Step 1](admin_access_policy.png)
    ![Administrator Access Policy Step 2](admin_access_policy2.png)

9. Assign the newly created AWS user to this group.
    ![Assign AWS user to group step 1](assign_user_to_group.png)
    ![Assign AWS user to group step 2](assign_user_to_group2.png)
    ![Assign AWS user to group step 3](assign_user_to_group3.png)


## Next Steps
[Setup SAP Cloud Appliance Library account and install preconfigured SAP solution in cloud (coming soon)](/developer/tutorials/sapcp-prepare-cal-account.html)
