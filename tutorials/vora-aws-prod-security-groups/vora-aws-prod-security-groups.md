---
title: Create a Security Group in AWS for SAP Vora AWS Marketplace Production Edition
description: This is a step by step guide to set up security groups in your AWS virtual private cloud for SAP Vora 1.3.
primary_tag: products>sap-vora
tags: [  tutorial>how-to, tutorial>beginner, products>sap-vora ]
---
## Prerequisites  
 - AWS account
 - AWS Virtual Private Cloud (VPC)

## Next Steps
 - View other [SAP Vora How-Tos](https://www.sap.com/developer/tutorial-navigator.tutorials.html?tag=products:data-management/sap-vora) or visit the [SAP Vora Developer](https://www.sap.com/developer/topics/vora.html) Page.


## How-To Details
This How-to is a step by step guide for setting up security groups in your AWS virtual private cloud for SAP Vora 1.3 Marketplace Production Edition. This is one-time security group set-up can be used by SAP Vora Cluster instances.

### Time to Complete
**20 min**.

---

[ACCORDION-BEGIN [Step 1: ](Create security group)]

Log in to the AWS console and select **Security Groups** from `EC2` services.

![Log into AWS](security-picture1.png)

Click on '**Create security group**'. Enter name, description; choose the VPC created for SAP Vora.

![Create security group](security-picture2.png)

While creating security group add the TCP Ports as shown below and click **Create**.

![Add ports](security-picture3.png)



[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Edit security group)]

Select the newly created security group, then click on the **Inbound** tab.

![Edit inbound settings](security-picture4.png)

Add the same security group as the source for **All Traffic**. This allows all the traffic within the security group (all instances having this security group can communicate on all the ports)

Now the security group is all set and can be reused while creating a Vora Cluster. When launching SAP Vora Manager AMI use this security group.

![Select security group](security-picture5.png)

![Select security group](security-picture6.png)


As long as clusters are created in the same VPC, the same security group can be used. If you want to create another cluster or clusters in other another VPC, new security groups must be created.


[ACCORDION-END]
---


## Next Steps
 - View other [SAP Vora How-Tos](https://www.sap.com/developer/tutorial-navigator.tutorials.html?tag=products:data-management/sap-vora) or visit the [SAP Vora Developer](https://www.sap.com/developer/topics/vora.html) Page.
