---
auto_validation: true
title: Create Communication System to Connect to SAP BTP, ABAP Environment.
description: Create a communication system on S/4HANA Cloud to connect to SAP BTP, ABAP Environment.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>intermediate, topic>abap-development, products>sap-business-technology-platform,tutorial>license ]
time: 45
author_name: Merve Temel
---

## Prerequisites  
 - Create a developer user in a SAP BTP, ABAP Environment system.
 - Download the [latest Eclipse version and install ABAP Development Tools (ADT)](https://tools.hana.ondemand.com/#abap).

## Details
### You will learn  
  - How to download trust certificate for SAP S/4HANA Cloud connection
  - How to create business roles
  - How to create communication arrangement in SAP S/4HANA Cloud
  - How to create communication system in SAP S/4HANA Cloud
  - How to create inbound and outbound communication user
  - How to deactivate outbound services

---

[ACCORDION-BEGIN [Step 1: ](Download trust)]
  1. Open [SAP BTP cockpit](https://account.hana.ondemand.com/) and select your global account.

      ![Download trust](login2.png)

  2. Select your **global account**.

      ![Download trust](trust.png)

  3. Click **Subaccounts**.

      ![Download trust](trust2.png)

  4. Select your **subaccount**.

      ![Download trust](trust3.png)

  5. On your left menu select **Destinations** under **Connectivity** and click **Download Trust**.

      ![Download trust](trust4.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create role for communication management in SAP S/4HANA Cloud)]

  1. Open SAP S/4HANA Cloud system as an administrator.

      ![Create role for communication management](flp.png)

  2. Go to **Identity and Access Management** and select the **Maintain Business Roles** tile.

      ![Create role for communication management](flp2.png)

  3. Click **New** to create a new business role.

      ![Create role for communication management](flp3.png)

  4. Define your business role and your business role ID and click **Create**.

      ![Create role for communication management](flp4.png)

  5. Select **Assigned Business Catalogs** tab and click **Add**.

      ![Create role for communication management](flp6.png)

  6. Search `SAP_CORE_BC_COM`, select it, apply the change and click **OK**.

      ![Create role for communication management](flp7.png)

  7. Save your changes.

      ![Create role for communication management](flp8.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create communication arrangement in SAP S/4HANA Cloud)]

  1. Go to **Communication Management** and select the **Communication Arrangements** tile.

      ![Create communication arrangement](flp13.png)

  2. Click **New** to add a new communication arrangement.

      ![Create communication arrangement](flp14.png)

  3. Select **`SAP_COM_0008`** as scenario and give your arrangement a name.
     `SAP _COM_0008` as the communication scenario is needed to set up the communication with the business partner.

     Click **Create**.

      ![Create role for communication management](flp15.png)

  4. As a result you get following display. Click **Create**.

      ![Create role for communication management](flp16.png)

  5.  Click **New** to create a new communication system.

      ![Create role for communication management](flp17.png)

  6. Enter system ID and name. Click **Create**.

      ![Create role for communication management](flp18.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Copy URL)]

   1. Open [SAP BTP cockpit](https://account.hana.ondemand.com/), navigate to your **subaccount**. On the left menu select **Instances and Subscriptions** under **Services**. Click **Instances**, select your ABAP instance and **click** on it. Now scroll to **Service Keys** on the right side, select the menu and click **View**.

    ![Create communication system](adt.png)

   2. Copy the URL without the `https://` for later use.

    ![Create communication system](adt2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create communication system)]

  1. Switch to your SAP S/4HANA Cloud system, open your communication system, paste the URL as your host name. Enter `DUMMY` as logical system and business system. Don' t click save yet.

      ![Create communication system](flpxx.png)

  2. Set `OAuth 2.0 Identity Provider area` ON. Upload the certificate you downloaded in step `1.5` from your SAP BTP cockpit. **Copy** the common name (the string after CN=) from the signing certificate subject or the signing certificate issuer and **paste** it into the `OAuth 2.0 SAML Issuer` field.

     Don't save yet.

      ![Create communication system](hostname.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create inbound and outbound communication user)]
  1. Choose **`+`** in the inbound communication area to add a new communication user for inbound communication in SAP S/4HANA Cloud.

      ![Create inbound and outbound communication user](flp20.png)

  2. Choose **New User**.

      ![Create inbound and outbound communication user](flp21.png)

  3. Enter your communication system name as user name and description. Click **Propose Password** to generate a password and
     Click **Create**.

      ![Create inbound and outbound communication user](flp22.png)

  4. Click **OK**.

      ![Create inbound and outbound communication user](flp23.png)

  5. Choose **`+`** in the outbound communication user area to create a outbound communication user.

      ![Create inbound and outbound communication user](flp24.png)

  6. Select **`SSL Client Certificate`** as authentication method and **Client Default** as client certificate. Click **Create**.

      ![Create inbound and outbound communication user](flp25.png)

  7. Choose **Save** to save your communication system.

      ![Create inbound and outbound communication user](flp26.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 7: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
