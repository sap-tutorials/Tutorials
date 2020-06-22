---
auto_validation: true
title: Create Communication System to Connect to SAP Cloud Platform ABAP Environment.
description: Create a communication system on S/4HANA Cloud to connect to SAP Cloud Platform ABAP environment.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>intermediate, topic>abap-development, products>sap-cloud-platform, tutorial>license ]
time: 45
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
 - Create a developer user in a SAP Cloud Platform ABAP Environment system.
 - Download Eclipse Photon or Oxygen and install ABAP Development Tools (ADT). See <https://tools.hana.ondemand.com/#abap>.

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
  1. Open SAP Cloud Platform Cockpit and select your global account.

      ![Download trust](download.png)

  2. Click **Subaccounts**.

      ![Download trust](subaccount.png)

  3. Select your Cloud Foundry subaccount.

      ![Download trust](cloudfoundry.png)

  4. Go to **Destinations** and click **Download Trust** to get the certification.

      ![Download trust](destination.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create role for communication management in SAP S/4HANA Cloud)]

  1. Open SAP S/4HANA Cloud system as an administrator.

      ![Create role for communication management](administrator.png)

  2. Go to **Identity and Access Management** and select the **Maintain Business Roles** tile.

      ![Create role for communication management](identity.png)

  3. Click **New** to create a new business role.

      ![Create role for communication management](role.png)

  4. Define your business role and your business role ID.

      ![Create role for communication management](id.png)

  5. Select **Assigned Business Catalogs** tab and click **Add**.

      ![Create role for communication management](catalog.png)

  6. Search `SAP_CORE_BC_COM`, select it, apply the change and click **OK**.

      ![Create role for communication management](apply.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create communication arrangement in SAP S/4HANA Cloud)]
  1. Open SAP S/4HANA Cloud system as administrator.

      ![Create communication arrangement](administrator.png)

  2. Go to **Communication Management** and select the **Communication Arrangements** tile.

      ![Create communication arrangement](communication.png)

  3. Click **New** to add a new communication arrangement.

      ![Create communication arrangement](arrangement.png)

  4. Select **`SAP_COM_0008`** as scenario and give your arrangement a name.
     `SAP _COM_0008` as the communication scenario is needed to set up the communication with SAP Cloud Platform as business partner.
     Click **Create**.

      ![Create role for communication management](scenario.png)

  5. As a result you get following display. Click **New** to create a new communication system.

      ![Create role for communication management](new.png)

  6. Enter a system ID and system name.

      ![Create role for communication management](systemid.png)

 7. You need to switch to your SAP Cloud Platform Cockpit to set your host name.

      ![Create role for communication management](switch.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Select instance URL)]
  1. Open SAP Cloud Platform Cockpit and select your global account.

      ![Select instance URL](global.png)

  2. Select **Subaccounts**.

      ![Select instance URL](subaccounts.png)

  3. Select your Cloud Foundry subaccount.

      ![Select instance URL](cloudfoundry.png)

  4. Switch to your space.

      ![Select instance URL](space.png)

  5. Click **Service Instances** and select your own service instance.

      ![Select instance URL](instance.png)

  6. Now you can see your service key. Inside your service key is your instance URL.
     Your instance URL is structured as following:

    `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx.abap.eu10.hana.ondemand.com`
     Copy your instance URL without `https://` to paste it as host name in your communication system.

      ![Select instance URL](key.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create communication system)]
  1. Switch to your SAP S/4HANA Cloud system, open your communication system, paste your host name and click save.

      ![Create communication system](comsys.png)

  2. Enter `DUMMY` as logical system and business system. Don' t click save yet.

      ![Create communication system](dummy.png)

  3. Check enabled in the `OAuth 2.0 Identity Provider area`. Upload the certificate
     you downloaded in step `1.4` from your SAP Cloud Platform Cockpit.
     Copy the common name (the string after CN=) and paste it into the Provider Name field.

     Don't save yet.

      ![Create communication system](oauth.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create inbound and outbound communication user)]
  1. Choose **`+`** in the inbound communication area to add a new communication user for inbound communication in SAP S/4HANA Cloud.

      ![Create inbound and outbound communication user](inbound.png)

  2. Choose **New User**.

      ![Create inbound and outbound communication user](inbound2.png)

  3. Enter your communication system name as user name and description. Click **Propose Password** to generate a password and
     Click **Create**.

      ![Create inbound and outbound communication user](inbound3.png)

  4. Click **OK**.

      ![Create inbound and outbound communication user](inbound4.png)

  5. Choose **`+`** in the outbound communication user area to create a outbound communication user.

      ![Create inbound and outbound communication user](inbound5.png)

  6. Select **`SSL Client Certificate`** as authentication method and **Default Client Certificate** as certificate type.

      ![Create inbound and outbound communication user](inbound6.png)

  7. Choose **Save** to save your communication system.

      ![Create inbound and outbound communication user](inbound7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Deactivating outbound services)]
  1. Edit your communication system and select **`OAuth 2.0`** in the inbound communication area.

      ![Select Instance URL](deactivate.png)

  2. Select **Default Client Certificate** in the outbound communication area.

      ![Select Instance URL](deactivate2.png)

  3. Deactivate all outbound communication services. The outbound services aren't relevant
     for outbound communication from the SAP Cloud Platform ABAP environment to SAP S/4HANA Cloud
     Click **Save**.

      ![Select Instance URL](deactivate3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
