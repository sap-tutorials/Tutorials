---
parser: v2
author_name: Céline Audin
author_profile: https://github.com/celineaudinsap
auto_validation: true
time: 15
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform, tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

# Run a Business Process from the Launchpad
<!-- description --> Configure a form trigger in the SAP Launchpad to run a Business Process

## Prerequisites
 - Complete [Configure Launchpad](spa-configure-launchpad) tutorial

## You will learn
  - How to configure your form trigger as a tile in the SAP Launchpad Service.

---

### Configure the Process Trigger


You completed [Configure Launchpad](spa-configure-launchpad) tutorial. You assigned **My Inbox**, **Process Workspace**, **Visibility Scenario Dashboard** and **Visibility Scenario Instances** to **Everyone** role and clicked **Save**.

<!-- border -->![Assignments to Everyone Role](01.png)

1. Go back to **My Content** page.

    <!-- border -->![Back My Content](02.png)

2. Choose **Process Trigger**.

    <!-- border -->![Process Trigger](07.png)

2. Choose **Create a Local Copy**.

    <!-- border -->![Create Local Copy](08.png)

3. Choose **Edit** to make changes to the **Process Trigger** app.

    <!-- border -->![Edit](09.png)

4. Change the name of the app to **Sales Order Management**.

    <!-- border -->![Change Name](10.png)

5. Select the tab **Navigation**.

    <!-- border -->![Navigation](11.png)

6. Now you need to fill the **Default Value** of the app's parameters with the **Launchpad Configuration Parameter** of your process's **Trigger Settings**.

    <!-- border -->![Default Value Field](11a.png)

7. Navigate back to your deployed project in the **Application Development**.

8. Select the **Start Process**.

9. Copy the **Launchpad Configuration Parameter**.

    <!-- border -->![Copy Parameter](12.png)

    >This is the parameter you need to configure this form trigger as a tile in the SAP Launchpad Service.

10. Now navigate back to the Launchpad.

11. Paste the value in the **Default Value** field of the Sales Order Management app's parameters.

    <!-- border -->![Default Value](13.png)

12. Select the tab **Translation**.

13. Change the name of the **Title** to **Sales Order Management**.

    <!-- border -->![Change Name](13b.png)

12. Choose **Save**.



### Assign Sales Order Management Trigger to SAP Process Automation Group


1. Navigate back to **My Content**.

    <!-- border -->![My Content](13a.png)

2. Select **SAP Process Automation** group.

    <!-- border -->![SAP Process Automation](03.png)

3. Choose **Edit** to make changes to the group.

    <!-- border -->![Edit Group](04.png)

    You will now assign the **Sales Order Management** trigger to your group.

4. Select the search bar.

5. Choose the **+** to add the **Sales Order Management** item.

6. Choose **Save**.

    <!-- border -->![Save](05.png)


### Assign Sales Order Management Trigger to Everyone Role


1. Go back to **My Content**.

    <!-- border -->![My Contact](06.png)

2. Choose **Everyone**.

    <!-- border -->![Everyone](16.png)

3. Choose **Edit**.

4. You will now assign the **Sales Order Management** trigger to Everyone role.

5. Select the search bar.

6. Choose the **+** to add the **Sales Order Management** item.

7. Choose **Save**.

    <!-- border -->![Everyone](17.png)


### Launch Site


1. Navigate back to the **Site Directory**.

    <!-- border -->![Site Directory](18.png)

2. Go to the site.

    <!-- border -->![Go to site](19.png)

    You will be directed to the Launchpad where the Sales Order Management tile has been created.

    You can now trigger the process from the launchpad.

3. Choose **Sales Order Management** tile.

    <!-- border -->![Tile](20.png)

    > You can also see `MyInbox` and Process Workspace tiles added to the launchpad which can be used by the business users to access the tasks and monitor the processes respectively.

    You will be redirected to the **Order Approval Request Form**.

    <!-- border -->![Form](21.png)



---
