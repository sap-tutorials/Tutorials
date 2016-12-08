---
title: Customize game mechanics of HelpDesk application in SAP HANA Cloud Platform
description: Manually customize the game mechanics of the gamified HelpDesk application using the Gamification Workbench on the SAP HANA Cloud Platform.
tags: [  tutorial>beginner, topic>cloud, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Deploy gamified HelpDesk application to SAP HANA Cloud Platform](http://www.sap.com/developer/tutorials/hcp-deploy-gamified-application.html)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
In this tutorial, you will tweak the game mechanics of the HelpDesk application manually using the Gamification Workbench on the SAP HANA Cloud Platform. You will increase the number of experience points the user receives for solving sample tickets and increase the total points for solving sample critical tickets. As a result of that change, you will also have to adapt the game levels.

### Time to Complete
**20 Min**.

---

1. In your Web browser, open the cockpit of the [SAP HANA Cloud Platform](https://account.hanatrial.ondemand.com/cockpit). Then select **Services** from the left-hand navigation and click the **Gamification Service**.

    ![Open Gamification Service](1.png)

2. Click **Go to Service**.

    ![Go to Service](2.png)

    >You can bookmark this page for ease of access in the future.

3. Select the **`HelpDesk`** application from the drop-down menu in the upper-right corner.

    ![Select HelpDesk Application](3.png)

4. To get an overview of existing rules, navigate to the **Game Mechanics** tab and select **Rules** from the second level navigation bar.

    ![Check Rules](4.png)

5. Open the rule **`GiveXP`** by clicking on it. Select the **Consequence** tab under **Rule Logic**.

    ![Open XP Rule](5.png)

6. Select **Edit** and increase the number of **`Experience Points`** that users receive when they respond to a ticket from `1` to `3`.

    ![Change XP Points](6.png)

    ```java
    updateAPIv1.givePointsToPlayer($playerid, 'Experience Points', 3, 'Ticket processed');
    update(engine.getPlayerById($playerid));
    ```

7. Click **Save**.

    ![Save Experience Points](7.png)

8. Open the rule **`GiveCT`** by clicking on it. Select the **Consequence** tab under **Rule Logic**.

    ![Open CT Rule](8.png)

9. Select **Edit** and increase the number of **`Experience Points`** that users receive when they respond to a critical ticket from `2` to `7`.

    ![Change CT XP Points](9.png)

    ```java
    updateAPIv1.givePointsToPlayer($playerid, 'Critical Tickets', 1, 'Critical ticket processed');
    updateAPIv1.givePointsToPlayer($playerid, 'Experience Points', 7, 'Critical ticket processed');
    update(engine.getPlayerById($playerid));
    ```

10. Click **Save**.

    ![Save Experience Points](10.png)

    >**`Experience Points`** for responding to critical tickets are given in addition to **`Experience Points`** given for processing any ticket. In other words, users will now receive 10 points when solving a critical ticket (7 critical points plus 3 points for solving any ticket).

11. You can ensure that these rules were successfully updated by checking the notification center in the upper right-hand corner.

    ![Notification Center](11.png)

12. Still in **Game Mechanics**, select **Levels** from the second level navigation bar.

    ![Levels Tab](12.png)

13. Select all the levels in the table and choose **Delete**.

    ![Delete Levels](13.png)

14. Click **Add**. Enter a **Name**, make sure that **Experience Points** are selected in the **Points** drop-down, and enter a value in the **Point Threshold**. Click **Save**.

    ![Create Levels](14.png)

15. Repeat the previous step three to five times until you have a variety of levels.

    >Start with a level with a small threshold, so that newly registered users will have a level already assigned.

16. Launch the gamified HelpDesk application by first selecting **Help** in the upper-right corner.

    ![Help Button](16.png)

    Then clicking **Open HelpDesk** from the pop-up window.

    ![Open HelpDesk](17.png)

17. Select **continue** when the home screen of the sample gamified HelpDesk application appears.

    ![Enter HelpDesk Application](18.png)

18. To see your new game mechanics in action, first respond to one of the generated sample tickets.

    ![Respond to Ticket](19.png)

    Then click your name in the upper-right corner to go to your profile.

    ![Go to Profile](20.png)

    In your profile, you should see the new levels (green box) as well as the increased number of experience points you received (red box).

    ![See Results](21.png)

## Next Steps
 - Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
