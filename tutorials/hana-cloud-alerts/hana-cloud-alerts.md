---
title: Alerts in SAP HANA Database and Data Lake
description: Learn how to configure and view alerts in SAP HANA Cloud.
auto_validation: true
time: 30
tags: [ tutorial>beginner, products>sap-alert-notification-service-for-sap-btp]
primary_tag: products>sap-hana-cloud
---

## Prerequisites
 - Access to an SAP HANA Cloud trial or production instance with a version of 2021 QRC 2 or higher.

## Details
### You will learn
  - An overview of alerts
  - How to configure, trigger, and access alerts
  - How to use the SAP BTP Alert Notification Service (ANS) to be notified of alerts

Alerts can inform you of potential issues before they occur, such as when the number of rows in a SAP HANA database table is approaching 2 billion, or of an issue currently occurring, such as a user in a data lake is locked out.  You can find details of SAP HANA database alerts which have been raised by looking at the SAP HANA cockpit Alerts app. This is known as a pull approach.

Alternatively, alert details can be pushed to several configured channels such as email, Slack, or Microsoft Teams.

---

[ACCORDION-BEGIN [Step 1: ](Examine SAP HANA Cloud, HANA Database alert definitions)]

In this step, the SAP HANA cockpit will be used to examine three alert definitions.

1. Open the SAP HANA Cockpit.

    ![Open SAP HANA Cockpit](open-cockpit.png)

2. Open the Alert Definitions app.

    ![alerts app in the cockpit](alerts-app-cockpit.png)

3. The three alerts that will be triggered in step 2 of this tutorial are shown below.

    ![alerts definitions in the cockpit](alert-defintions-cockpit.png)  

    Notice that alerts have a name, ID, description, category, and a suggested user action such as an SAP Note.

    ![restarted-services-alert-details](alert-definitions-cockpit-details.png)

    The categories are shown below.

    | SAP HANA Database Categories |
    | ----------- |
    | Other |
    | Availability |
    | Memory |
    | Disk |
    | Configuration |
    | Sessions/Transactions |
    | Backup |
    | Diagnosis Files |
    | Security |

4. Alerts may have severity and threshold levels.

    | Severity |
    | ----------- |
    | High |
    | Medium |
    | Low |
    | Information |    

    As an example, long-running statements (ID 39), has its threshold values set to 10 minutes for Medium, 5 minutes for Low, and 2 minutes for Info.

    ![long running statement updated thresholds](long-running-thresholds.png)

5. Alert checks are scheduled to run at a specified interval and can be enabled or disabled.  For example, record count of non-partitioned column-store tables ( ID 17 ), has an interval value of 1 hour which means that this check is performed each hour and is currently enabled.

    ![enabled and interval](restarted-services-interval.png)

    Note that the check for an alert can be manually triggered by pressing the **Check Now** button.

6. Alerts also have a retention period.  Once triggered, depending on their type, they will remain for a set duration such as 14 or 42 days.

  For additional details, consult [How Alerts are Generated](https://help.sap.com/viewer/f9c5015e72e04fffa14d7d4f7267d897/latest/en-US/8eca57e7e82e4b788246b6d9db020937.html) and
  the topic [Alerts](https://help.sap.com/viewer/9630e508caef4578b34db22014998dba/cloud/en-US/923f1c8f200b44708e7ee68876d5fe2b.html) in the document SAP HANA Cloud Database Administration with SAP HANA Cockpit.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Trigger alerts in a SAP HANA database)]

The following instructions demonstrate a few examples of triggering alerts in an SAP HANA database.      
> Ensure these actions are performed on a non-production system such as a trial account or only use the test alert.

1. Open the SAP HANA database explorer.

    ![Open the database explorer](open-dbxt.png)

2. Execute the following SQL to trigger a high (indicated by the parameter value of 4) severity test alert.

    ```SQL
    CALL _SYS_STATISTICS.Trigger_Test_Alert(?, 4, 'High test alert');  
    ```

    > The alert will be viewed in SAP HANA Cockpit in step 3.

3. Execute the following SQL to trigger long-running statements (ID 39) at each threshold assuming the thresholds were set to 1, 2 and 3 minutes with an interval time set to 1 minute.

    ```SQL
    DO BEGIN
      -- Wait for a few seconds
      USING SQLSCRIPT_SYNC AS SYNCLIB;
      CALL SYNCLIB:SLEEP_SECONDS( 300 );  --runs for 5 minutes
      -- Now execute a query
      SELECT * FROM M_TABLES;
    END;
    ```

    > Long running statements can be run as a background activity which will continue even if the machine the SAP HANA database explorer is running on loses internet connectivity or the tab is closed.  
    >
    >![run as a background activity](run-as-background.png)
    >
    For additional details on the database explorer see the tutorial [Get Started with the SAP HANA Database Explorer](group.hana-cloud-get-started).

4. The alert record count of non-partitioned column-store tables (ID 17) can be triggered by executing the following SQL.  

    ```SQL
    --The default threshold for 'Record count of non-partitioned column-store tables' is 300 million
    -- This SQL may take a minute or two to run
    -- Create a table and insert more than 300 million rows into it
    DO BEGIN
           DECLARE i INT;
           CREATE TABLE MYTABLE(MYVALUE INT);
           INSERT INTO MYTABLE VALUES(1);
           INSERT INTO MYTABLE VALUES(2);
           INSERT INTO MYTABLE VALUES(3);
           INSERT INTO MYTABLE VALUES(4);
           INSERT INTO MYTABLE VALUES(5);
           FOR i IN 1 .. 26 DO
                  INSERT INTO MYTABLE (SELECT * FROM MYTABLE);
           END FOR;
           SELECT COUNT(*) FROM MYTABLE;
    END;
    SELECT TOP 100 * FROM MYTABLE;

    -- To resolve, partition the table
    -- ALTER TABLE MYTABLE PARTITION BY HASH(MYVALUE) PARTITIONS 5;

    -- Clean up
    -- DROP TABLE MYTABLE;
    ```

    As the default interval time for this check by default is set to 1 hour, it can be manually trigged by pressing the Check Now button in the alert definitions app within the SAP HANA cockpit.

    ![check now](check-now.png)

    > Note that two other alerts may also be triggered by the above SQL; table growth of non-partitioned column-store tables and  record count of column-store table partitions.

5. The Database Overview in the SAP HANA cockpit will indicate if a medium or high alert was triggered.  The refresh interval can be set to control how often the contents of the page are updated.

    ![monitoring with refresh on](cockpit-monitoring.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](View SAP HANA database alerts using the alerts app)]
The following instructions will show how to view a triggered SAP HANA database alert in the alerts app in the SAP HANA cockpit.

1. After the long-running statement completes, open the alerts app to view the triggered alerts.  

    ![viewing the triggered alerts in the cockpit](triggered-alerts-cockpit.png)

    > Current alerts are alerts that were triggered by the last scheduled run of the alert definition.  You may need to select **Past** from the Type drop down.

    >---

    > Alerts can be sorted and grouped.

    > ![sort and group](sort-and-group.png)

    >---

    > Details about a SQL statement from a long-running statement alert can be found out with the following query.

    >```SQL
    SELECT * FROM M_SQL_PLAN_CACHE WHERE STATEMENT_HASH='XXXXXXXXXXXXXXXXXXXXX';
    >```

    >![using the statement hash](long-running-statement-hash.png)


2. The test alert will resolve itself after 5 minutes or can be removed (indicated by the parameter value of 0) from the Alerts tile by executing the following statement.

    ```SQL
    CALL _SYS_STATISTICS.Trigger_Test_Alert(?, 0, 'Resolve test alert');
    ```

    Additional details on the test alert are available at [SAP Note 3004477 - Usage of statistics server test alert (ID 999)](https://launchpad.support.sap.com/#/notes/3004477).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Trigger an alert in SAP HANA Cloud data lake)]
The following instructions show one example of triggering an alert in a data lake.  The alert will be triggered when a user becomes locked out because an incorrect password was provided too many times.     

1. In a SAP HANA database explorer that is connected to a data lake, execute the following SQL to create a login policy and a new user.

    ```SQL
    CREATE LOGIN POLICY lp max_failed_login_attempts=3;
    GRANT CONNECT TO user2 IDENTIFIED BY 'Password2';
    GRANT SELECT ANY TABLE TO user2;
    GRANT SET ANY CUSTOMER PUBLIC OPTION to user2;
    ALTER USER user2 LOGIN POLICY lp;
    ```

2. Create a new connection to the data lake using user2 but with an incorrect password.

    First choose **Properties** for the existing connection and copy the host value.

    ![properties of the existing connection](dl-properties.png)

    Create a new connection using the copied host, port 443, user2 and an incorrect password.

    ![adding a connection with an incorrect password](add-data-lake.png)

    After pressing OK an attempt will be made to connect to the data lake.  After three failed attempts with the incorrect password, user2 will become locked.  This can be seen in the SAP HANA cockpit.

    ![Locked user2](data-lake-user-locked.png)

    The alert can pushed to a channel such as a specified email address which will be shown in step 5.

3. The user can be unlocked using the following SQL or via the UI in the SAP HANA cockpit.

    ```SQL
    ALTER USER user2 RESET LOGIN POLICY;
    --DROP USER user2;
    --DROP LOGIN POLICY lp;
    ```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Set up email notification when an alert occurs)]
The Business Technology Platform (BTP) includes a service called the Alert Notification Service (ANS) that provides a common way for other services or applications running in the BTP to send out notifications such as an email, a post to a Microsoft Teams or Slack channel, the creation of a ticket in `ServiceNow`, or a webhook to send events to any Internet REST endpoint.  The SAP HANA Cloud database and data lake pass on events to (ANS) when an alert is triggered.  

![BTP with HC and ANS](btp-hc-ans.png)

For an overview of ANS and information about the different service plans including free tier, see [SAP Alert Notification Service for SAP BTP](https://discovery-center.cloud.sap/serviceCatalog/alert-notification-service?tab=feature&region=all&service_plan=standard) in the SAP Discovery center and watch the associated video [SAP Alert Notification service for SAP BTP - Overview](https://www.youtube.com/watch?v=_DInhi4Skn4).

In this step, ANS will be configured to act on the incoming notifications by sending an email with the details of the alert.  First, an instance of the alert notification service will be created.  Then, two conditions will be created; one that matches notifications sent from an SAP HANA database and one for the data lake.  An email action will also be created that describes who to send an email to and what content to include in the email when one of the conditions occurs.  Finally, a subscription will be created that will use the two conditions and the action.  Having the conditions and actions separate from the subscription enables them to be reused in multiple subscriptions.   

1. Create an instance of the Alert Notification Service in the SAP BTP Cockpit.

    ![find the Alert Notification Service](ans.png)

    > The Alert Notification Service must be in the same cloud foundry org and space as the SAP HANA Cloud instances that it will be receiving notifications from.

2. Provide a name for the alert notification service instance and press the **Create** button.

    ![ANS basic information](ans-basic.png)

3. Once the instance has been created, click on **Manage Instance**.

    ![Open ANS instance](ans-open.png)

4. Create a condition for SAP HANA database alerts by selecting **Conditions** and then pressing **Create**.

    ![create condition](create-condition.png)

5. Specify the condition details and press **Create**.

    ![create condition details](create-condition-db.png)

    > Alternatively, a condition `resource.resourceType` set to equal `hana-cloud-hdb` could be used.

    > ---

    > Note that the Mandatory checkbox is left unchecked.  This means that this condition can be `ORed` with one or more other non mandatory conditions.

    ![create condition](create-condition-dl.png)

6. Create another condition for data lake alerts.  Specify the condition details and press **Create**.

    ![assign the condition for data lake](data-lake-condition.png)

7. Create an action by selecting **Actions** and the pressing **Create**.

    ![create an action](create-action.png)

8. Specify the type of action to be **Email**.

    ![email action](create-action-email.png)

    See also [Managing Actions](https://help.sap.com/viewer/5967a369d4b74f7a9c2b91f5df8e6ab6/Cloud/en-US/8a7e092eebc74b3ea01d506265e8c8f8.html) for details on other available action types.

9. Provide a **Name** and then scroll to the additional properties section.

    ![email action details](create-action-email-details.png)

    In the **Additional Properties** section, provide values for **Email Address**, **Subject Template** and **Payload Template**.

    ![advanced action details](create-action-email-details-advanced.png)

    &nbsp;
    ```Subject Template
    {severity} {resource.resourceType} {eventType} {tags.ans:status} occurred on {resource.resourceName}
    ```

    ```Payload Template
    AlertID: {tags.hanaAlertRuleId}
    Body: {body}
    Recommended Action: {tags.ans:recommendedAction}
    Instance Details: {resource.tags.*}
    ```

    > It is also possible to leave the subject and payload template fields empty.  In this case, a default template will be used.

    Details on the available tags for each alert are provided at [Built-In Events](https://help.sap.com/viewer/5967a369d4b74f7a9c2b91f5df8e6ab6/Cloud/en-US/2ef9c72833df4f2690f071c47f50f5af.html).  

    ![example documentation](ANS-documentation-for-HANA-Cloud.png)

10. A confirmation token will be sent to the email address.  Click on the provided link or copy that value and use it to confirm the action as shown in the next sub-step.

    ![email with confirmation token](ans-sub-action-type-email-confirm0.png)

11. Click on **`email_action`** to open it.

    ![confirm email address](ans-sub-action-type-email-confirm.png)

    Press the **Confirm Action** button.

    ![confirm email address](ans-sub-action-type-email-confirm2.png)

    Enter the confirmation token.

    ![confirmation token](ans-sub-action-type-email-confirm3.png)

12. Create a subscription by selecting **Subscriptions** and pressing **Create**.

    ![subscriptions](ans-sub.png)

    Provide a **Name** and press the **Create** button.

    ![Add a subscription](ans-sub-create.png)

13. Assign the conditions to the subscription by including the two previously created conditions and pressing the **Assign** button.

    ![assign the condition](ans-sub-condition-assign.png)

    > Another example is to specify the instance ID of a specific production server to monitor and to only send an alert when the alert first occurs (create state).
    >
    > ![Alternative conditions example](conditions-example.png)

14. Assign the action to the subscription by including the previously created action and pressing the **Assign** button.

    ![assign the action](ans-sub-action-type-email-assign.png)

15. Completed subscription.

    ![subscription complete](ans-sub-complete.png)

16. Repeating the actions in step 2 and 4 should now send emails like the ones shown below.

    Test Alert

    ![test alert email](test-alert-email.png)

    Test Alert (default template)

    ![test alert email](default-template-email.png)

    > Notice that the `ans:status` is CLOSE. Events can have a status of CREATE, UPDATE or CLOSE.

    Long-Running Statement

    ![long running email](long-running-email.png)

    Record count of non-partitioned column-store tables

    ![record count email](number-of-records-email.png)

    Locked user

    ![data lake locked user](locked-user-email.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Set up notification with a Microsoft Teams channel)]

This step will briefly show an example of how to receive a notification in Microsoft Teams.

1.  In Microsoft Teams, search for **Incoming Webhook** in Apps.

    ![teams webhook app search](teams-webhook-search.png)

2.  Choose to add the app to a team.

    ![teams add webhook](teams-add-webhook.png)

3.  Specify a name for the webhook, optionally upload an image, and press **Create**.

    ![teams add connector](teams-add-connector.png)

    After pressing create, copy the provided URL and press **Done**.

    ![teams copy webhook url](teams-webhook-copy-url.png)

    A notification will appear in the channel that the connector has been added.

    ![teams connector added posts](teams-connector-added.png)

4. Similar to the previous step where an ANS action was created for email, create one for Microsoft Teams in the SAP BTP Cockpit.

    ![add ANS teams action](teams-create-action.png)

    Provide the webhook URL.

    ![configure ANS teams action](teams-create-action-configure.png)

5. Assign the action to a subscription.

    ![assign action to a subscription](subscription-assigned-actions.png)

6. After triggering an alert, the Microsoft Teams channel will show the notification.

    ![notification in teams](teams-notification.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Set up notification with a Slack channel)]

This step will briefly show an example of how to receive a notification in Slack.

1. In a browser open [api.slack.com](https://api.slack.com/), click on **Create a custom app**, and then choose **from scratch**.

    Provide an app name, a workspace, and press **Create App**.

    ![slack create a custom app](slack-create-custom-app.png)

2. Choose to add **Incoming Webhooks**.

    ![Add incoming webhooks](slack-add-incoming-webhook.png)

3. After activating incoming webhooks, click on **Add New Webhook to Workspace**.

    Specify the Slack channel that will be used to post to and click **Allow**.

    ![slack-choose-channel](slack-add-webhook-to-channel.png)

    Copy the provided Webhook URL.

4. Similar to the step where an ANS action was created for email, create one for Slack in the SAP BTP Cockpit.

    ![add Slack action](slack-create-action.png)

    Provide the webhook URL.

    ![configure ANS Slack action](slack-create-action-configure.png)

    An example of a payload template.

    ```JSON
    [{
		"type": "section",
		"text": {
			"type": "mrkdwn",
			"text": "* {severity} {eventType} {tags.ans:status} on {resource.resourceName}*"
		}
  	},
  	{
  		"type": "divider"
  	},
  	{
  		"type": "section",
  		"text": {
  			"type": "mrkdwn",
  			"text": ":pencil2: Subject: {subject}"
  		}
  	},
  	{
  		"type": "section",
  		"text": {
  			"type": "mrkdwn",
  			"text": ":scroll: Body: {body}"
  		}
  	},
  	{
  		"type": "section",
  		"text": {
  			"type": "mrkdwn",
  			"text": " :medical_symbol: Recommended Action: {tags.ans:recommendedAction}"
  		}
  	}]
    ```

5. Assign the action to a subscription.

    ![assign action to a subscription](subscription-assigned-actions.png)

6. After triggering an alert, the Slack channel will show the notification.

    ![notification in Slack](slack-notification.png)

Congratulations! You have now configured alerts, triggered and viewed them via the SAP HANA cockpit and  received them via email, Microsoft Teams, and Slack using the Alert Notification Service (ANS).  

[VALIDATE_1]
[ACCORDION-END]

---
