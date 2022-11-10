---
parser: v2
auto_validation: true
primary_tag: software-product>sap-business-technology-platform
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, programming-tool>java ]
author_name: Nikola Simeonov
author_profile: https://github.com/nikolasimeonov77
time: 30
---

# Implement a Notification Application for Monitoring Java Apps
<!-- description --> Create an application that notifies you of critical metrics via e-mail or SMS, using SAP Monitoring service.

<!-- loio9c74305e78fa4d8181ba13cbde55d5cf -->

## Prerequisites
- To test the entire scenario, you need subaccounts in two regions: Europe (Rot/Germany) and US East.   For more information, see [Regions and Hosts Available for the Neo Environment](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/d722f7cea9ec408b85db4c3dcba07b52.html) and [SAP BTP Cockpit](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/19d7119265474dd18ec16fad2a0b28c1.html).
- To retrieve the metrics of Java applications as shown in this scenario, you need two deployed and running Java applications. For more information, see [Java: Application Operations](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/76f6dcfab9ec481dae0843873271d66c.html).
- You have installed Maven.
- You have downloaded and set up your Eclipse IDE, SAP BTP Tools for Java, and SDK.
  For more information, see [Setting Up the Development Environment](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/e815ca4cbb5710148376c549fd74c0db.html).

## You will learn
- How to configure a custom application to notify you of critical metrics of Java applications via e-mail or SMS.
- How to use the custom application to take self-healing actions by restarting the specified Java applications running in the Neo environment.

## Intro
In this tutorial, you implement a notification application that requests the metrics of the following Java applications (running in SAP BTP, Neo environment):

-   `app1` located in `a1` subaccount and `Europe (Rot/Germany)` region

-   `app2` located in `a2` subaccount and `US East` region


> Since the requests are sent to only two applications, the Maven project that you import in Eclipse only spawns two threads. However, you can change this number in the `MetricsWatcher` class, where the `ScheduledThreadPoolExecutor(2)` method is called. Furthermore, if you decide to change the list of applications, you also need to correct the list in the `Demo` class of the imported project.

When the notification application receives the Java application metrics, it checks for critical metrics. The application then sends an e-mail or SMS, depending on whether the metrics are received as critical once or three times. In addition, the notification application restarts the Java application when the metrics are detected as critical three times.

To learn more about the whole scenario, read the [Notification Scenario blog post](https://blogs.sap.com/2021/02/05/use-the-monitoring-service-for-critical-notifications-and-self-healing-of-java-applications-in-the-neo-environment/).

> If a Java application isn't started yet, the notification application automatically triggers the start process.


---

### Download the project


Download the `cloud-metricswatcher` project as a ZIP file from [https://github.com/SAP/cloud-metricswatcher](https://github.com/SAP/cloud-metricswatcher).


### Import into Eclipse


Extract the files into a local folder and import the folder in Eclipse as an existing Maven project.

> You can also upload your project by copying the URL from GitHub and pasting it as a Git repository path or URI after you switch to the Git perspective. Remember to switch back to a Java perspective afterward.



### Update Demo.java


Open the `Demo.java` class and update the following information:

- Your e-mail

- Your SMS

- Your logon credentials

- Your Java applications, and their subaccounts and regions

```Java
String mail_to = "my_email@email.com";
String mail_to_sms = "my_email@sms-service.com";


private final String auth_user = "my_user";
private final String auth_pass = "my_password";


String landscapeFqdn1 = "api.hana.ondemand.com";
String account1 = "a1";
String application1 = "app1";

String landscapeFqdn2 = "api.us1.hana.ondemand.com";
String account2 = "a2";
String application2 = "app2";
```



### Update `Mailsender.java`


Open the `Mailsender.java` class and update your e-mail account settings.

```Java
private static final String FROM = "my_email_account@email.com";
final String userName = "my_email_account";
final String password = "my_email_password";

public static void sendEmail(String to, String subject, String body) throws AddressException, MessagingException {
            // Set up the mail server
            Properties properties = new Properties();
            properties.setProperty("mail.transport.protocol", "smtp");
            properties.setProperty("mail.smtp.auth", "true");
            properties.setProperty("mail.smtp.starttls.enable", "true");
            properties.setProperty("mail.smtp.port", "587");
            properties.setProperty("mail.smtp.host", "smtp.email.com");
            properties.setProperty("mail.smtp.host", "mail.email.com");

}
```



### Test your scenario


1.   Open the SAP BTP cockpit and find your Java applications.

2.   Configure your Java applications to return critical metrics.

    To do this, you can create a JMX check with a low critical threshold for `HeapMemoryUsage` so that the check is always received in a critical state.

    For more information, see [create-jmx-check](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/298a207f33c4484b9894b7c4e2900566.html).

    > Example:

    > ```
    > neo create-jmx-check -a mysubaccount -b demo -u p1234567 -n "JMX Check Test - Heap Memory" -O java.lang:type=Memory -A HeapMemoryUsage -K used -U B -C 20000000 -h hana.ondemand.com
    > ```
    >
    > To use the console commands, you need to set up the console client. For more information, see [Set Up the Console Client](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/7613dee4711e1014839a8273b0e91070.html).

3.   Run your notification application in Eclipse and check the following:

    -   You receive an e-mail with subject **`A metric has reached a critical state.`** and body **`Metric HeapMemoryUsage for application app1 has reached а critical state.`** when a critical metric is received.

    -   You receive an SMS with text **`Metric HeapMemoryUsage for application app1 has reached critical state 3 times. The application will be restarted.`** when a critical metric is received three times.

    -   Your Java application is restarted when its critical metric is received three times.

You can check the status of your Java applications in the SAP BTP cockpit.


