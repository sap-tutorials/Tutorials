---
parser: v2
author_name: René Jeglinsky
author_profile: https://github.com/renejeglinsky
keywords: cap
auto_validation: true
time: 25
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, programming-tool>java, software-product>sap-business-application-studio]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Configure Authentication and Authorization on SAP BTP
<!-- description --> Set up authentication and authorization on SAP BTP and deploy your secured application there.

## You will learn
  - How to test authorizations on the applications deployed to SAP BTP, Cloud Foundry
---
Before you deploy your authentication-enabled application you have to create an instance of service **Authorization and Trust Management Service** (XSUAA) and configure it, bind it to your application and provide it with the security descriptor that contains roles and scopes of your application. For more details, see section [Protecting Your Application](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/7c5c565f37c946faa154909004331d57.html) in the SAP BTP documentation.


### Configure roles for testing on SAP BTP


Open your application in the browser. Using the links on the welcome page you can check that you can't access the `Orders` entity or everything under the `AdminService`. You should see a `401` error in case you click on these.

To use the `AdminService`, you need to assign yourself to the role collection `BookStore_Administrators` that was defined in the `xs-security.json` file. To assign this role collection to your user you need to navigate to the **Security** **&rarr;** **Role Collections** section of your SAP BTP subaccount. Select the `BookStore_Administrators` role collection and choose **Edit**. Enter your email address in the **ID** and **E-Mail** field and choose **Save**.

    <!-- border -->![role assignment to administrator](role-assignment.png)



### Test application on SAP BTP

1. To retrieve the application URL run the following command:

    ```Shell/Bash
    cf app bookstore
    ```

2. Open the application in the browser. The according route can be found under routes of the previous step.

3. Observe that your application is now secured by an authentication flow (very quick) and that you can access the `AdminService` because you've assigned the respective role to you.

Done! You have learned how to deploy secured applications to SAP BTP, Cloud Foundry and assign the needed roles.


---
