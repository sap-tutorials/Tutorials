---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Undeploy Your CAP Application from Kyma
description: This tutorial shows you how to undeploy your CAP application from Kyma.
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp\\, kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Assign a Role Collection to a User](btp-app-kyma-role-assignment)


## Details
### You will learn
 - How to undeploy your CAP application from Kyma


---
> This tutorial will soon be phased out. 
> 
> For more tutorials about how to develop and deploy a full stack CAP application on SAP BTP, see:
>
> - [Develop a Full-Stack CAP Application Following SAP BTP Developer’s Guide](https://developers.sap.com/group.cap-application-full-stack.html)
> - [Deploy a Full-Stack CAP Application in SAP BTP, Cloud Foundry Runtime Following SAP BTP Developer’s Guide](https://developers.sap.com/group.deploy-full-stack-cap-application.html)
> - [Deploy a Full-Stack CAP Application in SAP BTP, Kyma Runtime Following SAP BTP Developer’s Guide](https://developers.sap.com/group.deploy-full-stack-cap-kyma-runtime.html)
>
> To continue learning how to implement business applications on SAP BTP, see:
>
> - [SAP BTP Developer’s Guide](https://help.sap.com/docs/btp/btp-developers-guide/what-is-btp-developers-guide?version=Cloud&locale=en-US)
> - [Related Hands-On Experience](https://help.sap.com/docs/btp/btp-developers-guide/related-hands-on-experience?version=Cloud&locale=en-US)
> - [Tutorials for ABAP Cloud](https://help.sap.com/docs/btp/btp-developers-guide/tutorials-for-abap-cloud?version=Cloud&locale=en-US)
> - [Tutorials for SAP Cloud Application Programming Model](https://help.sap.com/docs/btp/btp-developers-guide/tutorials-for-sap-cloud-application-programming-model?version=Cloud&locale=en-US)

[ACCORDION-BEGIN [Step 1: ](Undeploy your CAP application from Kyma)]
1. Uninstall the CAP application:

    ```Shell/Bash
    helm uninstall cpapp
    ```

2. Delete the database secret:

    ```Shell/Bash
    kubectl delete secret cpapp-db
    ```

3. Delete the container registry secret:

    ```Shell/Bash
    kubectl delete secret container-registry
    ```

4. Delete the namespace:

    ```Shell/Bash
    kubectl delete namespace  risk-management
    ```

[VALIDATE_1]
[ACCORDION-END]
---