---
parser: v2
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp\\, kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Undeploy Your CAP Application from Kyma
<!-- description --> This tutorial shows you how to undeploy your CAP application from Kyma.

## Prerequisites
 - [Assign a Role Collection to a User](btp-app-kyma-role-assignment)


## You will learn
 - How to undeploy your CAP application from Kyma


---

### Undeploy your CAP application from Kyma

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


---