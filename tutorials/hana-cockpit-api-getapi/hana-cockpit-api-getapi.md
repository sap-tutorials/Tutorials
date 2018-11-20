---
title: Call SAP HANA Cockpit GET APIs
description: Introduce all the available SAP HANA cockpit GET APIs and the method to use them.
auto_validation: true
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-api-management, products>sap-hana ]
time: 15
---

## Prerequisites  
 - Install the Python programming language on the computer that will call the  endpoints

## Details
### You will learn  
  - What are the cockpit GET APIs that you can use
  - How to call a cockpit GET APIs

SAP HANA cockpit provides non modifying (GET) REST API endpoints. The GET APIs don't create, delete, or change anything in the cockpit - they only inform you the existing information that you would like to examine.

There are three cockpit GET API endpoints:

1. *`RegisteredResourcesGet`*: returns information about the resources registered in SAP HANA cockpit
2. *`GroupsForUserGet`*: returns information about the resource groups that are visible to you
3. *`GroupResourcesGet`*: returns information about the resources in a specified group that is visible to you (uses `cockpit-landscape-svc`)

> Only two of the three cockpit GET APIs will be further explained in the following steps. To know more about the cockpit GET APIs, click [here](https://help.sap.com/viewer/afa922439b204e9caf22c78b6b69e4f2/2.8.0.0/en-US/4888d87f6a934dd18e5e782079e4ca63.html) to navigate to the **SAP Help Portal**.

> The **sample code** for all the cockpit APIs (GET and POST ones) is posted at the end of the *cockpit POST APIs* tutorial as an appendix. The code is written in Python and you are welcome to copy and run it to examine how each API works.

---


[ACCORDION-BEGIN [Step 1: ](Call the RegisteredResourcesGet endpoint)]
This endpoint provides information about the resources registered in SAP HANA cockpit that you are allowed to see.

To use this API, copy the following code to your Python file.
```Python
def list_cockpit_resources(client, authorization):
    targetURI = baseURL + '/resource/RegisteredResourcesGet'

    resourceListResponse = client.get(targetURI, verify=False, headers=get_header(authorization))
    return resourceListResponse.json()
```

Suppose we only want all the names and the ID numbers of the cockpit resources. We then need to parse the JSON response:
```Python
result = list_cockpit_resources(client, authorization)
resources = result["result"]
print("Cockpit resources:")
for resource in resources:
    print("- Resource name: " + resource["ResourceName"] + ", resource ID: " + resource["ResourceId"])
```

Run the entire program and your output should be similar to the following:
```
Cockpit resources:
- Resource name: SYSTEMDB@H4C, resource ID: 1
- Resource name: SYSTEMDB@DB1, resource ID: 2
- Resource name: DB1@DB1, resource ID: 3
```

[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Call the GroupsForUserGet endpoint)]
This endpoint returns information about the existing resource groups in SAP HANA cockpit that you are allowed to see. As mentioned in the previous tutorial **_Get Ready to Use the SAP HANA Cockpit REST API_**, you will need your port number for `cockpit-landscape-svc` to be able to call this endpoint.

To use this endpoint, copy the following code to your Python source file. Replace *<host.domain.com>* and *<port for cockpit-landscape-svc>* with your own identities.

```Python
def list_cockpit_groups(client, authorization):
    targetURI = 'https://' + '<host.domain.com>' + ':' + '<port for cockpit-landscape-svc>' + '/group/GroupsForUserGet'

    groupListResponse = client.get(targetURI, verify=False, headers=get_header(authorization))
    return groupListResponse.json()
```

Suppose we only want all the names of the cockpit groups, we then need to parse the JSON response:
```Python
result = list_cockpit_groups(client, authorization)
groups = result["result"]
print("Cockpit groups:")
for group in groups:
    print("- Group name: " + group["Name"])
```

Run the entire program and your output should be similar to the following:
```
Cockpit groups:
- Group name: GROUP1
- Group name: GROUP2
- Group name: GROUP3
```

[VALIDATE_2]

[ACCORDION-END]

---
