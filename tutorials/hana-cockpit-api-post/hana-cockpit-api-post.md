---
title: Call SAP HANA Cockpit POST APIs
description: Introduce all the available SAP HANA cockpit POST APIs and the method to use them.
auto_validation: true
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-api-management, products>sap-hana ]
time: 25
---

## Prerequisites  
 - Install the Python programming language on the computer that will call the  endpoints

## Details
### You will learn  
  - What are the cockpit POST APIs that you can use
  - How to call a cockpit POST APIs

---

SAP HANA cockpit provides modifying (POST) REST API endpoints. Unlike the GET endpoints, the POST endpoints create, delete, or change objects in the cockpit.

There are ten cockpit POST API endpoints:

1. *`SystemRegister`*: registers an SAP HANA resource in the cockpit
2. *`ResourceUnregister`*: `unregisters` an SAP HANA resource that's already registered
3. *`GroupCreate`*: creates a new resource group
4. *`GroupDelete`*: deletes a resource group
5. *`CockpitUserCreate`*: creates a new cockpit user
6. *`CockpitUserDelete`*: deletes a cockpit user
7. *`GroupResourceAdd`*: adds a resource to a group
8. *`GroupResourceRemove`*: removes a resource from a group
9. *`GroupUserAdd`*: adds a user to a group
10. *`GroupUserRemove`*: removes a user from a group

> Only two of the ten cockpit POST APIs will be further explained in the following steps. To know more about the cockpit POST APIs, click [here](https://help.sap.com/viewer/afa922439b204e9caf22c78b6b69e4f2/2.8.0.0/en-US/a8aa6fdd1557450ea76cb691d7799ab1.html) to navigate to the **SAP Help Portal**.

> The **sample code** for all the cockpit APIs (GET and POST ones) is posted in **_Step 5_**. The code is written in Python and you are welcome to copy and run it to examine how each API works.

---

[ACCORDION-BEGIN [Step 1: ](Call the SystemRegister endpoint)]
This endpoint registers an SAP HANA resource in the cockpit. You have two options to register a cockpit resource: either by using its instance number or its port number.

**Option 1 - via instance number:**

Copy the following code to your Python program:
```Python
def add_resource_via_instance(client, authorization, hostName, instanceNumber, techUser, techUserCredentials,\
                         databaseName, encryptJDBC, validateServerCertificate, hostNameInCertificate):
    if databaseName == None:
        data = {'hostName': hostName,
				'instanceNumber': instanceNumber,
				'techUser': techUser,
				'techUserCredentials': techUserCredentials,
				'isMultiTenant': False,
				'encryptJDBC': encryptJDBC,
			    'validateServerCertificate': validateServerCertificate,
                'hostNameInCertificate': hostNameInCertificate}

    else:
        data = {'hostName': hostName,
				'instanceNumber': instanceNumber,
				'techUser': techUser,
				'techUserCredentials': techUserCredentials,
				'databaseName': databaseName,
				'isMultiTenant': True,
				'encryptJDBC': encryptJDBC,
		    	'validateServerCertificate': validateServerCertificate,
                'hostNameInCertificate': hostNameInCertificate}

    targetURI = baseURL + '/registration/SystemRegister'
    resourceCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return resourceCreateResponse.json()
```

Then call the function by replacing the parameters with the corresponding information. In this example, we will choose not encrypting anything nor putting an override hostname in certificate.
```Python
result = add_resource_via_instance(client, authorization, '<host.domain.com>', '<instance>', 'TECH_USER', 'Password1', '', False, False, '')
print(json.dumps(result, indent=4, sort_keys=True))
```

Run the entire program (make sure to delete or comment out the calling statements for the GET endpoints) and your output should be similar to the following:
```JSON
{
    "result": {
        "resid": 111,
        "resourceName": "SYSTEMDB@DEMO"
    }
}
```

**Option 2 - via port number:**

You can also register a resource by specifying the SQL port for its `indexserver` (for tenant databases) or `nameserver` (for a system database).

Copy the following code to your Python program:
```Python
def add_resource_via_port(client, authorization, hostName, port, techUser, techUserCredentials, encryptJDBC,\
						  validateServerCertificate, hostNameInCertificate):
    data = {'hostName': hostName,
            'port': port,
            'techUser': techUser,
            'techUserCredentials': techUserCredentials,
			'encryptJDBC': encryptJDBC,
			'validateServerCertificate': validateServerCertificate,
            'hostNameInCertificate': hostNameInCertificate}

    targetURI = baseURL + '/registration/SystemRegister'
    resourceCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return resourceCreateResponse.json()
```

Then call the function by replacing the parameters with the corresponding information. In this example, we will choose not encrypting anything nor putting an override hostname in certificate.
```Python
result = add_resource_via_port(client, authorization, '<host.domain.com>', '<port>', 'TECH_USER', 'Password1', False, False, '')
print(json.dumps(result, indent=4, sort_keys=True))
```

Run the entire program (make sure to delete or comment out the calling statements for the GET endpoints) and your output should be similar to the following:
```JSON
{
    "result": {
        "resid": 1111,
        "resourceName": "SYSTEMDB@DEMO"
    }
}
```

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Call the ResourceUnregister endpoint)]
This endpoint `unregisters` an SAP HANA resource. Copy the following code to your Python file.

```Python
def delete_cockpit_resource(client, authorization, resid):
    data = {'resid': resid}

    targetURI = baseURL + '/registration/ResourceUnregister'
    resourceDeleteResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return resourceDeleteResponse.text
```

For this example, we will delete the cockpit resource that we just registered in **_Step 1_**. Make sure to replace the `<resid>` parameter with the ID that was printed in the output from the previous step.
```Python
result = delete_cockpit_resource(client, authorization, '<resid>')
if result == '':
  print("SUCCESS")
else:
  print(json.dumps(result, indent=4, sort_keys=True))
```

Run the entire program (make sure to delete or comment out the calling statement from **_Step 1_**) and your output should be similar to the following:
```
SUCCESS
```

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Call the CockpitUserCreate endpoint)]
This endpoint creates a new cockpit user. To use this API, copy the following code to your Python file.

```Python
def add_cockpit_user(client, authorization, userName, userPWD, email, roleCollections):
    data = {'username': userName,
            'password': userPWD,
            'email': email,
            'roleCollections': roleCollections}

    targetURI = baseURL + '/user/CockpitUserCreate'
    userCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return userCreateResponse.json()
```

As an example, we will create a cockpit user named *DEMO* with random password and email, give it all the available roles and print the result in JSON.
```Python
role_collections = ["COCKPIT_ADMIN", "COCKPIT_CONFIG_TEMPLATE_ADMIN", "COCKPIT_RESOURCE_ADMIN", "COCKPIT_USER", "COCKPIT_USER_ADMIN"]
result = add_cockpit_user(client, authorization, 'DEMO', 'Demo1234', 'demo@demo.com', role_collections)
print(json.dumps(result, indent=4, sort_keys=True))
```

Run the entire program (make sure to delete or comment out the calling statement from **_Step 2_**) and your output should be similar to the following:
```JSON
{
    "result": {
        "cockpitId": "123",
        "email": "demo@sap.com",
        "roleCollections": [
            "COCKPIT_ADMIN",
            "COCKPIT_CONFIG_TEMPLATE_ADMIN",
            "COCKPIT_RESOURCE_ADMIN",
            "COCKPIT_USER",
            "COCKPIT_USER_ADMIN"
        ],
        "username": "DEMO"
    }
}
```

In the area below, copy your Python output, and then click **Validate**.

[VALIDATE_3]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Call the CockpitUserDelete endpoint)]
This endpoint deletes an existing cockpit user. To use this API, copy the following code to your Python file.

```Python
def delete_cockpit_user(client, authorization, deleteFromUAA, userId, username):
    data = {'deleteFromUAA': deleteFromUAA,
            'userId': userId,
            'username': username}

    targetURI = baseURL + '/user/CockpitUserDelete'
    userDeleteResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return userDeleteResponse.text
```

For this example, we will delete the cockpit user that we just created in **_Step 3_**. Make sure to replace the `<userId>` parameter with the ID that was printed in the output from the previous step.
```Python
result = delete_cockpit_user(client, authorization, 'True', '<UserId>', 'DEMO')
print(result)
```

Run the entire program (make sure to delete or comment out the calling statements from the previous steps) and your output should be:
```
{"result":true}
```

[VALIDATE_4]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Appendix)]
Below is a **sample code** for all the Cockpit APIs (GET and POST ones). The code is written in Python. Feel free to copy and run it to examine how each API endpoint works.

```Python
import requests
import json
import urllib3

print("SAP HANA Cockpit API Samples\n----------------------------------------------")

# Interactive part - Prompt the user to enter his/her own information
HANA_HOST = input("Please enter the SAP HANA cockpit host name: ")
HANA_PORT = input("Please enter the port number for the app cockpit-adminui-svc: ")
HANA_USER = input("Please enter the SAP HANA cockpit user name: ")
HANA_PWD = input("Please enter the SAP HANA cockpit password: ")
service_key = input("Please enter the name of the service key file (must be located in the current folder): ")
print("----------------------------------------------")

# the URL "prefix"
baseURL = 'https://' + HANA_HOST + ':' + HANA_PORT


# Obtain the service key from the .json file (which contains the service key)
def getServiceKeyJSON(fileName):
    contents = open(fileName).read()
    return json.loads(contents)


# Obtain an OAuth token
def get_oauth_token(client):
    serviceKeyJSON = getServiceKeyJSON(service_key)

    oauthURI = serviceKeyJSON['url'] + '/oauth/token'
    oauthResponse = client.post(oauthURI, verify=False, data={
        'grant_type': 'password',
        'username': HANA_USER,
        'password': HANA_PWD,
        'client_id': serviceKeyJSON['clientid'],
        'client_secret': serviceKeyJSON['clientsecret']
    })

    authorization = 'Bearer ' + oauthResponse.json()['access_token']
    return authorization


def get_header(authorization):
    return {'Authorization': authorization,
            'Content-Type': 'application/json'}


## Endpoints (functions for all APIs)

## GET APIs
# List all resources that are visible to the customer
def list_cockpit_resources(client, authorization):
    targetURI = baseURL + '/resource/RegisteredResourcesGet'

    resourceListResponse = client.get(targetURI, verify=False, headers=get_header(authorization))
    return resourceListResponse.json()

# List all groups that are visible to the customer
def list_cockpit_groups(client, authorization, prefix_URL):
    targetURI = prefix_URL + '/group/GroupsForUserGet'

    groupListResponse = client.get(targetURI, verify=False, headers=get_header(authorization))
    return groupListResponse.json()

# List all resources in a specified group that is visible to the customer via group ID
def list_group_resources_id(client, authorization, groupId, prefix_URL):
    PARAMS = {'groupId': groupId}
    targetURI = prefix_URL + '/group/GroupResourcesGet'

    group_resourceListResponse = client.get(targetURI, verify=False, params=PARAMS, headers=get_header(authorization))
    return group_resourceListResponse.json()

# List all resources in a specified group that is visible to the customer via group designation
def list_group_resources_des(client, authorization, groupDesignation, prefix_URL):
    PARAMS = {'groupDesignation': groupDesignation}
    targetURI = prefix_URL + '/group/GroupResourcesGet'

    group_resourceListResponse = client.get(targetURI, verify=False, params=PARAMS, headers=get_header(authorization))
    return group_resourceListResponse.json()


## POST APIs
# Add a resource via instance number
def add_resource_via_instance(client, authorization, hostName, instanceNumber, techUser, techUserCredentials,\
                         databaseName, encryptJDBC, validateServerCertificate, hostNameInCertificate):
    if databaseName == None:
        data = {'hostName': hostName,
				'instanceNumber': instanceNumber,
				'techUser': techUser,
				'techUserCredentials': techUserCredentials,
				'isMultiTenant': False,
				'encryptJDBC': encryptJDBC,
			    'validateServerCertificate': validateServerCertificate,
                'hostNameInCertificate': hostNameInCertificate}

    else:
        data = {'hostName': hostName,
				'instanceNumber': instanceNumber,
				'techUser': techUser,
				'techUserCredentials': techUserCredentials,
				'databaseName': databaseName,
				'isMultiTenant': True,
				'encryptJDBC': encryptJDBC,
		    	'validateServerCertificate': validateServerCertificate,
                'hostNameInCertificate': hostNameInCertificate}

    targetURI = baseURL + '/registration/SystemRegister'
    resourceCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return resourceCreateResponse.json()

# Add a resource via port
def add_resource_via_port(client, authorization, hostName, port, techUser, techUserCredentials, encryptJDBC,\
						  validateServerCertificate, hostNameInCertificate):
    data = {'hostName': hostName,
            'port': port,
            'techUser': techUser,
            'techUserCredentials': techUserCredentials,
			'encryptJDBC': encryptJDBC,
			'validateServerCertificate': validateServerCertificate,
            'hostNameInCertificate': hostNameInCertificate}

    targetURI = baseURL + '/registration/SystemRegister'
    resourceCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return resourceCreateResponse.json()

# Delete a resource
def delete_cockpit_resource(client, authorization, resid):
    data = {'resid': resid}

    targetURI = baseURL + '/registration/ResourceUnregister'
    resourceDeleteResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return resourceDeleteResponse.text


# Add a group
def add_cockpit_group(client, authorization, groupName, groupDescription):
    data = {'groupName': groupName,
            'groupDescription': groupDescription}

    targetURI = baseURL + '/group/GroupCreate'
    groupCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return groupCreateResponse.json()

# Delete a group
def delete_cockpit_group(client, authorization, groupId):
    data = {'groupId': groupId}

    targetURI = baseURL + '/group/GroupDelete'
    groupDeleteResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return groupDeleteResponse.text

# Add a user
def add_cockpit_user(client, authorization, userName, userPWD, email, roleCollections):
    data = {'username': userName,
            'password': userPWD,
            'email': email,
            'roleCollections': roleCollections}

    targetURI = baseURL + '/user/CockpitUserCreate'
    userCreateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return userCreateResponse.json()

# Delete a user
def delete_cockpit_user(client, authorization, deleteFromUAA, userId, username):
    data = {'deleteFromUAA': deleteFromUAA,
            'userId': userId,
            'username': username}

    targetURI = baseURL + '/user/CockpitUserDelete'
    userDeleteResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return userDeleteResponse.text


# Add a resource to a group
def add_group_resource(client, authorization, groupId, resourceId):
    data = {'groupId': groupId,
            'resourceId': resourceId}

    targetURI = baseURL + '/group/GroupResourceAdd'
    resource_to_groupResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return resource_to_groupResponse.text #Error with return resource_to_groupResponse xxx.json() (but resource added successfully)

# Remove a resource from a group
def remove_group_resource(client, authorization, groupId, resourceId):
    data = {'groupId': groupId,
            'resourceId': resourceId}

    targetURI = baseURL + '/group/GroupResourceRemove'
    resourceRemoveResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return resourceRemoveResponse.text

# Add a user to a group
def add_group_user(client, authorization, groupId, userId):
    data = {'groupId': groupId,
            'userId': userId}

    targetURI = baseURL + '/group/GroupUserAdd'
    user_to_groupResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))

    return user_to_groupResponse.json()

# Remove a user from a group
def remove_group_user(client, authorization, groupId, userId):
    data = {'groupId': groupId,
            'userId': userId}

    targetURI = baseURL + '/group/GroupUserRemove'
    resourceRemoveResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return resourceRemoveResponse.text


# Print information in json style
def printJSON(title, jsonObj):
    print('\n' + title + '\n' + json.dumps(jsonObj, indent=4, sort_keys=True))



def main(): # launch the interactive program
    # disable warnings that dirty up the console app
    urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

    # get a handle on the client session
    client = requests.Session()

    # get the oauth token
    authorization = get_oauth_token(client)

    # Ask for the action (POST/GET) that the user wants to proceed
    while True:
        action = input("Please enter the type of SAP HANA cockpit APIs that you would like to call (POST/GET) or enter 'exit' to quit: ")

        if action == 'POST':
            print("The list of all POST APIs:\n\
            1. SystemRegister: registers an SAP HANA resource with the cockpit\n\
            2. ResourceUnregister: unregisters an SAP HANA resource\n\
            3. GroupCreate: creates a new resource group\n\
            4. GroupDelete: deletes a resource group\n\
            5. CockpitUserCreate: creates a new cockpit user\n\
            6. CockpitUserDelete: deletes a cockpit user\n\
            7. GroupResourceAdd: adds a resource to a group\n\
            8. GroupResourceRemove: removes a resource from a group\n\
            9. GroupUserAdd: adds a user to a group\n\
            10. GroupUserRemove: removes a user from a group.")
            api = int(input("Please enter the number of the Cockpit API which you would like to call: "))

            if api == 1:
                host = input("Please enter the host name of the SAP HANA resource: ")
                tech_user = input("Please enter the name of the technical user: ")
                tech_creds = input("Please enter the credentials for the corresponding technical user: ")
                encrypt_JDBC = input("Do you want to encrypt SAP start service connection (yes/no)? ")
                validateServerCertificate = input("Do you want to validate the server certificate (yes/no)? ")
                hostNameInCertificate = input("Please enter a hostname if you want to override the hostname in certificate (enter 'None' otherwise): ")

                if encrypt_JDBC == 'yes':
                    encrypt_JDBC = True
                else:
                    encrypt_JDBC = False
                if validateServerCertificate == 'yes':
                    validateServerCertificate = True
                    if hostNameInCertificate == 'None':
                        hostNameInCertificate = ''
                else:
                    validateServerCertificate = False
                    hostNameInCertificate = ''

                while True:
                    method = input("Please enter one of the two possible methods (instance number/port): ")
                    if method == 'instance number':
                        i_num = input("Please enter the instance number of the SAP HANA resource: ")
                        db_name = input("Please enter the name of the SAP HANA database: ")
                        result = add_resource_via_instance(client, authorization, host, i_num, tech_user,
                                                           tech_creds, db_name, encrypt_JDBC, validateServerCertificate,
                                                           hostNameInCertificate)
                        break
                    elif method == 'port':
                        port = input("Please enter the port number for the SAP HANA resource: ")
                        result = add_resource_via_port(client, authorization, host, port, tech_user, tech_creds,
                                                       encrypt_JDBC, validateServerCertificate, hostNameInCertificate)
                        break
                    else:
                        print("Failed to recognize your input. Please try again :)\n")

            elif api == 2:
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                result = delete_cockpit_resource(client, authorization, resource_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 3:
                group_name = input("Please enter the name of the resource group: ")
                group_description = input("Please enter the description of the resource group: ")
                result = add_cockpit_group(client, authorization, group_name, group_description)

            elif api == 4:
                group_id = input("Please enter the ID number of the resource group: ")
                result = delete_cockpit_group(client, authorization, group_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 5:
                user_name = input("Please enter the name of the cockpit user: ")
                user_pwd = input("Please create a password for the cockpit user: ")
                email = input("Please enter the email for the cockpit user: ")
                admin_bool = input("Would you like to assign the Cockpit administrator role to this user (yes/no)? ")
                resource_bool = input("Would you like to assign the Cockpit resource administrator role to this user (yes/no)? ")
                admin_user_bool = input("Would you like to assign the Cockpit user administrator role to this user (yes/no)? ")
                user_bool = input("Would you like to assign the Cockpit user role to this user (yes/no)? ")
                template_bool = input("Would you like to allow this user to manage system configuration templates (yes/no)? ")

                role_collections = []
                if admin_bool == 'yes':
                    role_collections.append('COCKPIT_ADMIN')
                if admin_user_bool == 'yes':
                    role_collections.append('COCKPIT_USER_ADMIN')
                if resource_bool == 'yes':
                    role_collections.append('COCKPIT_RESOURCE_ADMIN')
                if user_bool == 'yes':
                    role_collections.append('COCKPIT_USER')
                if template_bool == 'yes':
                    role_collections.append('COCKPIT_CONFIG_TEMPLATE_ADMIN')

                result = add_cockpit_user(client, authorization, user_name, user_pwd, email, role_collections)

            elif api == 6:
                user_id = input("Please enter the ID number of the cockpit user: ")
                user_name = input("Please enter the name of the cockpit user: ")
                while True:
                    delete_from_UAA = int(input("Please enter the number of one of the following options (1/2):\n\
                    1. Only remove cockpit access for this user.\n\
                    2. Delete the user completely.\n\
                    Input: "))
                    if delete_from_UAA == 1:
                        delete_from_UAA = False
                        break
                    elif delete_from_UAA == 2:
                        delete_from_UAA = True
                        break
                    else:
                        print("Your input is invalid. Please try again :)")
                result = delete_cockpit_user(client, authorization, delete_from_UAA, user_id, user_name)
                if result == '':
                    result = "SUCCESS"

            elif api == 7:
                group_id = input("Please enter the ID number of the destination resource group: ")
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                result = add_group_resource(client, authorization, group_id, resource_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 8:
                group_id = input("Please enter the ID number of the destination resource group: ")
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                result = remove_group_resource(client, authorization, group_id, resource_id)
                if result == 'true':
                    result = "SUCCESS"

            elif api == 9:
                group_id = input("Please enter the ID number of the destination resource group: ")
                user_id = input("Please enter the ID number of the cockpit user: ")
                result = add_group_user(client, authorization, group_id, user_id)
                if result == 'true':
                    result = "SUCCESS"

            elif api == 10:
                group_id = input("Please enter the ID number of the destination resource group: ")
                user_id = input("Please enter the ID number of the cockpit user: ")
                result = remove_group_user(client, authorization, group_id, user_id)
                if result == '':
                    result = "SUCCESS"

            else:
                print("Your input is invalid, please try again :)\n----------------------------------------------")
                continue

            printJSON("The result of your action is the following:\n", result)
            print("----------------------------------------------")


        elif action == 'GET':
            print("The list of all GET APIs:\n\
            1. RegisteredResourcesGet: returns information about the resources registered in SAP HANA cockpit\n\
            2. GroupsForUserGet: returns information about the resource groups that are visible to you\n\
            3. GroupResourcesGet: returns information about the resources in a specified group that is visible to you.")
            api = int(input("Please enter the number of the Cockpit API which you would like to call: "))

            if api == 1:
                cockpit_resources = list_cockpit_resources(client, authorization)
                printJSON("Registered Resources:\n", cockpit_resources)

            elif api == 2:
              landscape_port = input("Please enter your port number for the app cockpit-landscape-svc: ")
              # need this port specifically for this endpoint
              prefix_URL = 'https://' + HANA_HOST + ':' + landscape_port
              cockpit_groups = list_cockpit_groups(client, authorization, prefix_URL)
              printJSON('Resource Groups:\n', cockpit_groups)

            elif api == 3:
                while True:
                    method = input("What type of resource groups do you want to access (customized/automatic)? ")
                    landscape_port = input("Please enter your port number for the app cockpit-landscape-svc: ")
                    # need this port specifically for this endpoint
                    prefix_URL = 'https://' + HANA_HOST + ':' + landscape_port
                    if method == 'customized':
                        group_id = input("Please enter the ID number of the resource group which you would like to access: ")
                        group_resources = list_group_resources_id(client, authorization, group_id, prefix_URL)
                        printJSON("Resources for this customized resource group:\n", group_resources)
                        break
                    elif method == 'automatic':
                        group_des = input("Please enter the resource group designation which you would like to access (PRODUCTION, DEVELOPMENT, TEST, CUSTOM or ALL): ")
                        group_resources = list_group_resources_des(client, authorization, group_des, prefix_URL)
                        printJSON("Resources for this automatic resource group:\n", group_resources)
                        break
                    else:
                        print("Failed to recognized your input. Please try again :)")

            else:
                print("Your input is invalid, please try again :)")
                continue

            print("----------------------------------------------")

        elif action == "exit": # customer requests to exit
            break

        else: # customer entered some invalid input
            print("Your input is invalid, please try again :)\n----------------------------------------------")


# launch the program
main()

print("Thank you for using this program. See you next time :)")
```

[DONE]

[ACCORDION-END]

---
