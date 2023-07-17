---
parser: v2
auto_validation: true
primary_tag: software-product>sap-hana
tags: [  tutorial>beginner, software-product>sap-api-management, software-product>sap-hana ]
time: 25
---

# Call SAP HANA Cockpit POST APIs
<!-- description --> Introduce all the available SAP HANA cockpit POST APIs and the method to use them.

## Prerequisites  
 - Install the Python programming language on the computer that will call the endpoints

## You will learn  
  - What are the cockpit POST APIs that you can use
  - How to call cockpit POST APIs

---

## Intro
SAP HANA cockpit provides modifying (POST) REST API endpoints. Unlike the GET endpoints, the POST endpoints create, delete, or change objects in the cockpit.

There are nineteen cockpit POST API endpoints:

1. *`SystemRegister`*: registers an SAP HANA resource in the cockpit
2. *`SystemUnregister`*: `unregisters` an SAP HANA resource that's already registered
3. *`CockpitUserCreate`*: creates a new cockpit user
4. *`CockpitUserDelete`*: deletes a cockpit user
5. *`CockpitUserUpdate`*: updates an existing user (SSO option available)
6. *`CockpitUserRetrieve`*: returns a cockpit user's information with role collection
7. *`GroupCreate`*: creates a new resource group
8. *`GroupDelete`*: deletes a resource group
9.  *`GroupResourceAdd`*: adds a resource to a group
10. *`GroupResourceRemove`*: removes a resource from a group
11. *`GroupUserAdd`*: adds a user to a group
12. *`GroupUserRemove`*: removes a user from a group
13. *`GroupUpdate`*: updates the information of an existing group
14. *`ResourceSecurityUpdate`*: enables, disables, enforces resource SSO
15. *`ResourceUpdate`*: updates database name, description, owner name, owner email, and owner details
16. *`TechnicalUserStore`*: updates database technical user name and password
17. *`SSOKerberosUpdate`*: enables/disables SSO with Kerberos
18. *`RemoteCredentialsSet`*: stores credentials for the database user connecting to the monitored database (as displayed in Database Directory, uses `cockpit-landscape-svc`)
19. `SapControlUserSet`*: sets credentials for SAPControl access

> Only two of the nineteen cockpit POST APIs will be further explained in the following steps. To know more about the cockpit POST APIs, click [here](https://help.sap.com/docs/SAP_HANA_COCKPIT/afa922439b204e9caf22c78b6b69e4f2/a8aa6fdd1557450ea76cb691d7799ab1.html) to navigate to the **SAP Help Portal**.

> The **sample code** for all the cockpit APIs (GET and POST ones) is posted in **_Step 5_**. The code is written in Python and you are welcome to copy and run it to examine how each API works.

---

### Call the SystemRegister endpoint

This endpoint registers an SAP HANA resource in the cockpit. You have two options to register a cockpit resource: either by using its instance number or its port number.

[OPTION BEGIN [via Instance Number]]

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
[OPTION END]

[OPTION BEGIN [via Port Number]]

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
[OPTION END]


### Call the ResourceUnregister endpoint

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



### Call the CockpitUserCreate endpoint

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



### Call the CockpitUserDelete endpoint

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



### Appendix

Below is a **sample code** for all the Cockpit APIs (GET and POST ones). The code is written in Python. Feel free to copy and run it to examine how each API endpoint works.

```Python
import requests
import json
import urllib3

print("SAP HANA Cockpit API Samples\n----------------------------------------------")

# Interactive part - Prompt the user to enter his/her own information
HANA_HOST = input("Please enter the SAP HANA cockpit host name: ")
HANA_PORT = input("Please enter the port number for the app cockpit-adminui-svc: ")
LANDSCAPE_PORT = input("Please enter the port number for the app cockpit-landscape-svc: ")
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

# List all cockpit users in a specified group that are visible to the customer via group ID
def list_group_users(client, authorization, groupId):
    data = {'groupId': groupId}
    targetURI = baseURL + '/group/GroupUsersGet?groupId=' + groupId

    group_usersListResponse = client.get(targetURI, verify=False, headers=get_header(authorization))
    return group_usersListResponse.json()
    
# List all cockpit users that are visible to the customer
def list_cockpit_users(client, authorization):
    targetURI = baseURL + '/user/CockpitUsersGet'

    cockpit_usersListResponse = client.get(targetURI, verify=False, headers=get_header(authorization))
    return cockpit_usersListResponse.json()

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

    targetURI = baseURL + '/registration/SystemUnregister'
    resourceDeleteResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return resourceDeleteResponse.text

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

# Updates an existing user without SSO
def update_cockpit_user(client, authorization, username, password, givenName, familyName, email, roleCollections):
    data = {'username': username,
            'password': password,
            'givenName': givenName,
            'familyName': familyName,
            'email': email,
            'roleCollections': roleCollections}

    targetURI = baseURL + '/user/CockpitUserUpdate'
    cockpituserUpdateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return cockpituserUpdateResponse.json()

# Updates an existing user with SSO
def update_cockpit_user_sso(client, authorization, username, password, givenName, familyName, email, roleCollections, enable, externalId, user, credentials):
    data = {'username': username,
            'password': password,
            'givenName': givenName,
            'familyName': familyName,
            'email': email,
            'roleCollections': roleCollections,
            'kerberos': {'enable': enable,
                         'externalId': externalId,
                         'user': user,
                         'credentials': credentials}}

    targetURI = baseURL + '/user/CockpitUserUpdate'
    cockpituserUpdateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return cockpituserUpdateResponse.json()

# Returns a cockpit user's information with role collection
def retrieve_cockpit_user(client, authorization, username):
    data = {'username': username}

    targetURI = baseURL + '/user/CockpitUserRetrieve'
    cockpituserRetrieveResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return cockpituserRetrieveResponse.json()

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

# Update a group 
def update_cockpit_group(client, authorization, groupId, groupName, groupDescription):
    data = {'groupId': groupId,
            'groupName': groupName,
            'groupDescription': groupDescription}

    targetURI = baseURL + '/group/GroupUpdate'
    groupUpdateResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return groupUpdateResponse.json()

# Manage resource SSO 
def update_resource_security(client, authorization, enableSSO, enforceSSO, resid, trustAdminCredentials, trustAdminUser, encrypt, validateCertificate, hostNameInCertificate):
    data = {'enableSSO': enableSSO,
            'enforceSSO': enforceSSO,
            'resid': resid,
            'trustAdminCredentials': trustAdminCredentials,
            'trustAdminUser': trustAdminUser,
            'encrypt': encrypt,
            'validateCertificate': validateCertificate,
            'hostNameInCertificate': hostNameInCertificate}

    targetURI = baseURL + '/resource/ResourceSecurityUpdate'
    update_securityResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))  
    return update_securityResponse.json()

# Update database name, description, owner name, owner email, and owner details
def update_resource(client, authorization, resourceId, resourceName, resourceDescription, resourceOwnerName, resourceOwnerEmail, resourceOwnerDetails):
    data = {'resourceId': resourceId,
            'resourceName': resourceName,
            'resourceDescription': resourceDescription,
            'resourceOwnerName': resourceOwnerName,
            'resourceOwnerEmail': resourceOwnerEmail,
            'resourceOwnerDetails': resourceOwnerDetails}
    
    targetURI = baseURL + '/resource/ResourceUpdate'
    update_resourceResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return update_resourceResponse.json()

# Update database technical user name and password
def update_technical_user(client, authorization, resid, user, credentials):
    data = {'resid': resid,
            'user': user,
            'credentials': credentials}
    
    targetURI = baseURL + '/resource/TechnicalUserStore'
    update_technical_userResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return update_technical_userResponse.json()

# Enables/Disables SSO with Kerberos 
def update_kerberos(client, authorization, user, credentials, value):
    data = {'user': user,
            'credentials': credentials,
            'value': value}

    targetURI = baseURL + '/settings/SSOKerberosUpdate'
    updateKerberosResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return updateKerberosResponse.json()

# Store credentials for database for the calling application user
def set_remote_credentials(client, authorization, resid, user, credentials, use_sso, prefix_URL):
    data = {'resid': resid,
            'user': user,
            'use_sso': use_sso,
            'credentials': credentials}
    
    targetURI = prefix_URL + '/user/RemoteCredentialsSet'
    set_remote_credentials_userResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return set_remote_credentials_userResponse.json()   

# Sets credentials for SAPControl access
def set_SapControl(client, authorization, hostName, resid, sidCredentials, sidUser, prefix_URL):
    data = {'hostName': hostName,
            'resid': resid,
            'sidCredentials': sidCredentials,
            'sidUser': sidUser}

    targetURI = prefix_URL + '/user/SapControlUserSet'
    setSapControlUserResponse = client.post(targetURI, verify=False, json=data, headers=get_header(authorization))
    return setSapControlUserResponse.json() 

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
            2. SystemUnregister: unregisters an SAP HANA resource\n\
            3. CockpitUserCreate: creates a new cockpit user\n\
            4. CockpitUserDelete: deletes a cockpit user\n\
            5. CockpitUserUpdate: updates an existing user (SSO option available)\n\
            6. CockpitUserRetrieve: returns a cockpit user's information with role collection\n\
            7. GroupCreate: creates a new resource group\n\
            8. GroupDelete: deletes a resource group\n\
            9. GroupResourceAdd: adds a resource to a group\n\
            10. GroupResourceRemove: removes a resource from a group\n\
            11. GroupUserAdd: adds a user to a group\n\
            12. GroupUserRemove: removes a user from a group.\n\
            13. GroupUpdate: updates the information of an existing group\n\
            14. ResourceSecurityUpdate: enables, disables, enforces resource SSO\n\
            15. ResourceUpdate: updates database name, description, owner name, owner email, and owner details\n\
            16. TechnicalUserStore: updates database technical user name and password\n\
            17. SSOKerberosUpdate: enables/disables SSO with Kerberos\n\
            18. RemoteCredentialsSet: stores credentials for the database user connecting to the monitored database (as displayed in Database Directory\n\
            19. SapControlUserSet: sets credentials for SAPControl access).")

            api = int(input("Please enter the number of the Cockpit API which you would like to call: "))

            if api == 1: # SystemRegister
                host = input("Please enter the host name of the SAP HANA resource: ")
                tech_user = input("Please enter the name of the technical user: ")
                tech_creds = input("Please enter the password for the corresponding technical user: ")
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

            elif api == 2: # SystemUnregister
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                result = delete_cockpit_resource(client, authorization, resource_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 3: # CockpitUserCreate
                user_name = input("Please enter the user name of the cockpit user: ")
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

            elif api == 4: # CockpitUserDelete
                user_id = input("Please enter the ID number of the cockpit user: ")
                user_name = input("Please enter the user name of the cockpit user: ")
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

            elif api == 5: # CockpitUserUpdate
                user_name = input("Please enter the user name of the cockpit user: ")
                user_pwd = input("Please enter the new password for the cockpit user (simply press enter if you do not want to change): ")
                given_name = input("Please enter the new given name for the cockpit user: ")
                family_name = input("Please enter the new family name for the cockpit user: ")
                email = input("Please enter the new email for the cockpit user: ")
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

                while True:
                    kerberos_change = input("Would you like to change the Kerberos SSO settings of this user (yes/no)? ")

                    if kerberos_change == 'yes':
                        kerberos_change = True
                        break
                    elif kerberos_change == 'no':
                        kerberos_change = False
                        break
                    else:
                        print("Your input is invalid. Please try again :)")

                if kerberos_change == True:
                    enable_bool = input("Would you like to enable Kerberos SSO (yes/no)? ")
                    if enable_bool == 'yes':
                        enable_bool = True
                    else:
                        enable_bool = False

                    if enable_bool == True:
                        external_id = input("Please enter the external ID of Kerberos Realm: ")
                    else:
                        external_id = ""

                    manager_user = input("Please enter the SAP HANA cockpit administrator user name (e.g. COCKPIT_ADMIN): ")
                    manager_credentials = input("Please enter the SAP HANA cockpit administrator password: ")

                    result = update_cockpit_user_sso(client, authorization, user_name, user_pwd, given_name, 
                                                     family_name, email, role_collections, enable_bool, external_id, manager_user, manager_credentials)
                else:
                    result = update_cockpit_user(client, authorization, user_name, user_pwd, given_name, family_name, email, role_collections)

            elif api == 6: # CockpitUserRetrieve
                user_id = input("Please enter the user name of the cockpit user: ")

                result = retrieve_cockpit_user(client, authorization, user_id)
            
            elif api == 7: # GroupCreate
                group_name = input("Please enter the name of the resource group: ")
                group_description = input("Please enter the description of the resource group: ")
                result = add_cockpit_group(client, authorization, group_name, group_description)

            elif api == 8: # GroupDelete
                group_id = input("Please enter the ID number of the resource group: ")
                result = delete_cockpit_group(client, authorization, group_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 9: # GroupResourceAdd
                group_id = input("Please enter the ID number of the destination resource group: ")
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                result = add_group_resource(client, authorization, group_id, resource_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 10: # GroupResourceRemove
                group_id = input("Please enter the ID number of the destination resource group: ")
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                result = remove_group_resource(client, authorization, group_id, resource_id)
                if result == 'true':
                    result = "SUCCESS"

            elif api == 11: # GroupUserAdd
                group_id = input("Please enter the ID number of the destination resource group: ")
                user_id = input("Please enter the ID number of the cockpit user: ")
                result = add_group_user(client, authorization, group_id, user_id)
                if result == 'true':
                    result = "SUCCESS"

            elif api == 12: # GroupUserRemove
                group_id = input("Please enter the ID number of the destination resource group: ")
                user_id = input("Please enter the ID number of the cockpit user: ")
                result = remove_group_user(client, authorization, group_id, user_id)
                if result == '':
                    result = "SUCCESS"

            elif api == 13: # GroupUpdate
                group_id = input("Please enter the ID number of the destination resource group: ")
                group_name = input("Please enter the name of the resource group: ")
                group_description = input("Please enter the description of the resource group: ")

                result = update_cockpit_group(client, authorization, group_id, group_name, group_description)

            elif api == 14: # ResourceSecurityUpdate
                enable_sso = input("Do you want to enable SSO (yes/no)? ")
                enforce_sso = input("Do you want to enforce SSO (yes/no)? ")
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                admin_user = input("Please enter the user name of the database user with TRUST ADMIN Privilege: ")
                admin_creds = input("Please enter the password for the corresponding database user: ")
                encrypt = input("Do you want to encrypt SAP start service connection (yes/no)? ")
                validateServerCertificate = input("Do you want to validate the server certificate (yes/no)? ")
                hostNameInCertificate = input("Please enter a hostname if you want to override the hostname in certificate (enter 'None' otherwise): ")

                if enable_sso == 'yes':
                    enable_sso = True
                else:
                    enable_sso = False
                if enforce_sso == 'yes':
                    enforce_sso = True
                else:
                    enforce_sso = False
                if encrypt == 'yes':
                    encrypt = True
                else:
                    encrypt = False
                if validateServerCertificate == 'yes':
                    validateServerCertificate = True
                else:
                    validateServerCertificate = False
                if hostNameInCertificate == 'None':
                    hostNameInCertificate = ''

                result = update_resource_security(client, authorization, enable_sso, enforce_sso, resource_id, admin_creds, admin_user, encrypt, validateServerCertificate, hostNameInCertificate)
           
            elif api == 15: # ResourceUpdate
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                resource_name = input("Please enter the new resource name (enter 'None' to keep the same name): ")
                resource_description = input("Please enter the new resource description (enter 'None' to set it blank): ")
                resource_owner_name = input("Please enter the new resource owner name (enter 'None' to set it blank): ")
                resource_owner_email = input("Please enter the new resource owner email (enter 'None' to set it blank): ")
                resource_owner_details = input("Please enter the new resource owner details (enter 'None' to set it blank): ")

                if resource_name == 'None':
                    resource_name = ''
                if resource_description == 'None':
                   resource_description = ''
                if resource_owner_name == 'None':
                    resource_owner_name = ''
                if resource_owner_email == 'None':
                    resource_owner_email = ''
                if resource_owner_details == 'None':
                    resource_owner_details = ''

                result = update_resource(client, authorization, resource_id, resource_name, resource_description, resource_owner_name, resource_owner_email, resource_owner_details)

            elif api == 16: # TechnicalUserStore
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                tech_user = input("Please enter the user name of the technical user: ")
                tech_creds = input("Please enter the password for the corresponding technical user: ")

                result = update_technical_user(client, authorization, resource_id, tech_user, tech_creds)

            elif api == 17: # SSOKerberosUpdate
                app_user = input("Please enter the user name of the database application user: ")
                app_creds = input("Please enter the password for the corresponding application user: ")
                enable_sso_with_kerberos = input("Do you want to enable SSO with Kerberos (yes/no)? ")

                if enable_sso_with_kerberos == 'yes':
                    enable_sso_with_kerberos = True
                    print("Enabling SSO with Kerberos.")
                else:
                    enable_sso_with_kerberos = False
                    print("Disabling SSO with Kerberos.")

                result = update_kerberos(client, authorization, app_user, app_creds, enable_sso_with_kerberos)

            elif api == 18: # RemoteCredentialsSet
                # need landscape port specifically for this endpoint
                prefix_URL = 'https://' + HANA_HOST + ':' + LANDSCAPE_PORT

                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                app_user = input("Please enter the user name of the database application user: ")
                app_creds = input("Please enter the password for the corresponding application user: ")

                result = set_remote_credentials(client, authorization, resource_id, app_user, app_creds, prefix_URL)

            elif api == 19: # SapControlUserSet
                # need landscape port specifically for this endpoint
                prefix_URL = 'https://' + HANA_HOST + ':' + LANDSCAPE_PORT

                host_name = input("Please enter the hostname of the SAP HANA resource: ")
                resource_id = input("Please enter the ID number of the SAP HANA resource: ")
                app_user = input("Please enter the user name of the database application user: ")
                app_creds = input("Please enter the password for the corresponding application user: ")

                result = set_SapControl(client, authorization, host_name, resource_id, app_creds, app_user)

            else:
                print("Your input is invalid, please try again :)\n----------------------------------------------")
                continue

            printJSON("The result of your action is the following:\n", result)
            print("----------------------------------------------")


        elif action == 'GET':
            print("The list of all GET APIs:\n\
            1. RegisteredResourcesGet: returns information about the resources registered in SAP HANA cockpit\n\
            2. GroupsForUserGet: returns information about the resource groups that are visible to you\n\
            3. GroupResourcesGet: returns information about the resources in a specified group that is visible to you\n\
            4. GroupUsersGet: returns information about the cockpit users in SAP HANA cockpit Database Group\n\
            5. CockpitUsersGet: returns information about cockpit users.")
            api = int(input("Please enter the number of the Cockpit API which you would like to call: "))

            if api == 1:
                cockpit_resources = list_cockpit_resources(client, authorization)
                printJSON("Registered Resources:\n", cockpit_resources)

            elif api == 2:
              # need landscape port specifically for this endpoint
              prefix_URL = 'https://' + HANA_HOST + ':' + LANDSCAPE_PORT
              cockpit_groups = list_cockpit_groups(client, authorization, prefix_URL)
              printJSON('Resource Groups:\n', cockpit_groups)

            elif api == 3:
                while True:
                    method = input("What type of resource groups do you want to access (customized/automatic)? ")
                    # need landscape port specifically for this endpoint
                    prefix_URL = 'https://' + HANA_HOST + ':' + LANDSCAPE_PORT
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

            elif api == 4:
                group_id = input("Please enter the ID number of the resource group which you would like to access: ")
                group_users = list_group_users(client, authorization, group_id)
                printJSON("Group users for this customized resource group:\n", group_users)

            elif api == 5:
                cockpit_users = list_cockpit_users(client, authorization)
                printJSON("Cockpit users:\n", cockpit_users)


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


---
