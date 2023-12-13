---
parser: v2
auto_validation: true
time: 15
tags: [tutorial>intermediate, software-product-function>sap-hana-cloud--sap-hana-database, software-product-function>sap-hana-cloud--data-lake, software-product-function>sap-automation-pilot]
primary_tag: software-product>sap-hana-cloud
---

# Automating SAP HANA Cloud Tasks with the SAP Automation Pilot Service
<!-- description --> Learn how to use the SAP Automation Pilot service to execute administrative tasks for an SAP HANA Cloud instance.  

## Prerequisites
- An SAP BTP account
- An SAP HANA Cloud instance

## You will learn
  - About the catalogs provided in SAP Automation Pilot relating to SAP HANA Cloud
  - How to import a catalog containing examples of automation commands
  - How to edit inputs and run the commands from the imported catalog

---

### Learn about the SAP Automation Pilot
The [SAP Automation Pilot](https://help.sap.com/docs/automation-pilot/automation-pilot/what-is-sap-automation-pilot) is a service within the SAP BTP that can be used to schedule and automate tasks.  

![Subscribe to the service](subscribe.png)

Once subscribed to the service, the tool can be opened by clicking on the application link.

![subscribed to the service](subscribed.png)

It provides a visual tool for constructing commands.

![Example command](command.png)

The SAP Automation Pilot provides the following catalogs to aid in managing an SAP HANA Cloud instance:

  * **sm-sapcp** contains commands to perform tasks within a BTP subaccount.

      ![sm-sapcp catalog](sm-sapcp.png)

  * **dblm-sapcp** contains commands to start, stop, or upgrade an SAP HANA Cloud instance provisioned in Cloud Foundry.

    ![dblm-sapcp catalog](dblm-sapcp.png)

  * **cf-sapcp** contains commands to perform tasks within Cloud Foundry.

      ![cf-sapcp catalog](cf-sapcp.png)

In addition, SAP Automation Pilot provides an option to schedule commands and provides integration with the SAP Alert Notification service which is used to send an email or create an incident in various ticketing systems.  If you do not already have the SAP Automation Pilot configured, step 1 of the tutorial [Take Action Following a SAP HANA Cloud Database Alert with SAP Automation Pilot](hana-cloud-alerts-autopilot) and the tutorial [Get Started with SAP Automation Pilot](https://developers.sap.com/tutorials/automation-pilot-1.html) demonstrate how to get started.

### Import the automation catalog
The catalog below contains commands and inputs for those commands that can be used to automate tasks within SAP HANA Cloud instances.

1. Import the catalog below into the SAP Automation Pilot.  

    ![import catalog](import.png)

    ```JSON
    {
      "id": "Automation-<<<TENANT_ID>>>",
      "technicalName": "Automation",
      "name": "Automation",
      "description": "Collection of commands to demonstrate automating tasks in SAP HANA Cloud",
      "owner": "<<<TENANT_ID>>>",
      "inputs": [
        {
          "id": "Automation-<<<TENANT_ID>>>:CFInstanceDetails:1",
          "name": "CFInstanceDetails",
          "description": "The details of an SAP HANA Cloud instance provisioned in Cloud Foundry",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "resourceGroup": {
              "type": "string",
              "sensitive": false,
              "description": "Cloud Foundry space name (dev)"
            },
            "resourceName": {
              "type": "string",
              "sensitive": false,
              "description": "The name of an instance (HC_HDB_CF)"
            },
            "region": {
              "type": "string",
              "sensitive": false,
              "description": "cf- value from API Endpoint (cf-us10-001)"
            },
            "subAccount": {
              "type": "string",
              "sensitive": false,
              "description": "The Cloud Foundry org name (2fb68c96trial)"
            }
          },
          "values": {
            "resourceGroup": "dev",
            "resourceName": "HC_HDB_CF",
            "region": "cf-us10-001",
            "subAccount": "2fb68c96trial"
          },
          "tags": {}
        },
        {
          "id": "Automation-<<<TENANT_ID>>>:Description:1",
          "name": "Description",
          "description": "Update description JSON for an SAP HANA Cloud instance",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "parameters": {
              "type": "object",
              "sensitive": false,
              "description": "JSON parameter to update its description"
            }
          },
          "values": {
            "parameters": "{\n    \"metadata\": {\n        \"ui.hc.sap.com/description\": \"Updated by the SAP Automation Pilot\"\n    }\n}"
          },
          "tags": {}
        },
        {
          "id": "Automation-<<<TENANT_ID>>>:CFTechnicalUser:1",
          "name": "CFTechnicalUser",
          "description": "A user that has space developer role in a Cloud Foundry space",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "password": {
              "type": "string",
              "sensitive": true,
              "description": ""
            },
            "user": {
              "type": "string",
              "sensitive": false,
              "description": "A Cloud Foundry user"
            }
          },
          "values": {
            "user": "",
            "password": ""
          },
          "tags": {}
        },
        {
          "id": "Automation-<<<TENANT_ID>>>:Start:1",
          "name": "Start",
          "description": "Start JSON for an SAP HANA Cloud instance",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "parameters": {
              "type": "object",
              "sensitive": false,
              "description": "JSON parameter to start an SAP HANA Cloud instance"
            }
          },
          "values": {
            "parameters": "{\n    \"data\": {\n        \"serviceStopped\": false\n    }\n}"
          },
          "tags": {}
        },
        {
          "id": "Automation-<<<TENANT_ID>>>:Upgrade:1",
          "name": "Upgrade",
          "description": "Upgrade JSON for an SAP HANA Cloud instance",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "parameters": {
              "type": "object",
              "sensitive": false,
              "description": "JSON parameter to upgrade an instance in the subaccount"
            }
          },
          "values": {
            "parameters": "{\n   \"data\": {\n     \"productVersion\": {\n         \"releaseCycle\":\"generally-available-quarterly\",\n         \"track\": \"2023.28\",\n         \"id\": \"2023.28.7\"\n     }\n   }\n}\n"
          },
          "tags": {}
        },
        {
          "id": "Automation-<<<TENANT_ID>>>:Stop:1",
          "name": "Stop",
          "description": "Stop JSON for an SAP HANA Cloud instance",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "parameters": {
              "type": "object",
              "sensitive": false,
              "description": "JSON parameter to stop an SAP HANA Cloud instance"
            }
          },
          "values": {
            "parameters": "{\n    \"data\": {\n        \"serviceStopped\": true\n    }\n}"
          },
          "tags": {}
        },
        {
          "id": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
          "name": "InstanceDetails",
          "description": "The details of an SAP HANA Cloud Instance deployed to an SAP BTP subaccount",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "owner": null,
          "version": 1,
          "keys": {
            "instanceId": {
              "type": "string",
              "sensitive": false,
              "description": "The instance ID"
            },
            "instanceName": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "serviceKey": {
              "type": "object",
              "sensitive": false,
              "description": null
            },
            "servicePlanId": {
              "type": "string",
              "sensitive": false,
              "description": "Required for MassUpgradeHC when attempting to get a  list of all the SAP HANA Cloud database instances in a subaccount.  Obtained via btp list services/plan --fields-filter \"name contains 'hana'\" "
            }
          },
          "values": {
            "instanceId": "be0bdfe8-e0a3-468b-84a6-b1ba14b5f5ce",
            "instanceName": "HC_HDB",
            "serviceKey": "{\n  \"clientid\": \"sb-aed8de6f-2401-4e27-8db3-15c00ad34a5d!b2030|service-manager!b16\",\n  \"clientsecret\": \"5bb27450-7089-4a44-9168-bac14fc4425e$H6CK7PbCfpRfW30xT_lN6FypTZEaDRnb9tCsXM6xIX4=\",\n  \"sm_url\": \"https://service-manager.cfapps.ca10.hana.ondemand.com\",\n  \"url\": \"https://dansftsubaccount.authentication.ca10.hana.ondemand.com\",\n  \"xsappname\": \"aed8de6f-2401-4e27-8db3-15c00ad34a5d!b2030|service-manager!b16\"\n}",
            "servicePlanId": "e573479c-39f8-4774-80a9-12d762b0f159"
          },
          "tags": {}
        }
      ],
      "commands": [
        {
          "configuration": {
            "values": [
              {
                "alias": "CFUser",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFTechnicalUser:1",
                  "inputKey": null
                }
              },
              {
                "alias": "CFInstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFInstanceDetails:1",
                  "inputKey": null
                }
              }
            ],
            "output": {},
            "executors": [
              {
                "execute": "dblm-sapcp:StopHanaCloudInstance:1",
                "input": {
                  "resourceGroup": "$(.CFInstanceDetails.resourceGroup)",
                  "password": "$(.CFUser.password)",
                  "resourceName": "$(.CFInstanceDetails.resourceName)",
                  "region": "$(.CFInstanceDetails.region)",
                  "user": "$(.CFUser.user)",
                  "subAccount": "$(.CFInstanceDetails.subAccount)"
                },
                "alias": "CFStopHC",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:CFStopHC:1",
          "name": "CFStopHC",
          "description": "Stop an SAP HANA Cloud Instance provisioned in Cloud Foundry",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {},
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "CFUser",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFTechnicalUser:1",
                  "inputKey": null
                }
              },
              {
                "alias": "CFInstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFInstanceDetails:1",
                  "inputKey": null
                }
              }
            ],
            "output": {},
            "executors": [
              {
                "execute": "dblm-sapcp:StartHanaCloudInstance:1",
                "input": {
                  "resourceGroup": "$(.CFInstanceDetails.resourceGroup)",
                  "password": "$(.CFUser.password)",
                  "resourceName": "$(.CFInstanceDetails.resourceName)",
                  "region": "$(.CFInstanceDetails.region)",
                  "user": "$(.CFUser.user)",
                  "subAccount": "$(.CFInstanceDetails.subAccount)"
                },
                "alias": "CFStartHC",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:CFStartHC:1",
          "name": "CFStartHC",
          "description": "Start an SAP HANA Cloud Instance provisioned in Cloud Foundry",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {},
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "InstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
                  "inputKey": null
                }
              },
              {
                "alias": "start",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:Start:1",
                  "inputKey": null
                }
              }
            ],
            "output": {},
            "executors": [
              {
                "execute": "sm-sapcp:UpdateServiceInstance:1",
                "input": {
                  "instanceId": "$(.InstanceDetails.instanceId)",
                  "displayName": "$(.InstanceDetails.instanceName)",
                  "serviceKey": "$(.InstanceDetails.serviceKey)",
                  "parameters": "$(.start.parameters)"
                },
                "alias": "update",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:StartHC:1",
          "name": "StartHC",
          "description": "Start an SAP HANA Cloud instance provisioned to an SAP BTP subaccount",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {},
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "InstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
                  "inputKey": null
                }
              },
              {
                "alias": "Upgrade",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:Upgrade:1",
                  "inputKey": null
                }
              }
            ],
            "output": {
              "result": "$(.upgrade.output.outputs[])",
              "instancesFound": "$(.getInstances.output.serviceInstances | map({name: .name,  env: .context.env_type, instanceId: .id, service_plan: .service_plan_id}))",
              "instanceList": "$(.getInstances.output.serviceInstances)"
            },
            "executors": [
              {
                "execute": "sm-sapcp:ListServiceInstances:1",
                "input": {
                  "fieldSelector": "service_plan_id eq '$(.InstanceDetails.servicePlanId)' and context/env_type eq 'sapcp'",
                  "serviceKey": "$(.InstanceDetails.serviceKey)"
                },
                "alias": "getInstances",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": {
                  "semantic": "OR",
                  "conditions": [
                    {
                      "semantic": "OR",
                      "cases": [
                        {
                          "expression": "$(.getInstances.output.totalResultsCount) ",
                          "operator": "GREATER_THAN",
                          "semantic": "OR",
                          "values": [
                            "0"
                          ]
                        }
                      ]
                    }
                  ]
                },
                "autoRetry": null,
                "repeat": null,
                "errorMessages": [
                  {
                    "message": "No instances found to update",
                    "when": {
                      "semantic": "OR",
                      "conditions": [
                        {
                          "semantic": "OR",
                          "cases": [
                            {
                              "expression": "$(.execution.error.originalMessage)",
                              "operator": "STARTS_WITH",
                              "semantic": "OR",
                              "values": [
                                "Validation"
                              ]
                            }
                          ]
                        }
                      ]
                    }
                  }
                ]
              },
              {
                "execute": "utils-sapcp:ForEach:1",
                "input": {
                  "inputs": "$(.getInstances.output.serviceInstances | map({ instanceId: .id })) ",
                  "defaultValues": "{ \"parameters\": $(.Upgrade.parameters), \"serviceKey\": $(.InstanceDetails.serviceKey) }",
                  "command": "sm-sapcp:TriggerUpdateServiceInstance:1"
                },
                "alias": "upgrade",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:MassUpgradeHC:1",
          "name": "MassUpgradeHC",
          "description": "Update the description of an SAP HANA Cloud instance provisioned to an SAP BTP subaccount",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {
            "result": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "instancesFound": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "instanceList": {
              "type": "string",
              "sensitive": false,
              "description": null
            }
          },
          "tags": {
            "feature:priority": "medium"
          }
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "InstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
                  "inputKey": null
                }
              },
              {
                "alias": "description",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:Description:1",
                  "inputKey": null
                }
              }
            ],
            "output": {
              "CommandOutput": "$(.update.output.serviceInstance)"
            },
            "executors": [
              {
                "execute": "sm-sapcp:UpdateServiceInstance:1",
                "input": {
                  "instanceId": "$(.InstanceDetails.instanceId)",
                  "displayName": "$(.InstanceDetails.instanceName)",
                  "serviceKey": "$(.InstanceDetails.serviceKey)",
                  "parameters": "$(.description.parameters)"
                },
                "alias": "update",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:UpdateHC:1",
          "name": "UpdateHC",
          "description": "Update the description of an SAP HANA Cloud instance provisioned to an SAP BTP subaccount",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {
            "CommandOutput": {
              "type": "string",
              "sensitive": false,
              "description": ""
            }
          },
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "CFUser",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFTechnicalUser:1",
                  "inputKey": null
                }
              },
              {
                "alias": "CFInstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFInstanceDetails:1",
                  "inputKey": null
                }
              }
            ],
            "output": {
              "instanceParameters": "$(.getDetails.output.parameters) "
            },
            "executors": [
              {
                "execute": "cf-sapcp:GetCfServiceInstance:1",
                "input": {
                  "password": "$(.CFUser.password)",
                  "org": "$(.CFInstanceDetails.subAccount)",
                  "serviceInstance": "$(.CFInstanceDetails.resourceName)",
                  "region": "$(.CFInstanceDetails.region)",
                  "user": "$(.CFUser.user)",
                  "includeParameters": "true",
                  "space": "$(.CFInstanceDetails.resourceGroup)"
                },
                "alias": "getDetails",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:CFGetInstanceParameters:1",
          "name": "CFGetInstanceParameters",
          "description": "Get details on available upgrades for an SAP HANA Cloud Instance provisioned in Cloud Foundry",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {
            "instanceParameters": {
              "type": "string",
              "sensitive": false,
              "description": null
            }
          },
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "CFUser",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFTechnicalUser:1",
                  "inputKey": null
                }
              },
              {
                "alias": "CFInstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFInstanceDetails:1",
                  "inputKey": null
                }
              }
            ],
            "output": {
              "availableUpgradeVersions": "$(.getParams.output.parameters.availableUpgradeVersions)",
              "appliedVersion": "$(.generateJSON.output.output[0])",
              "previousProductVersion": "$(.getParams.output.parameters.currentProductVersion)"
            },
            "executors": [
              {
                "execute": "cf-sapcp:GetCfServiceInstance:1",
                "input": {
                  "password": "$(.CFUser.password)",
                  "org": "$(.CFInstanceDetails.subAccount)",
                  "serviceInstance": "$(.CFInstanceDetails.resourceName)",
                  "region": "$(.CFInstanceDetails.region)",
                  "user": "$(.CFUser.user)",
                  "includeParameters": "true",
                  "space": "$(.CFInstanceDetails.resourceGroup)"
                },
                "alias": "getParams",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": {
                  "semantic": "OR",
                  "conditions": [
                    {
                      "semantic": "OR",
                      "cases": [
                        {
                          "expression": "$(.getParams.output.parameters.availableUpgradeVersions) | length",
                          "operator": "GREATER_THAN",
                          "semantic": "OR",
                          "values": [
                            "0"
                          ]
                        }
                      ]
                    }
                  ]
                },
                "autoRetry": null,
                "repeat": null,
                "errorMessages": [
                  {
                    "message": "No patches or updates found",
                    "when": {
                      "semantic": "OR",
                      "conditions": [
                        {
                          "semantic": "OR",
                          "cases": [
                            {
                              "expression": "$(.execution.error.originalMessage)",
                              "operator": "STARTS_WITH",
                              "semantic": "OR",
                              "values": [
                                "Validation"
                              ]
                            }
                          ]
                        }
                      ]
                    }
                  }
                ]
              },
              {
                "execute": "scripts-sapcp:ExecuteScript:2",
                "input": {
                  "stdin": "$(.getParams.output.parameters)",
                  "script": "#!/usr/bin/env python3\n\nimport json\nimport sys\n\ninput = sys.stdin.read()\nparameters = json.loads(input)\n\n#There is only ever the latest patch per QRC\n#There can be up to two QRCs\n#So at most there could be two patches and two QRCs\n#For simplicity, select the first entry in the list of availableUpgradeVersions\n#Logic could be added here to: \n#  favour QRC upgrades over patches or vice versa\n#  Check the expirationDate of the currentProductVersion\n\nversionParameter= {'data': {'productVersion': {'releaseCycle':'generally-available-quarterly', 'track': parameters['availableUpgradeVersions'][0]['track'], 'id': parameters['availableUpgradeVersions'][0]['id']} } }\n\nprint(json.dumps(versionParameter))"
                },
                "alias": "generateJSON",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              },
              {
                "execute": "cf-sapcp:UpdateCfServiceInstance:1",
                "input": {
                  "password": "$(.CFUser.password) ",
                  "org": "$(.CFInstanceDetails.subAccount) ",
                  "serviceInstance": "$(.CFInstanceDetails.resourceName) ",
                  "region": "$(.CFInstanceDetails.region) ",
                  "user": "$(.CFUser.user)",
                  "parameters": "$(.generateJSON.output.output[0])",
                  "space": "$(.CFInstanceDetails.resourceGroup) "
                },
                "alias": "upgrade",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:CFUpgradeHC:1",
          "name": "CFUpgradeHC",
          "description": "Upgrade an SAP HANA Cloud Instance provisioned in Cloud Foundry",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {
            "availableUpgradeVersions": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "appliedVersion": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "previousProductVersion": {
              "type": "string",
              "sensitive": false,
              "description": null
            }
          },
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "CFUser",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFTechnicalUser:1",
                  "inputKey": null
                }
              },
              {
                "alias": "CFInstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:CFInstanceDetails:1",
                  "inputKey": null
                }
              },
              {
                "alias": "description",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:Description:1",
                  "inputKey": null
                }
              }
            ],
            "output": {},
            "executors": [
              {
                "execute": "cf-sapcp:UpdateCfServiceInstance:1",
                "input": {
                  "password": "$(.CFUser.password)",
                  "org": "$(.CFInstanceDetails.subAccount)",
                  "serviceInstance": "$(.CFInstanceDetails.resourceName)",
                  "region": "$(.CFInstanceDetails.region)",
                  "deadline": "30",
                  "user": "$(.CFUser.user)",
                  "parameters": "$(.description.parameters)",
                  "space": "$(.CFInstanceDetails.resourceGroup)"
                },
                "alias": "CFUpdateHC",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:CFUpdateHC:1",
          "name": "CFUpdateHC",
          "description": "Update the description of an SAP HANA Cloud Instance provisioned in Cloud Foundry",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {},
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "InstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
                  "inputKey": null
                }
              },
              {
                "alias": "stop",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:Stop:1",
                  "inputKey": null
                }
              }
            ],
            "output": {},
            "executors": [
              {
                "execute": "sm-sapcp:UpdateServiceInstance:1",
                "input": {
                  "instanceId": "$(.InstanceDetails.instanceId)",
                  "displayName": "$(.InstanceDetails.instanceName)",
                  "serviceKey": "$(.InstanceDetails.serviceKey)",
                  "parameters": "$(.stop.parameters)"
                },
                "alias": "update",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:StopHC:1",
          "name": "StopHC",
          "description": "Stop an SAP HANA Cloud instance provisioned to an SAP BTP subaccount",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {},
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "InstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
                  "inputKey": null
                }
              }
            ],
            "output": {
              "CommandOutput": "$(.getDetails.output.parameters)"
            },
            "executors": [
              {
                "execute": "sm-sapcp:GetServiceInstanceParameters:1",
                "input": {
                  "instanceId": "$(.InstanceDetails.instanceId)",
                  "serviceKey": "$(.InstanceDetails.serviceKey)"
                },
                "alias": "getDetails",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:GetInstanceParameters:1",
          "name": "GetInstanceParameters",
          "description": "Get the instance parameter details",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {
            "CommandOutput": {
              "type": "object",
              "sensitive": false,
              "description": "Instance parameters"
            }
          },
          "tags": {}
        },
        {
          "configuration": {
            "values": [
              {
                "alias": "InstanceDetails",
                "valueFrom": {
                  "inputReference": "Automation-<<<TENANT_ID>>>:InstanceDetails:1",
                  "inputKey": null
                }
              }
            ],
            "output": {
              "availableUpgradeVersions": "$(.getParams.output.CommandOutput.availableUpgradeVersions)",
              "appliedVersion": "$(.generateJSON.output.output[0])",
              "previousProductVersion": "$(.getParams.output.CommandOutput.currentProductVersion)"
            },
            "executors": [
              {
                "execute": "Automation-<<<TENANT_ID>>>:GetInstanceParameters:1",
                "input": {},
                "alias": "getParams",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": {
                  "semantic": "OR",
                  "conditions": [
                    {
                      "semantic": "OR",
                      "cases": [
                        {
                          "expression": "$(.getParams.output.CommandOutput.availableUpgradeVersions) | length",
                          "operator": "GREATER_THAN",
                          "semantic": "OR",
                          "values": [
                            "0"
                          ]
                        }
                      ]
                    }
                  ]
                },
                "autoRetry": null,
                "repeat": null,
                "errorMessages": [
                  {
                    "message": "No patches or updates found",
                    "when": {
                      "semantic": "OR",
                      "conditions": [
                        {
                          "semantic": "OR",
                          "cases": [
                            {
                              "expression": "$(.execution.error.originalMessage)",
                              "operator": "STARTS_WITH",
                              "semantic": "OR",
                              "values": [
                                "Validation"
                              ]
                            }
                          ]
                        }
                      ]
                    }
                  }
                ]
              },
              {
                "execute": "scripts-sapcp:ExecuteScript:2",
                "input": {
                  "stdin": "$(.getParams.output.CommandOutput)",
                  "script": "#!/usr/bin/env python3\n\nimport json\nimport sys\n\ninput = sys.stdin.read()\nparameters = json.loads(input)\n\n#There is only ever the latest patch per QRC\n#There can be up to two QRCs\n#So at most there could be two patches and two QRCs\n#For simplicity, select the first entry in the list of availableUpgradeVersions\n#Logic could be added here to: \n#  favour QRC upgrades over patches or vice versa\n#  Check the expirationDate of the currentProductVersion\n\nversionParameter= {'data': {'productVersion': {'releaseCycle':'generally-available-quarterly', 'track': parameters['availableUpgradeVersions'][0]['track'], 'id': parameters['availableUpgradeVersions'][0]['id']} } }\n\nprint(json.dumps(versionParameter))"
                },
                "alias": "generateJSON",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              },
              {
                "execute": "sm-sapcp:UpdateServiceInstance:1",
                "input": {
                  "instanceId": "$(.InstanceDetails.instanceId)",
                  "serviceKey": "$(.InstanceDetails.serviceKey)",
                  "deadline": "30",
                  "parameters": "$(.generateJSON.output.output[0])"
                },
                "alias": "upgrade",
                "progressMessage": null,
                "initialDelay": null,
                "pause": null,
                "when": null,
                "validate": null,
                "autoRetry": null,
                "repeat": null,
                "errorMessages": []
              }
            ],
            "listeners": []
          },
          "id": "Automation-<<<TENANT_ID>>>:UpgradeHC:1",
          "name": "UpgradeHC",
          "description": "Upgrade an SAP HANA Cloud instance provisioned to an SAP BTP subaccount",
          "catalog": "Automation-<<<TENANT_ID>>>",
          "version": 1,
          "inputKeys": {},
          "outputKeys": {
            "availableUpgradeVersions": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "appliedVersion": {
              "type": "string",
              "sensitive": false,
              "description": null
            },
            "previousProductVersion": {
              "type": "string",
              "sensitive": false,
              "description": null
            }
          },
          "tags": {}
        }
      ]
    }
    ```

    The catalog will appear under **My Catalogs** and is named **Automation**.

    ![my catalog](my-catalog.png)

    >Should you wish to delete this catalog, perhaps to import a newer version, press the Edit Multiple button for both commands and inputs as a non empty catalog cannot be deleted.

2. Examine the imported commands and inputs.

    ![Commands](commands.png)

    The commands and inputs that start with CF are for instances that were provisioned to Cloud Foundry.  

    ![Inputs](inputs.png)

Further examples can be found at [automation pilot examples](https://github.com/SAP-samples/automation-pilot-examples).

### Execute commands for instances provisioned to the subaccount
The examples shown include commands to start, stop, update, and upgrade an SAP HANA Cloud instance.  The commands make use of a common input parameter that specifies the instance to target.  

1. Modify the input key **InstanceDetails** to match the SAP HANA Cloud instance that you wish to work with.

    ![Update the input instance details](subaccount-inputs.png)

    The serviceKey value comes from a binding on a service manager instance that was created in the previous [tutorial](hana-cloud-automation-rest).

2. The commands StartHC and StopHC can be used to start and stop a SAP HANA Cloud instance.

    * Open the command **StartHC**.

        ![inputs for the start command](start-inputs.png)

    * Examine the **Additional Values** section for the **input**.  Notice that it has as its inputs **InstanceDetails** which describes which SAP HANA Cloud instance to target and **Start** which contains the JSON that specifies how to update the serviceStopped state.

        ![serviceStopped JSON parmameter](start.png)

    * Examine the executor named **update**.  Notice that it makes use of one of the provided commands `sm-sapcp:UpdateServiceInstance` and that its inputs are taken from inputs just examined.

        ![start executor](start-executor.png)

    * Trigger the command.

        ![Trigger the command](trigger.png)

        No additional inputs are required.

        The command will then complete, and its status can be examined.

        ![completed command](status.png)

        The status of the SAP HANA Cloud instance will update to Starting.

        ![HCC starting](starting.png)

3. Open the command **UpdateHC**.  It can be used to perform an update of an SAP HANA Cloud instance.  

    * Examine the **Description** input parameter.  This parameter describes the change that will be made.

        ![update the description](description.png)

        In this case the update will be to the description of the instance but other parameters could be updated as described at [Parameter Reference](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/parameter-reference).

    * Trigger the command.  Once the command has completed,  the description will appear as shown below in SAP HANA Cloud Central.

        ![updated description](updated-description.png)

4. Open the command **UpgradeHC**.  This command takes an input parameter to specify the instance to target.  It can be used to perform an upgrade of an SAP HANA Cloud instance and selects the instance to upgrade to from a value returned by a call to GetInstanceParameters.  This command could then be scheduled to run nightly or weekly which would then keep the SAP HANA Cloud instance running using the latest available patch or update.  
    
    ![UpgradeHC2 command](upgradeHC2.png)

    * Prior to triggering the command, examine the instance in SAP HANA Cloud Central.

        ![Viewing available upgrade details in HCC](upgrade-hcc2.png)

    * Trigger the command and examine the output.

        ![updgrade HC Output](upgradeHCOutput.png)

    * Once the command completes, the notification that there is a new version available will disappear and the new version will be shown in SAP HANA Cloud Central.

        ![HCC After the upgrade](hcc-after-update2.png)
      
    * If there is not an available version to update to, an error message is shown.

        ![no version to update to](no-version-to-update.png)

        This check is performed in the validation step of the commmand getParams.  
        
        ![validate step](validation.png)

        This is performed using a [dynamic expression](https://help.sap.com/docs/automation-pilot/automation-pilot/dynamic-expression?locale=en-US) which uses jq.  A playground is available for jq at [jq play](https://jqplay.org/#). 

    * Additional customization can be performed in the **generateJSON** command.  

        ![generate Update JSON](generateUpdateJSON.png)

        If no further customization is needed, the JSON could instead be specified directly in the parameter parameters in the upgrade executor using the below JSON.

        ```JSON
        {
          "data": {
            "productVersion": {
              "releaseCycle": "generally-available-quarterly",
              "track": "$(.getParams.output.CommandOutput.availableUpgradeVersions[0].track)",
              "id": "$(.getParams.output.CommandOutput.availableUpgradeVersions[0].id)"
            }
          }
        } 
        ``` 

5. Open the command **MassUpgradeHC**.  This command takes two input parameters.  The parameter **InstanceDetails** includes the serviceKey which is used for authentication and the servicePlanId is used as a filter to return only SAP HANA Cloud instances in the getInstances executor.  The parameter **Upgrade** specifies the product version that should be used when upgrading the list of SAP HANA Cloud instances.  

    ![UpgradeHC3 command](upgradeHC3.png)

    * Prior to triggering the command, examine the instances in SAP HANA Cloud Central and note any instances that have an upgrade available to them.

        ![Viewing available upgrade details in HCC](upgrade-hcc2.png)  

    * The getInstances executor returns the list of SAP HANA Cloud instances that have been deployed to the subaccount.  It does not include instances deployed to a Cloud Foundry space.

        ![getInstances executor](mass-upgrade-getInstances.png)

    * The upgrade executor loops through the list of instances returned by getInstances and attempts to upgrade each instance.

        ![upgrade executor](mass-upgrade.png)

    * After triggering the command, examine the instances in SAP HANA Cloud Central and note that they now have been upgraded to the specified version.  
    
    * The output of the command also indicates the list of instances found.

        ![MassupgradeHC output](mass-upgrade-output.png)

### Execute commands for instances provisioned to Cloud Foundry
The examples shown include commands to start, stop, update, and upgrade an SAP HANA Cloud instance.  The commands make use of two common input parameters that specify the instance to target and the credentials to use.

1. Modify the input keys to match the SAP HANA Cloud instances that you wish to work with.

    * Open **CFInstanceDetails**.

        ![Update the input instance details](cf-inputs.png)

        The values can be found on the Overview page of the subaccount page.

        ![subaccount overview page](overview.png)

    * Open **CFTechnicalUser**.

        Enter your Cloud Foundry user and password.  The user must not have two factor authentication enabled.  

        ![Cloud Foundry technical user](cftechnicaluser.png)

2. The commands **CFStartHC** and **CFStopHC** can be used to start and stop a SAP HANA Cloud instance.

    * Open the command **CFStartHC**.

        ![inputs for the start command](cf-start-inputs.png)

        Notice that it has as its inputs **CFUser** which provides the technical user details and **CFInstanceDetails** which describes which SAP HANA Cloud instance to target.

        Notice that the executor named **CFStartHC** makes use of one of the provided commands `dblm-sapcp:StartHanaCloudInstance` and that its inputs are taken from inputs just examined.

        ![cf start executor](cf-start-executor.png)

    * Trigger the command.

        ![Trigger the command](cf-trigger.png)

        No additional inputs are required.

        The command will then complete, and its status can be examined.

        ![completed command](cf-status.png)

        The status of the SAP HANA Cloud instance will update to Starting.

        ![HCC starting](cf-starting.png)

    * An addition example that demonstrates how to perform start and stop operations on multiple instances at once is available at [Mass Stop/Start HANA Cloud Databases](https://github.com/SAP-samples/automation-pilot-examples/tree/main/mass-stop-start-hana-cloud).

3. Open the command **CFUpdateHC**.  The command can be used to perform an update of an SAP HANA Cloud instance.  

    * Examine the **Description** input parameter.  This parameter describes the change that will be made.

        ![update the description](description.png)

        In this case the update will be to the description of the instance but other parameters could be updated as described at [Parameter Reference](https://help.sap.com/docs/hana-cloud/sap-hana-cloud-administration-guide/parameter-reference).

    * Trigger the command and notice that the description will appear as shown below in SAP HANA Cloud Central after the command has finished executing.

        ![updated description](cf-updated-description.png)


4. Open the command **CFUpgradeHC**.  It can be used to perform an upgrade of an SAP HANA Cloud instance.  This command takes two input parameters; one to specify the instance to target and another for the credentials to use.  The command can be used to perform an upgrade of an SAP HANA Cloud instance and selects the instance to upgrade to from a value returned by a call to GetCfServiceInstance.  This command could then be scheduled to run nightly or weekly which would then keep the SAP HANA Cloud instance running using the latest available patch or update.  
    
    ![CFUpgradeHC command](cf-upgrade.png)

    * Prior to triggering the command, examine the instance to be upgraded in SAP HANA Cloud Central.

        ![Viewing available upgrade details in HCC](cf-upgrade-hcc.png)

    * Trigger the command and examine the output once it completes.

        ![updgrade HC Output](upgradeHCCOutput.png)

    * In SAP HANA Cloud Central, the notification that there is a new version available will disappear and the new version will be shown.

        ![HCC After the upgrade](cf-hcc-after-upgrade.png)
      
    * If there is not an available version to update to, an error message is shown.

        ![no version to update to](cf-no-version-to-update.png)

        This check is performed in the validation step of the commmand getParams.  
        
        ![validate step](validation2.png)

        This is performed using a [dynamic expression](https://help.sap.com/docs/automation-pilot/automation-pilot/dynamic-expression?locale=en-US) which uses jq.  A playground is available for jq at [jq play](https://jqplay.org/#). 

    * Additional customization can be performed in the **generateJSON** command.

        ![generate Update JSON](cf-generateUpdateJSON.png)
      
        If no further customization is needed, the JSON could instead be specified directly in the parameter parameters in the upgrade executor using the below JSON.

        ```JSON
        {
          "data": {
            "productVersion": {
              "releaseCycle": "generally-available-quarterly",
              "track": "$(.getParams.output.parameters.availableUpgradeVersions[0].track)",
              "id": "$(.getParams.output.parameters.availableUpgradeVersions[0].id)"
            }
          }
        } 
        ```

        An additional upgrade example is available at [Patch Update of HANA Cloud Database](https://github.com/SAP-samples/automation-pilot-examples/tree/main/patch-update-hana-cloud).

### Knowledge check

Congratulations! You have now used the SAP Automation Pilot to run commands against an SAP HANA Cloud instance.
