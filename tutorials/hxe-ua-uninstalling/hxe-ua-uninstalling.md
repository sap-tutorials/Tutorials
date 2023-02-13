---
parser: v2
primary_tag: software-product>sap-hana--express-edition
tags: [  tutorial>how-to, tutorial>beginner, software-product>sap-hana--express-edition  ]
author_name: John Currie
author_profile: https://github.com/JCurrie27
time: 20
---
# Uninstalling SAP HANA, express edition
<!-- description --> Follow these steps to uninstall the binary version of SAP HANA, express edition, or the SAP EA Designer component.

## Prerequisites  
- **Setup:** You have installed the binary version of SAP HANA, express edition.



### Uninstalling SAP HANA, express edition



Start the `hdblcm` tool:

`sudo /hana/shared/HXE/hdblcm/hdblcm`


Select `uninstall`.

Choose to uninstall all components.

Uninstall the SAP Host Agent:

`sudo /usr/sap/hostctrl/exe/saphostexec -uninstall`



### Uninstalling the SAP EA Designer Component



As the `<sid>adm` user, log in to XSA:

`xs login -u xsa_admin -p "<password>" -s SAP`

Uninstall the SAP EA Designer software component. To uninstall the component plus the HDI container and repository database, use the following command:

`xs uninstall XSAC_HANA_EA_D --delete-services`

To delete the component but retain the HDI container and repository database, use the following command:

`xs uninstall XSAC_HANA_EA_D`



