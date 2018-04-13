---
title: Install the Optional SAP HANA Interactive Education Package for SAP HANA, express edition
description: SAP HANA Interactive Education (SHINE) makes it easy to learn how to build applications on SAP HANA Extended Application Services Advanced Model (XSA).
primary_tag: products>sap-hana\,-express-edition
tags: [ tutorial>beginner, products>sap-hana\,-express-edition ]
---

<!-- loio5a9927fcad7c436d9fb8a36062acc1b8 -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:**  

## Details
### You will learn
You will learn how to download, install, and configure the `shine.tgz` SAP HANA Interactive Education package.

### Time to Complete
15 min

---

SHINE is provided as an optional component for SAP HANA, express edition. Download the SHINE installation file, `shine.tgz`, from the SAP HANA, express edition Download Manager. This download includes installation files for installing SHINE on XSC and XSA.

[ACCORDION-BEGIN [Step 1: ](Install SHINE for XSC.)]

Installation files for SHINE for `XSC` are located at:

```bash
`extracted_path`/HANA_EXPRESS_20/DATA_UNITS/HCO_HANA_SHINE
```

To install SHINE for XSC, see the [SAP HANA Interactive Education (SHINE) guide](http://help.sap.com/hana/SAP_HANA_Interactive_Education_SHINE_en.pdf).

> Note:
> The HANA JDBC port number for SAP HANA, express edition is different than the default port number 30015 mentioned in the SHINE guide. You need to update the port parameter for the resources `CrossSchemaSys` and `CrossSchemaSysBi` in the `mtaext` file to 3`instance-number`13.
> 
> 

> Note:
> SAP plans to remove SAP HANA extended application services, classic model (XSC) and the corresponding SAP HANA Repository with the next major product version of SAP HANA.
> 
> These components will be removed:
> 
> -   SAP HANA extended application services, classic model
> 
> -   SAP HANA Repository (XS classic)
> 
> -   SAP HANA Studio (Development, Modeling, and Administration perspectives)
> 
> -   SAP HANA Web-based Development Workbench (XS classic)
> 
> 
> SAP strongly advises you to plan the transition of existing content and applications from XSC to SAP HANA extended application services, advanced model (XS Advanced).
> 
> 

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install SHINE for XSA.)]

Installation files for SHINE for `XSA` are located at:

```bash
`extracted_path`/HANA_EXPRESS_20/DATA_UNITS/XSA_CONTENT_10
```

To install SHINE for XSA, run the following as `sid``adm`:

```bash
<extracted_path>/HANA_EXPRESS_20/install_shine.sh
```

[ACCORDION-END]


