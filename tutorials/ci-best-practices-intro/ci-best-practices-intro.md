---

title: Continuous Integration (CI) Best Practices with SAP: Introduction and Navigator
description: Part 1: Introduction and Navigation through the Best Practices Guide
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites  
 - **Proficiency:** Intermediate

## Outline

1. [Introduction and Navigator (this document)](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)  
2. Continuous Integration and Delivery  
2.1. [CI/CD Practices](http://go.sap.com/developer/tutorials/ci-best-practices-ci-cd.html)  
2.2. [Pipeline Suggestions](http://go.sap.com/developer/tutorials/ci-best-practices-pipelines.html)  
3. CI/CD Landscape - Component Setup  
3.1. [Source Code Versioning System](http://go.sap.com/developer/tutorials/ci-best-practices-scm.html)  
3.2. [Build Scheduler](http://go.sap.com/developer/tutorials/ci-best-practices-build.html)  
3.3. [Artifact Repository](http://go.sap.com/developer/tutorials/ci-best-practices-artifacts.html)  
3.4. [Landscape Configuration](http://go.sap.com/developer/tutorials/ci-best-practices-landscape.html)  
4. CI/CD Process Setup  
4.1. [Generic Project](http://go.sap.com/developer/tutorials/ci-best-practices-generic.html)  
4.2. [Java Web on SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/ci-best-practices-java-hcp.html)  

---


What you need to be successful in building business applications on SAP's cloud platform or using SAP on premise technologies is an efficient software development process.

In this guide we will discuss some basic principles for software development that are derived from the process definitions for Continuous Integration (CI) and Continuous Delivery (CD). We will then break the principles down into the best practices that will support you in setting up an efficient development process for your cloud application development on SAP HANA Cloud Platform (SAP HCP).

We noticed that customers seek for more guidance on an end-to-end CI process. SAP offers several tools to address single aspects.
The goal of this guide is to provide recommendations that focus on the complete picture of a CI or CD process and how to set it
up using widely adapted complementary tools. The choice of tools should be considered as examples. Other tools fulfilling the same purpose will work as well.

We will examine the most important SAP technologies and describe how to use existing tools to set up a CI process that fits the individual needs of your development project, your requirements, and your environment.

Just follow the step-by-step recipes we offer to set up and configure the needed technical components. Get guidance on how to orchestrate them to build a generic skeleton CI or CD process based on selected SAP technologies. We will work out the specifics of each technology and explain how to adopt the CI and CD approaches with the help of concrete examples or reference applications.

This guide is targeted at any SAP customer or partner interested in or currently setting up a development process for their cloud application development on SAP HCP or on premise technologies.
Part 2 of the guide describing the high level principles addresses all people: It will help managers, product owners, quality managers and developers to understand why these principles make sense and how they can be applied.
The subsequent parts are more technical. They address engineers who are actually implementing the process on a technical level: process architects, build engineers, and operators.

## Navigator through the Best Practices Guide

This best practices guide has to cover a broad spectrum of aspects and addresses a lot of topics on different levels for people with different roles and interests. Not all parts of the guide are of interest for all people. Thus we tried to identify some roles who might be interested in at least parts of the guide and direct them to exactly those parts. By this, we try to avoid that somebody who is primarily interested in the high-level principles has to go through a lot of technical details and vice versa.

### Roles

| Role | Description, Skill Set |
|-------------------|----------------------------------------------------------------|
| Manager, Decision Maker | Makes investment decisions on infrastructure, tools and usage of public services. He is mainly interested in a sustainable investment into the infrastructure. |
| Quality Manager | Is responsible for the quality. He defines and decides on the processes to ensure the quality level. | 
| Delivery Manager | Is responsible for the shipment and deployment of the product to production. He relies on the quality and defines the deployment process. |
| Build Operator | Operates the technical components for a CI/CD infrastructure. |
| Build Engineer | Implements the CI/CD process on top of the technical components. He implements and supports the product-specific requirements that have to be implemented in the build and test procedures. He might be an expert of the software technology in which the product is implemented. | 
| Developer engaged in CI | He implements the build scripts and tests in collaboration with the Build Engineer. He is an expert of the software technology in which the product is implemented. |

### Navigate through the relevant parts of the Guide

+: The part is of interest  
++: The part is of strong interest  
(+): The part is of interest based on the respective technology  
(++): The part is of strong interest based on the respective technology

| Part of the Guide      | Manager, Decision Maker | Quality Manager | Delivery Manager | Build Operator | Build Engineer | Developer engaged in CI |
|-----------|---|---|---|---|---|---|
| **1. [Introduction and Navigator (this document)](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)** | ++         | ++         | ++         | ++         | ++         | ++          |
| **2. Continuous Integration and Delivery** |            |            |            |            |            |            |
| 2.1. [CI/CD Practices: The Practices and Principles of CI and CD](http://go.sap.com/developer/tutorials/ci-best-practices-ci-cd.html)   | ++         | ++         | ++         | +          | +          | +          |
| 2.2. [Pipeline Suggestions: Patterns for a CI/CD pipeline](http://go.sap.com/developer/tutorials/ci-best-practices-pipelines.html)   | +          | ++         | ++         | +          | ++         |            |
| **3. CI/CD Landscape - Component Setup**   |            |            |            |            |            |            |
| 3.1. [Source Code Versioning System: Setting up a Git/Gerrit Instance](http://go.sap.com/developer/tutorials/ci-best-practices-scm.html)     |            |            |            | ++         |            |            |
| 3.2. [Build Scheduler: Setting up a Jenkins Instance](http://go.sap.com/developer/tutorials/ci-best-practices-build.html)       |            |            |            | ++         |            |            |
| 3.3. [Artifact Repository: Setting up a Nexus Instance](http://go.sap.com/developer/tutorials/ci-best-practices-artifacts.html)       |            |            |            | ++         |            |            |
| 3.4. [Landscape Configuration: Configuring the CI Component Landscape](http://go.sap.com/developer/tutorials/ci-best-practices-landscape.html)       |            |            |            | ++         | +          |            |
| **4. CI/CD Process Setup**                |            |            |            |            |            |            |
| 4.1. [Generic Project: Configuring the CI system for Maven-based generic Java project](http://go.sap.com/developer/tutorials/ci-best-practices-generic.html)                 |            |            |            | +          | ++         | +          |
| 4.2. [Java Web on SAP HANA Cloud Platform: Configuring the CI system for Maven-based Java Web project](http://go.sap.com/developer/tutorials/ci-best-practices-java-hcp.html) |          |            |            | (+)        | (++)       | (+)        |

