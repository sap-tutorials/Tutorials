---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-btp--abap-environment]
primary_tag: programming-tool>abap-development
---

# Get Started with Entity Manipulation Language (EML) in SAP BTP ABAP Environment
<!-- description --> Access RAP business objects using EML in SAP BTP ABAP Environment

## Prerequisites
 - **Tutorial mission**: [Develop an SAP Fiori App Using SAP BTP, ABAP Environment](mission.cp-starter-extensions-abap)

## You will learn
  - How to access RAP business objects using Entity Manipulation Language (EML), part of ABAP. You can then use EML to read or modify business objects, or to implement behaviors for them.

## Intro
The Entity Manipulation Language (EML) is a part of the ABAP language that enables access to RAP business objects.

Because the consumption of business objects via the OData protocol requires a Fiori UI or a web API, EML enables a type-safe access to business objects directly by using ABAP. EML interacts with business objects by triggering their operations for specified entities. An operation can only be triggered by EML if the operation is specified for the relevant entity in the behavior definition and if it implemented accordingly.

Important: If you intend to use EML productively, we strongly recommend that you complete the following openSAP course:
[Building Apps with the ABAP RESTful Application Programming Model](https://open.sap.com/courses/cp13). However, this tutorial provides a first taster for understanding EML. Also, a basic understanding of the ABAP RESTful Application Programming Model (RAP) is essential if you are doing this tutorial toe earn a SAP Community badge.

Throughout this tutorial, replace `XXX` with your initials or group number.

---

### Watch video

Watch the following video from the openSAP course: Building Apps with the ABAP RESTful Application Programming Model: [Week 3, Unit 4: Understanding Entity Manipulation Language (EML)](https://open.sap.com/courses/cp13/items/1PQYUmWLxhSJ6jovoMOScA)




### Answer question 1





### Answer question 2






### Complete exercise

From the openSAP course, you need to complete the hands-on exercise for [Week 3, Unit 4: Understanding EML](https://github.com/SAP-samples/abap-platform-rap-opensap/blob/main/week3/unit4.md).
However, since you have not completed the other weeks of the course, you cannot use the objects already created by course participants in previous weeks. Instead, use the objects you created in the mission [Develop an SAP Fiori App Using SAP BTP, ABAP Environment](mission.cp-starter-extensions-abap).

You must make the following adjustments:
- Change the name of the CDS view you are using to **`zi_travel_ve_m_xxx`**
- Change the key field name **`TravelUUID`** to **`mykey`**

To check your code at any point, see step 4 below.




### Answer question 3






### Check your code

Your code should look like this:

```ABAP

CLASS zcl_rap_eml_xxx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_RAP_EML_xxx IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

" Change UUID > mykey;
" change entity to zi_travel_ve_m_XXX
" use ZI, not ZC bcs BDEF refers to ZI
" ENTITY travel = Behavior Definition

    " step 1 - READ
*    READ ENTITIES OF zi_travel_ve_m_xxx
*      ENTITY travel
*        FROM VALUE #( ( mykey = '02D5290E594C1EDA93815057FD946624' ) )
*      RESULT DATA(travels).
*          out->write( travels ).

    " step 2 - READ specific fields
*    READ ENTITIES OF zi_travel_ve_m_xxx
*      ENTITY travel
*        FIELDS ( agency_id customer_id )
*        WITH VALUE #( ( mykey = '02D5290E594C1EDA93815057FD946624' ) )
*      RESULT DATA(travels).
*    out->write( travels ).

    " step 3
*        READ ENTITIES OF zi_travel_ve_m_xxx
*      ENTITY travel
*        ALL FIELDS
*        WITH VALUE #( ( mykey = '02D5290E594C1EDA93815057FD946624' ) )
*      RESULT DATA(travels).
*    out->write( travels ).

    " step 4 READ by association - can't be done

    " step 5 Failure handling in READ operations
*         READ ENTITIES OF zi_travel_ve_m_xxx
*       ENTITY travel
*         ALL FIELDS WITH VALUE #( ( mykey = '11111111111111111111111111111111' ) )
*       RESULT DATA(travels)
*       FAILED DATA(failed)
*       REPORTED DATA(reported).
*
*     out->write( travels ).
*     out->write( failed ).    " complex structures not supported by the console output
*     out->write( reported ).  " complex structures not supported by the console output

    " step 6 - MODIFY Update
*     MODIFY ENTITIES OF zi_travel_ve_m_xxx
*       ENTITY travel
*         UPDATE
*           SET FIELDS WITH VALUE
*             #( ( mykey = '02D5290E594C1EDA93815057FD946624'
*                  Description = 'I like ABAP' ) )
*
*      FAILED DATA(failed)
*      REPORTED DATA(reported).
*
*     out->write( 'Update done' ).
*
*     " step 6b - Commit Entities
*     COMMIT ENTITIES
*       RESPONSE OF zi_travel_ve_m_xxx
*       FAILED     DATA(failed_commit)
*       REPORTED   DATA(reported_commit).

    " step 7 - MODIFY 1 = "Create an entity"
*       MODIFY ENTITIES OF zi_travel_ve_m_xxx
*        ENTITY travel
*          CREATE
*            SET FIELDS WITH VALUE
*            #( ( %cid        = 'MyContentID_1'
*                 agency_id    = '70012'
*                 customer_id  = '14'
*                 begin_date   = cl_abap_context_info=>get_system_date( )
*                 end_date     = cl_abap_context_info=>get_system_date( ) + 10
*                 Description = 'I created this!' ) )
*
*     MAPPED DATA(mapped)
*     FAILED DATA(failed)
*     REPORTED DATA(reported).
*
*    out->write( mapped-travel ).
*
*
*    COMMIT ENTITIES
*      RESPONSE OF zi_travel_ve_m_xxx
*      FAILED     DATA(failed_commit)
*      REPORTED   DATA(reported_commit).
*
*    out->write( 'Create done' ).

    " step 8 MODIFY 2 = "Delete an entry"
*    MODIFY ENTITIES OF zi_travel_ve_m_xxx
*      ENTITY travel
*        DELETE FROM
*          VALUE
*            #( ( mykey  = '<your uuid>' ) )
*
*     FAILED DATA(failed)
*     REPORTED DATA(reported).
*
*    COMMIT ENTITIES
*      RESPONSE OF zi_travel_ve_m_xxx
*      FAILED     DATA(failed_commit)
*      REPORTED   DATA(reported_commit).
*
*    out->write( 'Delete done' ).


  ENDMETHOD.
ENDCLASS.

```





### More Information
- SAP Help Portal: SAP - ABAP RESTful Application Programming Model: [About Entity Manipulation Language or EML](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/af7782de6b9140e29a24eae607bf4138.html)

---
