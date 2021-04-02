---
auto_validation: true
title: Implement ETag Handling For Your Factory Calendar
description: Implement ETag Handling for your factory calendar.
primary_tag: products>sap-btp--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-business-technology-platform ]
time: 20
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
- You need an SAP BTP, ABAP environment [trial user](abap-environment-trial-onboarding) or a license.

## Details
### You will learn  
- How to create `ETag` handling

A draft enabled RAP business object requires a designated field to enable optimistic [concurrency checks during the transition from draft to active data](https://help.sap.com/viewer/923180ddb98240829d935862025004d6/Cloud/en-US/f5e8548c241b43ab82bceec030b5dc9a.html). This is mandatory.
To provide optimistic concurrency locking for OData consumers an additional field must be designated. This is not mandatory.
For a smoothly running application, you need both `ETags`. Typically a timestamp field is used.
The RAP managed framework is able to update the `ETags` field automatically if they are annotated with `@Semantics.systemDateTime` in the CDS View.

---
[ACCORDION-BEGIN [Step 1: ](Create ETag handling)]

  1. Open behavior definition **`ZCAL_I_HOLIDAY_XXX`**. Add `etag master local_last_changed_at` to the root entity and `etag dependent by _Public_Holiday` to the text entity.

    ```ABAP
    managed implementation in class zbp_cal_i_holiday_xxx unique;
    with draft;

    define behavior for ZCAL_I_HOLIDAY_XXX alias HolidayRoot
    persistent table zcal_holiday_xxx
    draft table zcal_d_holi_xxx
    lock master total etag last_changed_at
    //authorization master ( instance )
    etag master local_last_changed_at
    {
      create;
      update;
      delete;

      field ( readonly : update ) HolidayId;

      association _HolidayTxt { create; with draft; }

      mapping for zcal_holiday_xxx corresponding
      {
        HolidayId = holiday_id;
        MonthOfHoliday = month_of_holiday;
        DayOfHoliday = day_of_holiday;
      }
    }

    define behavior for ZCAL_I_HOLIDAYTXT_XXX alias HolidayText
    persistent table zcal_holitxt_xxx
    draft table zcal_d_holit_xxx
    etag dependent by _Public_Holiday
    lock dependent by _Public_Holiday
    {
      update;
      delete;
      field ( readonly : update ) HolidayId;
      field ( readonly : update ) Language;

      association _Public_Holiday { with draft; }

      mapping for zcal_holitxt_xxx corresponding
      {
        Language = spras;
        HolidayId = holiday_id;
        HolidayDescription = fcal_description;
      }
    }
    ```

  2. Save and activate.

  3. Open behavior definition **`ZCAL_C_HOLIDAY_XXX`**. Add `use etag` for both entities.

    ```ABAP
    projection implementation in class zbp_cal_c_holiday_xxx unique;
    use draft;

    define behavior for ZCAL_C_HOLIDAY_XXX alias HolidayRoot
    use etag
    {
      use create(augment);
      use update(augment);
      use delete;

      use association _HolidayTxt { create; with draft; }

      field ( modify ) HolidayDescription;
    }

    define behavior for ZCAL_C_HOLIDAYTXT_XXX alias HolidayText
    use etag
    {
      use update;
      use delete;

      use association _Public_Holiday { with draft; }
    }
    ```

  4. Save and activate.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
