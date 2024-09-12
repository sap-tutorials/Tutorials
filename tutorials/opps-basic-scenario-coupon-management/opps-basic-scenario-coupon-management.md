---
parser: v2
author_name: Aathira P
author_profile: https://github.com/Aathira-I553036
auto_validation: true
time: 20
tags: [tutorial>advanced, topic>cloud, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: products>sap-business-technology-platform
---

# Create Multi-Code Coupons with SAP Omnichannel Promotion Pricing
<!-- description --> Learn how to use SAP Omnichannel Promotion Pricing for creating multi-code coupons in your promotions.

## Prerequisites
- You have registered for a trial account on [SAP BTP](hcp-create-trial-account) in the Singapore - Azure region.
- You should be familiar with the SAP BTP trial landscape and terminology (see [Take a Tour of SAP BTP Trial](cp-trial-quick-onboarding)).
- You have a subaccount in the Cloud Foundry environment, with Microsoft Azure as provider and the Singapore region (AP21).
- You have set up SAP Omnichannel Promotion Pricing as described in [Set Up SAP Omnichannel Promotion Pricing](opps-manual-setup).



## You will learn
- How to use the SAP Omnichannel Promotion Pricing trial on SAP BTP.
- How to upload a **promotional rule** with the help of the SAP Business Accelerator Hub. The promotional rule includes an example about creating multi-code coupons.
- How to apply a promotional rule and execute the calculation request.


---

### Get to know the business scenario


* Business goal: In your **Heidelberg** store (business unit ID: `FC01`) you want to promote coffee machines to free up stock. For this, you have created the following promotion: **Buy a coffee machine and show the coupon TIME FOR COFFEE to get 100 Euro off**.
* Your promotion is valid from **September 1st, 2024 until September 30th, 2024**.
* The **regular sales price** for the **coffee machine** (material number: `COFFEE_MACHINE`) is **500 Euro**.
* You are planning to generate 200 coupon codes and read them.
* You call the coupon service with the promotion information (store, products, date, time).


<!-- ### Set up SAP Omnichannel Promotion Pricing


To use the SAP Omnichannel Promotion Pricing trial, you need to:

1. Subscribe to SAP Omnichannel Promotion Pricing
2. Set Up Roles and Authorizations
3. Create Service Instances
4. Create Service Keys

For step-by-step instructions, go to the tutorial [Set Up SAP Omnichannel Promotion Pricing](opps-manual-setup).
 -->


### Create environments in SAP Business Accelerator Hub


In order to continue with the next steps, you need to have already set up your trial environments for the Coupon Management service, Data Upload service and Data Access service in the SAP Business Accelerator Hub. If you need guidance, have a look at step 2 of the tutorial [Apply a Simple Promotion with SAP Omnichannel Promotion Pricing](opps-basic-scenario).


### Create Multi-Code Coupon Object


You can now create a multi-code coupon object with the Coupon Management service. 

Navigate to the [Coupon Management service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/CouponManagement/resource). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Coupon management 1](coupon_management_1.png)
<!-- border -->![Coupon management 2](coupon_management_2.png)


1. For **API Environment**, select the trial environment you have created for the Coupon Management service.
2. On the left-hand side, choose **Create coupon object using OAuth2**.
3. Open the **POST** request `/idocinbound`.
4. Choose **Try Out**.
5. For the **Request body**, choose `application/json`.
6. Copy the following raw data and paste it into the body:
```json
{ 
"generatedCodeCount": 200, 
"validFrom": "2025-09-07" 
}
```
7. Choose **Execute**.
8. Make sure you get the HTTP response **200**.

With this, you created the coupon object for the promotional rule **Buy a coffee machine and show the coupon TIME FOR COFFEE to get 100 Euro off** for the **Heidelberg** store (business unit ID: `FC01`) and the validity period **September 1st, 2024 to September 30th, 2024**.


### Generate Coupon Codes


You can generate coupon codes with the multi-code coupon object created using the Coupon Management service. 

Navigate to the [Coupon Management service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/CouponManagement/resource). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Coupon management 1](coupon_management_1.png)
<!-- border -->![Coupon management 2](coupon_management_2.png)


1. For **API Environment**, select the trial environment you have created for the Coupon Management service.
2. On the left-hand side, choose **Generate coupon codes using OAuth2**.
3. Open the **POST** request `/idocinbound`.
4. Choose **Try Out**.
5. For the **Request body**, choose `application/json`.
6. Copy the following raw data and paste it into the body:
```json
{ 
"generatedCodeCount": 200, 
"validFrom": "2025-09-07" 
}
```
7. Choose **Execute**.
8. Make sure you get the HTTP response **200**.

With this, you generated the coupon codes for the multi-code coupon **TIME FOR COFFEE** for the **Heidelberg** store (business unit ID: `FC01`) and the validity period **September 1st, 2024 to September 30th, 2024**.


### Upload promotional rule


You can now upload the promotional rule with the Data Upload service.

Navigate to the [Data Upload service of SAP Omnichannel Promotion Pricing on SAP Business Accelerator Hub](https://api.sap.com/api/DataUpload/resource). Log on with the same login information that you use for your SAP BTP account.

<!-- border -->![Data upload 1](data_upload_1.png)
<!-- border -->![Data upload 2](data_upload_2.png)


1. For **API Environment**, select the trial environment you have created for the Data Upload service.
2. On the left-hand side, choose **Upload promotions using OAuth2**.
3. Open the **POST** request `/idocinbound`.
4. Choose **Try Out**.
5. For the **Request body**, choose `application/xml`.
6. Copy the following raw data and paste it into the body:
```XML
<?xml version="1.0" encoding="utf-8"?>
<_-ROP_-PROMOTION02>
  <IDOC BEGIN="1">
    <EDI_DC40 SEGMENT="1">
      <IDOCTYP>/ROP/PROMOTION02</IDOCTYP>
    </EDI_DC40>
    <_-ROP_-E1_PROMOTION SEGMENT="1">
      <CHANGED_ON>20210506072209</CHANGED_ON>
      <CHANGE_INDICATOR>I</CHANGE_INDICATOR>
      <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
      <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
      <EXTERN_PROMO_ID>000000000000000000000000000000000161</EXTERN_PROMO_ID>
      <LOGSYS>ABCCLNT999</LOGSYS>
      <MIN_PPS_RELEASE>0</MIN_PPS_RELEASE>
      <OFFER_ID>42010AEF0FAA1EEBBABE7EEA0C2D0823</OFFER_ID>
      <ORIGIN>01</ORIGIN>
      <PROMOTION_ID>771</PROMOTION_ID>
      <PROMOTION_TYPE>Z001</PROMOTION_TYPE>
      <STATUS_TCD>AC</STATUS_TCD>
      <_-ROP_-E1_PROMOTION_BU SEGMENT="1">
        <BU_ID>FC01</BU_ID>
        <BU_TYPE>1040</BU_TYPE>
      </_-ROP_-E1_PROMOTION_BU>
      <_-ROP_-E1_PROMOTION_RULE SEGMENT="1">
        <ELIGIBILITY_ID>772</ELIGIBILITY_ID>
        <ONLY_BASKET_MODE>X</ONLY_BASKET_MODE>
        <PRICE_RULE_ID>772</PRICE_RULE_ID>
        <PROMO_RULE_ID>772</PROMO_RULE_ID>
        <RESOLUTION>1</RESOLUTION>
        <SALE_RETURN_TCD>00</SALE_RETURN_TCD>
        <SEQUENCE>772</SEQUENCE>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <COMBINATION_CODE>&amp;&amp;</COMBINATION_CODE>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>772</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>772</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>772</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>772</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COMB</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <CONSUMPTION_TCD>00</CONSUMPTION_TCD>
          <COUPON_NUMBER>TIME FOR COFFEE</COUPON_NUMBER>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>773</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>772</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>772</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>772</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COUP</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>774</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>1.000</INTERVAL_QTY>
          <ITEM_ID>COFFEE_MACHINE</ITEM_ID>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>9999999999.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>772</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>772</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>772</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>1.000</THRESHOLD_QTY>
          <THRESHOLD_TCD>QUTI</THRESHOLD_TCD>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>ITEM</TYPE_CODE>
          <UOM_ISO_CODE>PCE</UOM_ISO_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_PRICE_RULE SEGMENT="1">
          <CALCULATION_BASE>00</CALCULATION_BASE>
          <CALC_BASE_SEQUENCE>1-</CALC_BASE_SEQUENCE>
          <CONSIDER_PREVIOUS_RULES>X</CONSIDER_PREVIOUS_RULES>
          <COUPON_VALIDITY>0</COUPON_VALIDITY>
          <CURRENCY_ISO_CODE>EUR</CURRENCY_ISO_CODE>
          <DISC_METHOD_TCD>00</DISC_METHOD_TCD>
          <EX_PRICE_RULE_ID>0000000000000000000</EX_PRICE_RULE_ID>
          <ITEM_METHOD>00</ITEM_METHOD>
          <MAM_LIMIT_CNT>0.000</MAM_LIMIT_CNT>
          <NEW_PRICE_AMT>0.00000</NEW_PRICE_AMT>
          <NEW_SET_PRCE_AMT>0.00000</NEW_SET_PRCE_AMT>
          <PRICE_MODIF_AMT>5.00000</PRICE_MODIF_AMT>
          <PRICE_MODIF_CODE>RT</PRICE_MODIF_CODE>
          <PRICE_MODIF_PERC>0.000</PRICE_MODIF_PERC>
          <PRICE_RULE_ID>772</PRICE_RULE_ID>
          <ROUNDING_METHOD>00</ROUNDING_METHOD>
          <ROUND_DECIMALS>2</ROUND_DECIMALS>
          <ROUND_DESTINATION>1</ROUND_DESTINATION>
          <RULE_CTRL_CODE>PO</RULE_CTRL_CODE>
          <STATUS_TCD>AC</STATUS_TCD>
          <SUBSEQ_NEW_PRICE_AMT>0.00000</SUBSEQ_NEW_PRICE_AMT>
          <SUBSEQ_PRICE_MODIF_AMT>0.00000</SUBSEQ_PRICE_MODIF_AMT>
          <SUBSEQ_PRICE_MODIF_PERC>0.000</SUBSEQ_PRICE_MODIF_PERC>
          <TYPE_CODE>RB</TYPE_CODE>
        </_-ROP_-E1_PRICE_RULE>
      </_-ROP_-E1_PROMOTION_RULE>
      <_-ROP_-E1_PROMOTION_RULE SEGMENT="1">
        <ELIGIBILITY_ID>775</ELIGIBILITY_ID>
        <ONLY_BASKET_MODE>X</ONLY_BASKET_MODE>
        <PRICE_RULE_ID>775</PRICE_RULE_ID>
        <PROMO_RULE_ID>775</PROMO_RULE_ID>
        <RESOLUTION>0</RESOLUTION>
        <SALE_RETURN_TCD>00</SALE_RETURN_TCD>
        <SEQUENCE>775</SEQUENCE>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <COMBINATION_CODE>&amp;&amp;</COMBINATION_CODE>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>775</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>775</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>775</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>775</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COMB</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <CONSUMPTION_TCD>00</CONSUMPTION_TCD>
          <COUPON_NUMBER>TIME FOR COFFEE</COUPON_NUMBER>
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>776</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>0.000</INTERVAL_QTY>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>0.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>775</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>775</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>775</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>0.000</THRESHOLD_QTY>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>COUP</TYPE_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_ELIGIBILITY SEGMENT="1">
          <EFFECTIVE_DATE>20210501000000</EFFECTIVE_DATE>
          <ELIGIBILITY_ID>777</ELIGIBILITY_ID>
          <EXPIRY_DATE>20210507235959</EXPIRY_DATE>
          <INTERVAL_AMT>0.00000</INTERVAL_AMT>
          <INTERVAL_QTY>1.000</INTERVAL_QTY>
          <ITEM_ID>COFFEE_MACHINE</ITEM_ID>
          <LIMIT_AMT>0.00000</LIMIT_AMT>
          <LIMIT_QTY>9999999999.000</LIMIT_QTY>
          <MERCH_SET_ID>0</MERCH_SET_ID>
          <PARENT_ELIGIB_ID>775</PARENT_ELIGIB_ID>
          <PROMO_RULE_ID>775</PROMO_RULE_ID>
          <ROOT_ELIGIB_ID>775</ROOT_ELIGIB_ID>
          <STATUS_TCD>AC</STATUS_TCD>
          <THRESHOLD_AMT>0.00000</THRESHOLD_AMT>
          <THRESHOLD_QTY>1.000</THRESHOLD_QTY>
          <THRESHOLD_TCD>QUTI</THRESHOLD_TCD>
          <TRANS_TOTAL_AMT>0.00000</TRANS_TOTAL_AMT>
          <TYPE_CODE>ITEM</TYPE_CODE>
          <UOM_ISO_CODE>PCE</UOM_ISO_CODE>
        </_-ROP_-E1_ELIGIBILITY>
        <_-ROP_-E1_PRICE_RULE SEGMENT="1">
          <CALCULATION_BASE>00</CALCULATION_BASE>
          <CALC_BASE_SEQUENCE>1-</CALC_BASE_SEQUENCE>
          <CONSIDER_PREVIOUS_RULES>X</CONSIDER_PREVIOUS_RULES>
          <COUPON_VALIDITY>0</COUPON_VALIDITY>
          <CURRENCY_ISO_CODE>EUR</CURRENCY_ISO_CODE>
          <DISC_METHOD_TCD>00</DISC_METHOD_TCD>
          <EX_PRICE_RULE_ID>0000000000000000000</EX_PRICE_RULE_ID>
          <ITEM_METHOD>00</ITEM_METHOD>
          <MAM_LIMIT_CNT>0.000</MAM_LIMIT_CNT>
          <NEW_PRICE_AMT>0.00000</NEW_PRICE_AMT>
          <NEW_SET_PRCE_AMT>0.00000</NEW_SET_PRCE_AMT>
          <PRICE_MODIF_AMT>100.00000</PRICE_MODIF_AMT>
          <PRICE_MODIF_CODE>RT</PRICE_MODIF_CODE>
          <PRICE_MODIF_PERC>0.000</PRICE_MODIF_PERC>
          <PRICE_RULE_ID>775</PRICE_RULE_ID>
          <REWARD_AS_POINTS>X</REWARD_AS_POINTS>
          <ROUNDING_METHOD>00</ROUNDING_METHOD>
          <ROUND_DECIMALS>0</ROUND_DECIMALS>
          <ROUND_DESTINATION>1</ROUND_DESTINATION>
          <RULE_CTRL_CODE>PO</RULE_CTRL_CODE>
          <STATUS_TCD>AC</STATUS_TCD>
          <SUBSEQ_NEW_PRICE_AMT>0.00000</SUBSEQ_NEW_PRICE_AMT>
          <SUBSEQ_PRICE_MODIF_AMT>0.00000</SUBSEQ_PRICE_MODIF_AMT>
          <SUBSEQ_PRICE_MODIF_PERC>0.000</SUBSEQ_PRICE_MODIF_PERC>
          <TYPE_CODE>RB</TYPE_CODE>
        </_-ROP_-E1_PRICE_RULE>
      </_-ROP_-E1_PROMOTION_RULE>
      <_-ROP_-E1_PROMOTION_TEXT SEGMENT="1">
        <DESCRIPTION>Buy a coffee machine and show coupon TIME FOR COFFEE to get 5 Euro off and 100 points</DESCRIPTION>
        <NAME>Coffee machine promotion</NAME>
        <SPRAS_ISO>EN</SPRAS_ISO>
      </_-ROP_-E1_PROMOTION_TEXT>
    </_-ROP_-E1_PROMOTION>
  </IDOC>
</_-ROP_-PROMOTION02>
```
7. Choose **Execute**.
8. Make sure you get the HTTP response **200**.

With this, you uploaded the promotional rule "Buy a coffee machine and show coupon TIME FOR COFFEE to get 100 Euro off" (promotion ID: `771`) for the **Heidelberg** store (business unit ID: `FC01`) and the validity period **September 1st, 2024 to September 30th, 2024**.

>In this tutorial, you only upload the promotional rule. You have already uploaded the regular sales price of **500 Euro** for the product **coffee machine** (material number: `COFFEE_MACHINE`, unit of measure: `PCE`) with the validity period **May 1st, 2021 to December 31st, 2021** in step 3 of the tutorial [Apply a Mix-and-Match Promotion with SAP Omnichannel Promotion Pricing](opps-advanced-scenario-mixandmatch).



### Check uploaded master data


You can use the **Data Access** service for checking your uploaded data. If you need guidance, have a look at step 4 of the tutorial [Apply a Simple Promotion with SAP Omnichannel Promotion Pricing](opps-basic-scenario).


### Additional Information

* [Official Product Documentation](https://help.sap.com/docs/OPP)
* [Trial Documentation](https://help.sap.com/docs/OPP/0c145d124b784b548b618cda8a5b2aba/31b8aedc8ce14fcd9f6021ad4f6323c9.html)
* [SAP Omnichannel Promotion Pricing on the SAP Business Accelerator Hub](https://help.sap.com/docs/OPP/7c87270e23c64c2aa922ce297a6df23d/67a9da084cf84e058cb3a3911971bdd0.html?version=Cloud)
* [Video: SAP Omnichannel Promotion Pricing in a nutshell](https://www.sap.com/assetdetail/2020/07/9060b3a5-a67d-0010-87a3-c30de2ffd8ff.html)

---
