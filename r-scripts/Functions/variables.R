NULL_STRING <- "null"
TRUE_STRING <- "true"
FALSE_STRING <- "false"


# Original Variables
APPLICATION_DATE <- "created"
SCORECARD_CREATED <- "scorecard_created"
HIGHEST_DPD_IN_LIVE_LOAN <- "highest_dpd_in_live_loan"
HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD <- "highest_dpd_in_loan_product_last_year_except_gold"
HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS <- "highest_dpd_in_loan_product_six_months"
SIXTY_DPD_LAST_YEAR <- "sixty_dpd_last_year"
ANY_WOFF_SUBSTANDARD_SUIT_FILED <- "any_woff_sub_standard_suit_filed"
OVERDUE_ACCOUNTS <- "overdue_accounts"
CIBIL_SCORE <- "cibil_score"
NO_OF_ENQUIRIES_LAST_6_MONTHS <- "number_of_enquiries_in_last_6_months"
BUREAU_HISTORY_SINCE <- "bureau_history_since"
DATE_OF_PULL <- "date_of_pull"
LOCATION_RISK <- "geo_portfolio_pl_index_band"
BORROWER_EMAIL <- "borrower_email"
EMAIL_EDIT_DISTANCE <- "emailMinEditDistanceToRecognisedDomains"
INCORPORATION_DATE <- "incorporation_date"
VINTAGE_MONTHS <- "vintage_months"
PROMOTER_DOB <- "promoter_dob"
RESIDENCE_OWNED <- "residenceOwned"
OFFICE_OWNED <- "officeOwned"

ANCHOR_MONTHLY_TS <- "anchor_average_monthly_transaction"
BANK_EOD_BALANCE <- "bank_statement_average_eod_balance"
HIGHEST_AMOUNT_BORROWED <- "highest_amount_borrowed_v1"

HIGHEST_DPD_IN_LIVE_LOAN_ORG_SCORE <- "daysPastDue"
HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD_ORG_SCORE <- "daysPastDueExceptGold"
HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS_ORG_SCORE <- "daysPastDueSixMonths"
SIXTY_DPD_LAST_YEAR_ORG_SCORE <- "sixtyDpdLastYear"
ANY_WOFF_SUBSTANDARD_SUIT_FILED_ORG_SCORE <- "woffSubstandardSuit"
OVERDUE_ACCOUNTS_ORG_SCORE <- "overdueAccounts"
CIBIL_SCORE_ORG_SCORE <- "cibilScore"
NO_OF_ENQUIRIES_LAST_6_MONTHS_ORG_SCORE <- "enquiriesSixMonths"
BUREAU_TENURE_ORG_SCORE <- "bureauTenure"
LOCATION_RISK_ORG_SCORE <- "geolocationIndexBand"
EMAIL_DOMAIN_ORG_SCORE <- "emailUniqueness"
BUSINESS_VINTAGE_ORG_SCORE <- "businessFirmographic"
PROMOTER_AGE_ORG_SCORE <- "promoterAge"
OWN_ADDRESS_ORG_SCORE <- "assetOwnership"

# Calculated variables
BUREAU_THICKNESS_THICK <- "thick"
BUREAU_THICKNESS_THIN <- "thin"

BUREAU_THICKNESS_CAL <- "BureauThicknessCal"

HIGHEST_DPD_IN_LIVE_LOAN_SCORE_CAL <- "highest_dpd_in_live_loan_score_cal"
HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD_SCORE_CAL <- "highest_dpd_in_loan_product_last_year_except_gold_score_cal"
HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS_SCORE_CAL <- "highest_dpd_in_loan_product_six_months_score_cal"
SIXTY_DPD_LAST_YEAR_VAR_SCORE <- "sixty_dpd_last_year_score_cal"
ANY_WOFF_SUBSTANDARD_SUIT_FILED_SCORE_CAL <- "any_woff_sub_standard_suit_filed_score_cal"
OVERDUE_ACCOUNTS_SCORE_CAL <- "overdue_accounts_score_cal"
CIBIL_SCORE_SCORE_CAL <- "cibil_score_score_cal"
NO_OF_ENQUIRIES_LAST_6_MONTHS_SCORE_CAL <- "number_of_enquiries_in_last_6_months_score_cal"
BUREAU_TENURE_SCORE_CAL <- "bureau_tenure_score_cal"
LOCATION_RISK_SCORE_CAL <- "location_risk_score_cal"
EMAIL_DOMAIN_SCORE_CAL <- "email_domain_score"
BUSINESS_VINTAGE_SCORE_CAL <- "business_vintage_score_cal"
PROMOTER_AGE_SCORE_CAL <- "promoter_age_score_cal"
OWN_ADDRESS_SCORE_CAL <- "own_address_score_cal"

HIGHEST_DPD_IN_LIVE_LOAN_WEIGHT_CAL <- "highest_dpd_in_live_loan_weight_cal"
HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD_WEIGHT_CAL <- "highest_dpd_in_loan_product_last_year_except_gold_weight_cal"
HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS_WEIGHT_CAL <- "highest_dpd_in_loan_product_six_months_weight_cal"
SIXTY_DPD_LAST_YEAR_VAR_WEIGHT <- "sixty_dpd_last_year_weight_cal"
ANY_WOFF_SUBSTANDARD_SUIT_FILED_WEIGHT_CAL <- "any_woff_sub_standard_suit_filed_weight_cal"
OVERDUE_ACCOUNTS_WEIGHT_CAL <- "overdue_accounts_weight_cal"
CIBIL_SCORE_WEIGHT_CAL <- "cibil_score_weight_cal"
NO_OF_ENQUIRIES_LAST_6_MONTHS_WEIGHT_CAL <- "number_of_enquiries_in_last_6_months_weight_cal"
BUREAU_TENURE_WEIGHT_CAL <- "bureau_tenure_weight_cal"
LOCATION_RISK_WEIGHT_CAL <- "location_risk_weight_cal"
EMAIL_DOMAIN_WEIGHT_CAL <- "email_domain_weight"
BUSINESS_VINTAGE_WEIGHT_CAL <- "business_vintage_weight_cal"
PROMOTER_AGE_WEIGHT_CAL <- "promoter_age_weight_cal"
OWN_ADDRESS_WEIGHT_CAL <- "own_address_weight_cal"

NEW_SCORE_CAL <- "new_score_cal"
NEW_SCORE_BAND_CAL <- "new_score_band_cal"
MODEL_DECISION <- "model_decision"

ANCHOR_MONTHLY_TS_LINE <- "anchor_monthly_ts_line"
BANK_EOD_BALANCE_LINE <- "bank_eod_balance_line"
HIGHEST_AMOUNT_BORROWED_LINE <- "highest_amount_borrowed_line"
LOAN_CAP_LINE <- "loan_cap_line"

LINE_CAL <- "line_cal"
LINE_REASON_CAL <- "line_reason_cal"
INTEREST_RATE_CAL <- "interest_rate_cal"

THICK_FILE_WEIGHTS_LIST <- list()
THICK_FILE_WEIGHTS_LIST[[HIGHEST_DPD_IN_LIVE_LOAN_WEIGHT_CAL]] = 0.05
THICK_FILE_WEIGHTS_LIST[[HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD_WEIGHT_CAL]] = 0.05
THICK_FILE_WEIGHTS_LIST[[HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS_WEIGHT_CAL]] = 0.03
THICK_FILE_WEIGHTS_LIST[[SIXTY_DPD_LAST_YEAR_VAR_WEIGHT]] = 0.04
THICK_FILE_WEIGHTS_LIST[[ANY_WOFF_SUBSTANDARD_SUIT_FILED_WEIGHT_CAL]] = 0.05
THICK_FILE_WEIGHTS_LIST[[OVERDUE_ACCOUNTS_WEIGHT_CAL]] = 0.04
THICK_FILE_WEIGHTS_LIST[[CIBIL_SCORE_WEIGHT_CAL]] = 0.12
THICK_FILE_WEIGHTS_LIST[[NO_OF_ENQUIRIES_LAST_6_MONTHS_WEIGHT_CAL]] = 0.1
THICK_FILE_WEIGHTS_LIST[[BUREAU_TENURE_WEIGHT_CAL]] = 0.1
THICK_FILE_WEIGHTS_LIST[[LOCATION_RISK_WEIGHT_CAL]] = 0.1
THICK_FILE_WEIGHTS_LIST[[EMAIL_DOMAIN_WEIGHT_CAL]] = 0.01
THICK_FILE_WEIGHTS_LIST[[BUSINESS_VINTAGE_WEIGHT_CAL]] = 0.15
THICK_FILE_WEIGHTS_LIST[[PROMOTER_AGE_WEIGHT_CAL]] = 0.1
THICK_FILE_WEIGHTS_LIST[[OWN_ADDRESS_WEIGHT_CAL]] = 0.06

THIN_FILE_WEIGHTS_LIST <- list()
THIN_FILE_WEIGHTS_LIST[[HIGHEST_DPD_IN_LIVE_LOAN_WEIGHT_CAL]] = 0.02
THIN_FILE_WEIGHTS_LIST[[HIGHEST_DPD_IN_LOAN_PRODUCT_LAST_YEAR_EXCEPT_GOLD_WEIGHT_CAL]] = 0.02
THIN_FILE_WEIGHTS_LIST[[HIGHEST_DPD_IN_LOAN_PRODUCT_SIX_MONTHS_WEIGHT_CAL]] = 0.02
THIN_FILE_WEIGHTS_LIST[[SIXTY_DPD_LAST_YEAR_VAR_WEIGHT]] = 0.02
THIN_FILE_WEIGHTS_LIST[[ANY_WOFF_SUBSTANDARD_SUIT_FILED_WEIGHT_CAL]] = 0.03
THIN_FILE_WEIGHTS_LIST[[OVERDUE_ACCOUNTS_WEIGHT_CAL]] = 0.02
THIN_FILE_WEIGHTS_LIST[[CIBIL_SCORE_WEIGHT_CAL]] = 0.0
THIN_FILE_WEIGHTS_LIST[[NO_OF_ENQUIRIES_LAST_6_MONTHS_WEIGHT_CAL]] = 0.05
THIN_FILE_WEIGHTS_LIST[[BUREAU_TENURE_WEIGHT_CAL]] = 0.02
THIN_FILE_WEIGHTS_LIST[[LOCATION_RISK_WEIGHT_CAL]] = 0.3
THIN_FILE_WEIGHTS_LIST[[EMAIL_DOMAIN_WEIGHT_CAL]] = 0.03
THIN_FILE_WEIGHTS_LIST[[BUSINESS_VINTAGE_WEIGHT_CAL]] = 0.22
THIN_FILE_WEIGHTS_LIST[[PROMOTER_AGE_WEIGHT_CAL]] = 0.15
THIN_FILE_WEIGHTS_LIST[[OWN_ADDRESS_WEIGHT_CAL]] = 0.1

