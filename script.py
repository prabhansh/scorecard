from django.db import connection

ap_columns = "ap.amount as amount, ap.\"creditAnalysis\"->'borrower'->'details'->>'amount' as loan_amount ," \
             " ap.\"creditAnalysis\"->'borrower'->'details'->>'duration' as loan_duration ,ap.\"requestId\" ," \
             "ap.created ,ap.is_topup ,ap.anchor_loan_type_id " \
             ",ap.\"creditAnalysis\"->'borrower'->'anchor_relation'->>'vintage' as anchor_vintage ," \
             "ap.\"creditAnalysis\"->'borrower'->'anchor_relation'->>'average_monthly_transaction_6_months'" \
             " as average_anchor_monthly_txns ," \
             "ap.\"creditAnalysis\"->'borrower'->'anchor_relation'->>'average_monthly_transaction' as " \
             "average_anchor_monthly_txns_all_time, ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'growth' " \
             "as banking_turnover, ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'average_eod_balance' " \
             "as avg_eod_bank_balance, ap.\"creditAnalysis\"->'borrower'->'tax_return'->>'total_income' " \
             "as profit_before_tax, ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->>'business_nature'" \
             " as business_nature, ap.\"creditAnalysis\"->'borrower'->'details'->'details'->>'agent_type'" \
             " as anchor_agent_type, ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->>'industry' as " \
             "margin_industry_depreceated, ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->>'sub_industry'" \
             " as margin_sub_industry, ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->>'business_category'" \
             " as business_category, " \
             "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'inward_cheque_bounces_3_months' as" \
             " inward_cheque_bounces, " \
             "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'outward_cheque_bounces_3_months' " \
             "as outward_cheque_bounces, " \
             "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'total_txns_credit_3_months' " \
             "as total_credit_txns, ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'total_credits'" \
             " as total_credits, ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'total_months' " \
             "as total_months, an.industry as industry, an.name as anchor_name , " \
             "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'average_eod_balance_6_months' as" \
             " average_eod_balance_6_months, " \
             "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'average_od_eod_balance_6_months' as " \
             "average_od_eod_balance_6_months , " \
             "ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->>'segment_category_id' as segment_category_id, " \
             "ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->'running_loan_emi_amount' as emi_running, " \
             "ap.\"creditAnalysis\"->'borrower'->'details'->'meta'->'fixed_expenses' as fixed_expenses , " \
             "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->>'unique_bank_accounts' as unique_bank_accounts "

bs_vars = "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'is_od' as is_od, " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'average_eod_balance_12_months' as avg_eod ,  " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'average_eod_balance_3_months' as eod_l3," \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'weighted_average_eod_balance_3_months' as eod_l3_w, " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'weighted_average_non_od_account_eod_balance_3_months'" \
          "as eod_l3_w_non_od_N ," \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'stability_eod' as stability_eod, " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'average_credits_12_months' as avg_credits, " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'average_credits_3_months' as credits_l3, " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'weighted_average_credits_3_months' as credits_l3_w," \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'macro_growth' as credits_macro_growth," \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'micro_growth' as credits_micro_growth, " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'stability_credits' as stability_credits , " \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'number_of_penal_transactions' as penal_transactions," \
          "ap.\"creditAnalysis\"->'borrower'->'bank_statement'->'inward_cheque_bounces_12_months' as" \
          " inward_cheque_bounces_12M "

br_columns = "br.\"is_key_promoter\" ,br.relationship "

be_columns = "be.primary_contact->'email' as email ," \
              "be.primary_address, be.primary_address->'isOwned' as isOwned," \
              " be.primary_address->'property_type' as property_type," \
              " be.\"details\"->>'override_emi_obligation' as override_emi_obligation, " \
              "be.details, " \
              "be.type as business_entity_type, " \
              "be.\"details\"->>'business_type' as business_type, " \
              "be.\"details\"->>'total_od_limit' as total_od_limit , be.kyc->>'pan_id' as pan_id"

ca_columns = "ca.\"ownerId\" , ca.details->> 'date_of_pull' as date_of_pull," \
              " ca.details->> 'version' as version," \
              " ca.created as cibil_created," \
              " ca.details->> 'email' as email_cibil, " \
              "ca.details->> 'gender' as gender, " \
              "ca.details->> 'dob' as dob, " \
              "ca.details->> 'cibil_score' as cibil_score, " \
              "ca.details->> 'enquiries' as enquiries, " \
              "ca.details->> 'bureau_history_since' as bureau_history, " \
              "ca.details->> 'accounts' as accounts, " \
              "ca.details->> 'employment' as employment, " \
              "ca.details->> 'highest_dpd_in_loan_product_last_two_years_except_gold' as " \
              "highest_dpd_in_loan_product_last_two_years_except_gold_v2, " \
              "ca.details->> 'highest_dpd_in_loan_product_six_months' as highest_dpd_in_loan_product_six_months_v2, " \
              "ca.details->> 'any_woff_sub_standard_suit_filed' as any_woff_sub_standard_suit_filed_v1, " \
              "ca.details->> 'sixty_dpd_last_year' as sixty_dpd_last_year_v1, " \
              "ca.details->> 'highest_dpd_in_live_loan' as highest_dpd_in_live_loan, " \
              "ca.details->> 'overdue_accounts' as overdue_accounts_v1, " \
              "ca.details->> 'number_of_enquiries_in_last_6_months' as number_of_enquiries_in_last_6_months_v1, " \
              "ca.details->> 'highest_dpd_in_loan_product_last_year_except_gold' as " \
              "highest_dpd_in_loan_product_last_year_except_gold_v1"

st_columns = "st.updated_at as application_date"

mam_columns = "mam.anchor_data as fast_loan_anchor_data, " \
              "mam.anchor_name as fast_loan_anchor_name, " \
              "mam.business_category as fast_loan_business_category"

bema_columns = "bema.pre_approved_amount as instant_credit_approved_amount, " \
               "bema.merchant_id as merchant_id"

vrp_columns = "vrp.url as vertical_risk_profile_url, " \
               "vrp.vertical_band as vertical_risk_profile_band"

request_id = 71167
query = 'select {},{},{},{},{},{},{},{} from applications as ap left join anchors as an ' \
        'on ap."anchorId"=an.id ' \
        'left join business_relationship br on br.business_request_id = ap.business_request_id ' \
        'left join business_entity be on be.id = br.business_entity_id ' \
        'left join cibil_analysis ca on ca.\"ownerId\" = br.business_entity_id ' \
        'left join business_entity_merchant_accounts bema on bema.business_entity_id = be.id ' \
        'and bema.anchor_id = ap.\"anchorId\" ' \
        'left join merchant_id_anchor_data_mapping mam on mam.merchant_id = bema.merchant_id' \
        ' and an.name = mam.anchor_name ' \
        'left join vertical_risk_profile vrp on be.details->>"online_platform_url" = vrp.url' \
        ' where ap."requestId"={} limit 1'.format(
            ap_columns, bs_vars, br_columns, be_columns,
            ca_columns, mam_columns, bema_columns, vrp_columns, request_id)


def dictfetchall(cursor):
    "Return all rows from a cursor as a dict"
    columns = [col[0] for col in cursor.description]
    return [
        dict(zip(columns, row))
        for row in cursor.fetchall()
    ]


with connection.cursor() as cursor:
    cursor.execute(query);
    data = dictfetchall(cursor)
    print(data)


