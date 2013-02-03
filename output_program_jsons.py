import ujson
import numpy as np
import pandas as pd
import re

df = pd.read_csv('table2.csv')

deposits = df[df['type']=='deposit']
withdrawals = df[df['type']=='withdrawal']

programs = ['Agriculture Loan Repayments (misc)', 'Air Transport Security Fees', "Cash FTD's Received (Table IV)",
    'Commodity Credit Corporation programs', 'Customs and Certain Excise Taxes', 'Supplemental Security Income', 'Unemployment',
    'Education Department programs', 'Energy Department programs', 'Estate and Gift Taxes', 'Federal Reserve Earnings',
    'Foreign Deposits, Military Sales', 'Housing and Urban Development programs', 'Individual Income and Employment Taxes Not Withheld',
    'Interest recd from cash investments', 'Justice Department programs', 'Postal Service', 'Public Debt Cash Issues (Table III-B)',
    'Defense Finance & Accounting Service', 'Federal Housing Admin: Note Sales', 'Railroad Unemployment Ins.', 'TARP']

dir_name = '/Users/bjdewilde/Dropbox/FMS/program_jsons/'
for program in programs:
    dates_values = []
    dates = list(deposits[deposits['item'] == program]['date'])
    values = list(deposits[deposits['item'] == program]['today'])
    for date in dates:
        value = values[dates.index(date)]
        dates_values.append([date, int(value)])
    f_name = re.sub(r'\s+', '_', program.lower())
    f_name = re.sub(r",|'|\.", '', f_name)
    print f_name
    f = open(dir_name + f_name + '_deposits.js', 'w')
    f.write(ujson.dumps(dates_values))
    f.close()

programs = ['Commodity Credit Corporation programs', 'Defense Vendor Payments (EFT)', 'Education Department programs',
    'Energy Department programs', 'Federal Employees Insurance Payments', 'Fed. Highway Administration programs',
    'Federal Salaries (EFT)', 'Food and Nutrition Service (misc)', 'GSA programs', 'Health and Human Services Grants (misc)',
    'Housing and Urban Development programs', 'Interest on Treasury Securities', 'IRS Tax Refunds Business (EFT)',
    'IRS Tax Refunds Individual (EFT)', 'Justice Department programs', 'Labor Dept. prgms (excl. unemployment)',
    'Medicaid', 'Medicare', 'NASA programs', 'Postal Service Money Orders and Other', 'Public Debt Cash Redemp. (Table III-B)',
    'Social Security Benefits (EFT)', 'Supple. Nutrition Assist. Program (SNAP)', 'Temporary Assistance for Needy Families (HHS)',
    'Unemployment Insurance Benefits', 'Veterans Affairs programs', 'Agriculture', 'Emergency Prep & Response (DHS)',
    'Federal Crop Ins. Corp.', 'Thrift Savings Plan Transfer']

dir_name = '/Users/bjdewilde/Dropbox/FMS/program_jsons/'
for program in programs:
    dates_values = []
    dates = list(withdrawals[withdrawals['item'] == program]['date'])
    values = list(withdrawals[withdrawals['item'] == program]['today'])
    for date in dates:
        value = values[dates.index(date)]
        dates_values.append([date, int(value)])
    f_name = re.sub(r'\s+', '_', program.lower())
    f_name = re.sub(r",|'|\.", '', f_name)
    print f_name
    f = open(dir_name + f_name + '_withdrawals.js', 'w')
    f.write(ujson.dumps(dates_values))
    f.close()
