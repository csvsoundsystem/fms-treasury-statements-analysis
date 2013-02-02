import datetime
import numpy as np
import matplotlib.pyplot as plt
import os
import pandas as pd
import re
import StringIO

OPEN_DIR_NAME = '/Users/bjdewilde/Dropbox/FMS/Scraped_txt_files_2013_to_2005/'
SAVE_DIR_NAME = '/Users/bjdewilde/Desktop/hackathon/data/'
BASE_URL = 'https://www.fms.treas.gov/fmsweb/viewDTSFiles?dir=w&fname='
save_to_csv = True

def clean_text(item):
	item = item.strip()
	item = re.sub(r'^\$\s*|,|r\/', '', item)
	return item

def get_date_and_day(f_name):
	raw_date = re.search(r'(\d+).txt', f_name).group(1)
	date = datetime.date(2000+int(raw_date[0:2]), int(raw_date[2:4]), int(raw_date[4:6]))
	day = datetime.datetime.strftime(date, '%A')
	return date, day

def get_table_name(page):
	try:
		table_line = re.search(r'\s+TABLE.*', page).group()
		table = re.sub(r'\|', '', table_line)
		table = re.sub(r'\s+', ' ', table)
	except AttributeError:
		table = ''
	return table

def get_footnotes(item):
    match = re.search(r'^\d\/', str(item))
    if match:
        return match.group()
    else:
        return 0

def handle_footnotes(i):
	#fwfs[i]['footnote'] = fwfs[i]['today'].apply(get_footnotes)
	fwfs[i]['footnote'] = 0
	for col in ['today', 'mtd', 'fytd']:
		fwfs[i][col] = fwfs[i][col].str.replace(r'\d\/', '')
	footnote_inds = fwfs[i]['item'][fwfs[i]['item'].str.contains(r'\d\/')].index
	if len(footnote_inds) > 0:
		for footnote_ind in footnote_inds:
			footnote = str(fwfs[i].ix[footnote_ind,'item']) + str(fwfs[i].ix[footnote_ind,'today']) + str(fwfs[i].ix[footnote_ind,'mtd']) + str(fwfs[i].ix[footnote_ind,'fytd'])
			try:
				if not re.search(r'\d\/', fwfs[i].ix[footnote_ind+1,'item']):
					footnote = footnote + str(fwfs[i].ix[footnote_ind+1,'item']) + str(fwfs[i].ix[footnote_ind+1,'today']) + str(fwfs[i].ix[footnote_ind+1,'mtd']) + str(fwfs[i].ix[footnote_ind+1,'fytd'])
					footnote = footnote.replace('nan', '')
					fwfs[i] = fwfs[i].drop(footnote_ind+1)
			except KeyError:
				pass
			fwfs[i] = fwfs[i].drop(footnote_ind)
			#print fwfs[i]['footnote'] != 0
			#fwfs[i]['footnote'][fwfs[i]['footnote'] != 0] = footnote
	return fwfs[i]

def table_2_merge_and_drop_rows(i):
	null_row_inds = fwfs[i][fwfs[i]['item'].isnull() & fwfs[i]['today'].isnull() & fwfs[i]['mtd'].isnull() & fwfs[i]['fytd'].isnull()].index
	if len(null_row_inds) > 0:
		for null_row_ind in null_row_inds:
			fwfs[i] = fwfs[i].drop(null_row_ind)
	two_line_ind = fwfs[i][fwfs[i]['item'].notnull() & fwfs[i]['today'].isnull() & fwfs[i]['mtd'].isnull() & fwfs[i]['fytd'].isnull() & ~fwfs[i]['item'].str.endswith(':') & ~fwfs[i]['item'].str.endswith('.')].index #& ~fwfs[i]['item'].str.contains('Revised')].index
	if len(two_line_ind) > 0:
		for line_ind in two_line_ind:
			account = fwfs[i].ix[line_ind, 'item']
			try:
				fwfs[i].ix[line_ind+1, 'item'] = fwfs[i].ix[line_ind, 'item'] + ' ' + fwfs[i].ix[line_ind+1, 'item']
			except KeyError:
				pass
			fwfs[i] = fwfs[i].drop(line_ind)
	header_line_ind = fwfs[i][fwfs[i]['item'].notnull() & fwfs[i]['today'].isnull() & fwfs[i]['mtd'].isnull() & fwfs[i]['fytd'].isnull() & fwfs[i]['item'].str.endswith(':')].index
	if len(header_line_ind) > 0:
		for line_ind in header_line_ind:
			fwfs[i] = fwfs[i].drop(line_ind)
	return fwfs[i]

def table_3_merge_and_drop_rows(i):
	null_row_inds = fwfs[i][fwfs[i]['item'].isnull() & fwfs[i]['today'].isnull() & fwfs[i]['mtd'].isnull() & fwfs[i]['fytd'].isnull()].index
	if len(null_row_inds) > 0:
		for null_row_ind in null_row_inds:
			fwfs[i] = fwfs[i].drop(null_row_ind)
	return fwfs[i]


f_names_all = os.listdir('/Users/bjdewilde/Dropbox/FMS/Scraped_txt_files_2013_to_2005/')
f_names = f_names_all[:-999]
#f_names = ['08010300.txt']
for f_name in reversed(f_names):

	print "\nf_name =", f_name, "( index =", f_names_all.index(f_name), ")"

	url = BASE_URL + f_name
	date = get_date_and_day(f_name)[0]
	day = get_date_and_day(f_name)[1]
	print "date =", date

	f = open(OPEN_DIR_NAME + f_name, 'r').read()
	pages = []
	# get rid of empty lines
	for page in re.split(r'\d.*DAILY TREASURY STATEMENT.*PAGE:\s+\d\s{2}', f):
		pages.append(re.sub(r'[\r\n]{2,}', '\r\n', page))

	fwfs = {}

	# PAGE: 1
	# ___________________________________________________________________________________________
    #  TABLE I  Operating Cash Balance
	# ___________________________________________________________________________________________
    #                                                             Opening balance
    #                                       Closing   ______________________________________
    #        Type of account                balance                    This         This
    #                                        today        Today        month       fiscal
    #                                                                               year
	# ____________________________________________________________________________________________
	# skiprows = 12
	# if date > datetime.date(2012, 5, 31):
	# 	cols = [(1,40), (40,53), (53,66), (66,79), (79,92)]
	# else:
	# 	cols = [(1,32), (32,48), (48,64), (64,80), (80,92)]
	# if date < datetime.date(2008, 1, 4):
	# 	skiprows = 13
	# fwfs[1] = pd.read_fwf(StringIO.StringIO(str(pages[1])), skiprows=skiprows, thousands=',', #skipfooter=2, 
	# 	colspecs=cols,
	# 	names=['account', 'close_today', 'open_today', 'open_mo', 'open_fy'],
	# 	converters={'account':clean_text, 'close_today':clean_text, 'open_today':clean_text, 'open_mo':clean_text, 'open_fy':clean_text})
	# null_row_inds = fwfs[1][fwfs[1]['account'].isnull() & fwfs[1]['close_today'].isnull() & fwfs[1]['open_today'].isnull() & fwfs[1]['open_mo'].isnull() & fwfs[1]['open_fy'].isnull()].index
	# if len(null_row_inds) > 0:
	# 	for null_row_ind in null_row_inds:
	# 		fwfs[1] = fwfs[1].drop(null_row_ind)
	# divider_row_inds = fwfs[1][fwfs[1]['account'].str.contains('_____')].index
	# if len(divider_row_inds) > 0:
	# 	for divider_row_ind in divider_row_inds:
	# 		fwfs[1] = fwfs[1].drop(divider_row_ind)
	# if date > datetime.date(2013, 1, 3) or date < datetime.date(2012, 6, 1):
	# 	two_line_ind = fwfs[1][fwfs[1]['account'].notnull() & fwfs[1]['close_today'].isnull() & fwfs[1]['open_today'].isnull() & fwfs[1]['open_mo'].isnull() & fwfs[1]['open_fy'].isnull() & ~fwfs[1]['account'].str.endswith(':')].index
	# 	if len(two_line_ind) > 0:
	# 		for line_ind in two_line_ind:
	# 			account = fwfs[1].ix[line_ind, 'account']
	# 			fwfs[1].ix[line_ind+1, 'account'] = fwfs[1].ix[line_ind, 'account'] + ' ' + fwfs[1].ix[line_ind+1, 'account']
	# 			fwfs[1] = fwfs[1].drop(line_ind)
	# else:
	# 	two_line_ind = fwfs[1][fwfs[1]['account'].notnull() & fwfs[1]['close_today'].isnull() & fwfs[1]['open_today'].isnull() & fwfs[1]['open_mo'].isnull() & fwfs[1]['open_fy'].isnull() & ~fwfs[1]['account'].str.endswith(':')].index
	# 	if len(two_line_ind) > 0:
	# 		for line_ind in two_line_ind:
	# 			account = fwfs[1].ix[line_ind-1, 'account']
	# 			fwfs[1].ix[line_ind-1, 'account'] = fwfs[1].ix[line_ind-1, 'account'] + ' ' + fwfs[1].ix[line_ind, 'account']
	# 			fwfs[1] = fwfs[1].drop(line_ind)
	# fwfs[1]['source'] = 'Daily Treasury Statement'
	# fwfs[1]['url'] = url
	# fwfs[1]['date'] = str(date)
	# fwfs[1]['day'] = day
	# fwfs[1]['table'] = get_table_name(pages[1])
	# fwfs[1]['is_total'] = fwfs[1]['account'].str.startswith('Total').astype(np.int16)
	# if save_to_csv:
	# 	fwfs[1].to_csv(SAVE_DIR_NAME + 'csv/table1/' + f_name[:-4] + '_t1.csv',
	# 		cols=['source', 'url', 'date', 'day', 'table', 'account', 'is_total', 'close_today', 'open_today', 'open_mo', 'open_fy'], index=False)
	# #print fwfs[1]

	# PAGE: 2
	# ___________________________________________________________________________________________
    #  TABLE II  Deposits and Withdrawals of Operating Cash
	# ___________________________________________________________________________________________
    #                                                                 This         Fiscal
    #                 Deposits                          Today         month         year
    #                                                                to date      to date
	# ____________________________________________________________________________________________
	skiprows=11
	if date > datetime.date(2012, 5, 31):
		cols = [(0,51), (51,65), (65,79), (79,92)]
	else:
		cols = [(0,46), (46,61), (61,76), (76,92)]
	if date < datetime.date(2008, 1, 4):
		skiprows = 12
	fwfs[2] = pd.read_fwf(StringIO.StringIO(str(pages[2])), skiprows=skiprows, thousands=',', #skipfooter=2, 
		colspecs=cols,
		names=['item', 'today', 'mtd', 'fytd'],
		converters={'item':clean_text, 'today':clean_text, 'mtd':clean_text, 'fytd':clean_text})
	fwfs[2] = table_2_merge_and_drop_rows(2)
	fwfs[2]['source'] = 'Daily Treasury Statement'
	fwfs[2]['url'] = url
	fwfs[2]['date'] = str(date)
	fwfs[2]['day'] = day
	fwfs[2]['table'] = get_table_name(pages[2])
	fwfs[2]['account'] = 'Federal Reserve Account'
	fwfs[2]['type'] = 'deposit'
	fwfs[2]['subtype'] = ''
	fwfs[2]['is_total'] = fwfs[2]['item'].str.startswith('Total').astype(np.int16)
	fwfs[2] = handle_footnotes(2)

	# PAGE: 3
	#___________________________________________________________________________________________
    #                                                                 This         Fiscal
    #                Withdrawals                        Today         month         year
    #                                                                to date      to date
	#____________________________________________________________________________________________
	skiprows=9
	if date < datetime.date(2008, 1, 4):
		skiprows = 11
	fwfs[3] = pd.read_fwf(StringIO.StringIO(str(pages[3])), skiprows=skiprows, thousands=',', #skipfooter=2, 
		colspecs=cols,
		names=['item', 'today', 'mtd', 'fytd'],
		converters={'item':clean_text, 'today':clean_text, 'mtd':clean_text, 'fytd':clean_text})
	fwfs[3] = table_2_merge_and_drop_rows(3)
	fwfs[3]['source'] = 'Daily Treasury Statement'
	fwfs[3]['url'] = url
	fwfs[3]['date'] = date
	fwfs[3]['day'] = day
	fwfs[3]['table'] = get_table_name(pages[2])
	fwfs[3]['account'] = 'Federal Reserve Account'
	fwfs[3]['type'] = 'withdrawal'
	fwfs[3]['subtype'] = ''
	fwfs[3]['is_total'] = fwfs[3]['item'].str.startswith('Total').astype(np.int16)
	fwfs[3] = handle_footnotes(3)

	# TABLE 2 MERGE	
	fwfs['t2'] = pd.concat([fwfs[2], fwfs[3]], axis=0, join='inner', ignore_index=True)
	if save_to_csv:
		fwfs['t2'].to_csv(SAVE_DIR_NAME + 'csv/table2/' + f_name[:-4] + '_t2.csv',
			cols=['source', 'url', 'date', 'day', 'table', 'account', 'item', 'type', 'subtype', 'is_total', 'today', 'mtd', 'fytd', 'footnote'],
			index=False)


	# PAGE: 4
	# ___________________________________________________________________________________________
	#      TABLE III-A  Public Debt Transactions
	# ___________________________________________________________________________________________
	#                                                                     This         Fiscal
	#                      Issues                           Today         month         year
	#                                                                    to date      to date
	#____________________________________________________________________________________________
	#fwfs[4] = pd.read_fwf(StringIO.StringIO(str(pages[4])), skiprows=11, skipfooter=2, thousands=',',
	#	colspecs=[(0,51), (51,65), (65,79), (79,92)],
	#	names=['item', 'today', 'mtd', 'fytd'],
	#	converters={'item':clean_text, 'today':clean_text, 'mtd':clean_text, 'fytd':clean_text})
	#fwfs[4] = table_3_merge_and_drop_rows(4)
	#fwfs[4]['source'] = 'Daily Treasury Statement'
	#fwfs[4]['url'] = url
	#fwfs[4]['date'] = date
	#fwfs[4]['day'] = day
	#fwfs[4]['table'] = get_table_name(pages[4])
	#fwfs[4]['type'] = 'issues' or 'redemptions'
	#fwfs[4]['subtype'] = ''
	#fwfs[4]['is_total'] = fwfs[4]['item'].str.startswith('Total').astype(np.int16)
	#fwfs[4]['footnotes'] = ''






