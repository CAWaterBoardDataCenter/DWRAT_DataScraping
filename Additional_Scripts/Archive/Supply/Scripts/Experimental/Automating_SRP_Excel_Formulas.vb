'Main Formula on Climate_stresses_update_2024 sheet
=CONCATENATE(
Updater!A28094," ", '4 spaces wide
Updater!B28094," ", '2 spaces wide
Updater!C28094," ", '2 spaces wide
Updater!D28094," ", '1 space widfe
Updater!E28094," ", '1 space wide
Updater!F28094,"     ", '1 space wide
TEXT(Updater!G28094,"0.0000"),"     ", '5 spaces, formats precip01 with 4 decimal places
TEXT(Updater!H28094,"0.0000"),"    ", '5 spaces, formats precip02 with 4 decimal places
TEXT(Updater!I28094,"0.0000"),"    ", '6 spaces, formats tmax01 with 4 decimal places
TEXT(Updater!J28094,"0.0000"),"    ", '6 spaces, formats tmax02 with 4 decimal places
TEXT(Updater!K28094,"0.0000"),"    ", '6 spaces, formats tmin01 with 4 decimal places
TEXT(Updater!L28094,"0.0000"), '6 spaces, formats tmin02 with 4 decimal places
)

'DATA DICTIONARY FOR FORMULA ABOVE
' Updater!A28094 is the 4-digit year
' Updater!B28094 is the 2-digit Month
' Updater!C28094 is the calendar day from 1 to 31
' Updater!D28094 indicates hour but is always 0
' Updater!E28094 indicates minutes but is always 0
' Updater!F28094 indicates seconds but is always 0
' Updater!G28094 indicates precip01, fed by PRISM_CIMIS_083_Interp ppt column for observed data range; rest of range comes from pre-existing SRP dat file
' Updater!H28094 indicates precip02  fed by PRISM_CIMIS_103_Interp ppt column for observed data range; rest of range comes from pre-existing SRP dat file
' Updater!I28094 indicates tmax01, comes from PRISM_CIMIS_083_Interp tmax column for observed data range; rest of the range is in the pre-existing SRP Dat file
' Updater!J28094 indicates tmax02, comes from PRISM_CIMIS_103_Interp tmax column for observed data range; rest of the range is in the pre-existing SRP Dat file
' Updater!K28094 indicates tmin01, comes from PRISM_CIMIS_083_Interp tmax column for observed data range; rest of the range is in the pre-existing SRP Dat file
' Updater L28094 indicates tmin02, comes from PRISM_CIMIS_103_Interp tmax column for observed data range; rest of the range is in the pre-existing SRP Dat file


'VARIOUS FORMULAS on Updater Sheet
[Climate_stresses_update_2024.xlsx]Updater!$G$28104 'fed by CIMIS_083 ppt
[Climate_stresses_update_2024.xlsx]Updater!$H$27627 'fed by CIMIS_103 ppt
[Climate_stresses_update_2024.xlsx]Updater!$I$28104 'fed by CIMIS_083 tmax
[Climate_stresses_update_2024.xlsx]Updater!$J$27627 'fed by CIMIS_103 tmax
[Climate_stresses_update_2024.xlsx]Updater!$K$28104 'fed by CIMIS_083 tmin
[Climate_stresses_update_2024.xlsx]Updater!$L$27627 'fed by CIMIS_103 tmin


'Save SRP Dat headers as a separate file, Dat_SRP_header.csv in the InputData Folder
	* Import into R script as Dat_SRP_header

'Import Pre-existing SRP DAT file into R
'Determine SRP Dat file column widths
	year foll

'Converting the Concatenation formula to R
* Assign variable names to all the columns
	A = year, from 1947 to 2024; 12 instances of each year
	B = month, repeats 1 to 12 for each year, except 1947 and 2024; months 10, 11, and 12 only in 1947,
		months 1-9 only in 2024
	C = days from 1 to 31 for each month for year in the sequence
	D = 1-column vector of entirely 0s
	E = 1-column vector of entirely 0s
	F = 1-column vector of entirely 0s
	
