import pandas as pd

# https://github.com/dsfsi/covid19za
province_rename_dict = {
    'EC': 'Eastern Cape', 
    'FS': 'Free State', 
    'GP': 'Gauteng', 
    'KZN': 'KwaZulu-Natal', 
    'LP': 'Limpopo', 
    'MP': 'Mpumalanga', 
    'NC': 'Nothern Cape', 
    'NW': 'North West', 
    'WC': 'Western Cape', 
    }
covid_case = pd.read_csv(r'..\Data\covid19za_provincial_cumulative_timeline_confirmed.csv') \
    .rename(columns=province_rename_dict)
covid_case = covid_case.iloc[:, 1:11]
covid_case = covid_case.rename(columns={'YYYYMMDD': 'date'}).set_index('date')

# Calculate daily new cases from total cases number
covid_case = covid_case.diff(1)
covid_case