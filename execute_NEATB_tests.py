import csv
import statistics
import sys
import argparse
import os
from pyswip import Prolog
from pyswip.prolog import PrologError

# 1. File .pl da caricare
PROLOG_CORE_FILE = 'neabt_core_lists.pl'
PROLOG_IO_FILE = 'neabt_io_lists.pl'

# 2. Configurazione Efficienza
EFFICIENCY_RUNS = 100
EFFICIENCY_FILES = [
    'NEABT_input_100.pl',
    'NEABT_input_500.pl',
    'NEABT_input_1000.pl',
    'NEABT_input_2000.pl',
    'NEABT_input_5000.pl',
    'NEABT_input_10000.pl'
]
EFFICIENCY_REPORT_FILE = 'efficiency_report.csv'

# 3. Configurazione Effectiveness
EFFECTIVENESS_TESTS = {
    "user_safety": ["user", ["driving_safety"]],
    "user_experience": ["user", ["driving_experience"]],
    "env_speed": ["env", ["speed"]],
    "domain_weather": ["domain", ["weather_conditions"]],
    "domain_danger": ["domain", ["road_dangerousness"]],
    "env_speed_traffic": ["env", ["speed", "traffic_status"]],
    "user_all": ["user", ["crashes_last_year", "address", "driving_safety", "driving_experience"]]
}
EFFECTIVENESS_REPORT_FILE = 'effectiveness_report.csv'

# 4. Nome del file per il report dei test aggiuntivi
ADDITIONAL_EFFECTIVENESS_REPORT_FILE = 'effectiveness_report_10k_additional.csv'

# 5. Configurazione Effectiveness Aggiuntiva (solo 10k)
EFFECTIVENESS_10K_FILE = 'NEABT_input_10000.pl'
ADDITIONAL_EFFECTIVENESS_TESTS = {
    "env_traffic_status": ["env", ["traffic_status"]],
    "env_driving_hours": ["env", ["driving_hours"]],
    "env_stops": ["env", ["stops"]],
    "env_avg_stop": ["env", ["avg_stop_duration"]],
    "env_location": ["env", ["location"]],
    "user_crashes": ["user", ["crashes_last_year"]],
    "user_address": ["user", ["address"]],
    "domain_road_cond": ["domain", ["road_conditions"]],
    "env_all": ["env", [
        "traffic_status", "driving_hours", "speed", 
        "stops", "avg_stop_duration", "location"
    ]],
    "domain_all": ["domain", [
        "road_conditions", "weather_conditions", "road_dangerousness"
    ]]
}


def setup_prolog():
    """Inizializza Prolog e carica gli script necessari."""
    print("Inizializzazione SWI-Prolog e caricamento script...")
    prolog = Prolog()
    
    try:
        prolog.consult(PROLOG_CORE_FILE)
        prolog.consult(PROLOG_IO_FILE)
        print(f"Caricati: {PROLOG_CORE_FILE}, {PROLOG_IO_FILE}\n")
        return prolog
    except PrologError as e:
        print(f"ERRORE: Impossibile caricare i file Prolog: {e}")
        print("Assicurati che i file .pl siano nella stessa cartella e che SWI-Prolog sia installato.")
        sys.exit(1)

def run_efficiency_tests(prolog):
    """Esegue i test di efficienza 100 volte e salva i risultati."""
    print(f"--- Avvio Test Efficienza ({EFFICIENCY_RUNS} esecuzioni) ---")
    
    all_results = {file_name: {'load_ms': [], 'exec_ms': []} for file_name in EFFICIENCY_FILES}
    
    prolog_file_list = str(EFFICIENCY_FILES)

    try:
        with open(EFFICIENCY_REPORT_FILE, 'w', newline='') as f:
            writer = csv.writer(f, delimiter=';') 
            
            writer.writerow(['Run', 'File', 'n_obs', 'load_ms', 'exec_ms'])
            
            for i in range(EFFICIENCY_RUNS):
                print(f"  Esecuzione Efficienza {i + 1}/{EFFICIENCY_RUNS}...")
                
                query = f"run_efficiency({prolog_file_list}, Report)."
                result = list(prolog.query(query))
                
                if not result:
                    print(f"ERRORE: La query di efficienza non ha restituito risultati (Run {i+1}).")
                    continue

                report = result[0]['Report']
                
                for row in report:
                    file_name = row[1]
                    n_obs = row[3]
                    load_ms = row[5]
                    exec_ms = row[7]
                    
                    writer.writerow([i + 1, file_name, n_obs, load_ms, exec_ms])
                    
                    if file_name in all_results:
                        all_results[file_name]['load_ms'].append(load_ms)
                        all_results[file_name]['exec_ms'].append(exec_ms)

            print(f"\n  Dati grezzi salvati in {EFFICIENCY_REPORT_FILE}")

            writer.writerow([])
            writer.writerow(['--- Valori Medi ---'])
            writer.writerow(['File', 'n_obs', 'Avg_Load_ms', 'Avg_Exec_ms'])
            
            for file_name in EFFICIENCY_FILES:
                n_obs = file_name.split('_')[2].split('.')[0]
                avg_load = statistics.mean(all_results[file_name]['load_ms'])
                avg_exec = statistics.mean(all_results[file_name]['exec_ms'])
                
                avg_load_str = f"{avg_load:.2f}".replace('.', ',')
                avg_exec_str = f"{avg_exec:.2f}".replace('.', ',')
                
                writer.writerow([file_name, n_obs, avg_load_str, avg_exec_str])
                
            print(f"  Valori medi aggiunti a {EFFICIENCY_REPORT_FILE}")

    except PrologError as e:
        print(f"ERRORE durante l'esecuzione dell'efficienza: {e}")
    except Exception as e:
        print(f"ERRORE (Python): {e}")

def run_effectiveness_tests(prolog):
    """Esegue i test di effectiveness standard su tutti i file e salva i risultati."""
    print(f"\n--- Avvio Test Effectiveness (Standard) ---")
    
    try:
        with open(EFFECTIVENESS_REPORT_FILE, 'w', newline='') as f:
            writer = csv.writer(f, delimiter=';')
            writer.writerow(['File', 'Test_Name', 'DropSpec', 'Accuracy', 'Total_Obs', 'Matches'])
            
            for file_name in EFFICIENCY_FILES:
                print(f"  Esecuzione test standard su {file_name}...")
                
                for test_name, drop_spec_list in EFFECTIVENESS_TESTS.items():
                    
                    which_atom = drop_spec_list[0]
                    keys_list = drop_spec_list[1]
                    keys_str = str(keys_list).replace("'", "") 
                    drop_spec_str = f"[{which_atom}, {keys_str}]"
                    
                    query = f"run_effectiveness('{file_name}', {drop_spec_str}, Accuracy, Details)."
                    
                    try:
                        result = list(prolog.query(query))
                        
                        if not result:
                            print(f"    ERRORE: Test '{test_name}' non ha restituito risultati.")
                            continue
                            
                        res = result[0]
                        accuracy = res['Accuracy']
                        total_obs = res['Details'][1]
                        matches = res['Details'][3]
                        
                        accuracy_str = f"{accuracy:.4f}".replace('.', ',')
                        
                        writer.writerow([file_name, test_name, drop_spec_str, accuracy_str, total_obs, matches])
                        
                    except PrologError as e:
                        print(f"    ERRORE (Prolog) su test '{test_name}': {e}")
                        return

            print(f"\n  Report Effectiveness Standard salvato in {EFFECTIVENESS_REPORT_FILE}")

    except Exception as e:
        print(f"ERRORE (Python) durante l'effectiveness: {e}")

def run_additional_effectiveness_tests(prolog):
    """Esegue i test di effectiveness aggiuntivi SOLO sul file 10k e salva su un file separato."""
    print(f"\n--- Avvio Test Effectiveness Aggiuntivi (solo {EFFECTIVENESS_10K_FILE}) ---")
    
    file_name = EFFECTIVENESS_10K_FILE
    report_file = ADDITIONAL_EFFECTIVENESS_REPORT_FILE 
    
    if file_name not in EFFICIENCY_FILES:
        print(f"ATTENZIONE: Il file {file_name} non è in EFFICIENCY_FILES. Test aggiuntivi saltati.")
        return

    try:
        with open(report_file, 'w', newline='') as f:
            writer = csv.writer(f, delimiter=';')
            
            writer.writerow(['File', 'Test_Name', 'DropSpec', 'Accuracy', 'Total_Obs', 'Matches'])
            print(f"  Salvataggio report in: {report_file}")

            print(f"  Esecuzione test aggiuntivi su {file_name}...")
            
            for test_name, drop_spec_list in ADDITIONAL_EFFECTIVENESS_TESTS.items():
                
                which_atom = drop_spec_list[0]
                keys_list = drop_spec_list[1]
                keys_str = str(keys_list).replace("'", "") 
                drop_spec_str = f"[{which_atom}, {keys_str}]"
                
                query = f"run_effectiveness('{file_name}', {drop_spec_str}, Accuracy, Details)."
                
                try:
                    print(f"    Running test: {test_name}...")
                    result = list(prolog.query(query))
                    
                    if not result:
                        print(f"    ERRORE: Test '{test_name}' non ha restituito risultati.")
                        continue
                        
                    res = result[0]
                    accuracy = res['Accuracy']
                    total_obs = res['Details'][1]
                    matches = res['Details'][3]
                    
                    accuracy_str = f"{accuracy:.4f}".replace('.', ',')
                    
                    writer.writerow([file_name, test_name, drop_spec_str, accuracy_str, total_obs, matches])
                    
                except PrologError as e:
                    print(f"    ERRORE (Prolog) su test '{test_name}': {e}")
                    continue

            print(f"\n  Report Effectiveness Aggiuntivo salvato in {report_file}")

    except Exception as e:
        print(f"ERRORE (Python) durante l'effectiveness aggiuntiva: {e}")

def main():
    # 1. Setup del parser
    parser = argparse.ArgumentParser(description="Script per eseguire test NEABT.")
    parser.add_argument('--efficiency_all', 
                        action='store_true', 
                        help='Esegue i test di Efficienza su tutti i file.')
    parser.add_argument('--effectiveness_all', 
                        action='store_true', 
                        help='Esegue i test di Effectiveness standard su tutti i file.')
    parser.add_argument('--drop_10000', 
                        action='store_true', 
                        help='Esegue i test di Effectiveness aggiuntivi solo sul file da 10k.')
    parser.add_argument('--all', 
                        action='store_true', 
                        help='Esegue TUTTI i test disponibili (efficiency + effectiveness + drop_10000).')

    # 2. Parsing degli argomenti
    args = parser.parse_args()

    # 3. Logica per determinare quali test eseguire
    if args.all:
        run_eff = True
        run_std_eff = True
        run_10k_eff = True
    else:
        run_eff = args.efficiency_all
        run_std_eff = args.effectiveness_all
        run_10k_eff = args.drop_10000

    # 4. Controlla se almeno un test è stato selezionato
    if not (run_eff or run_std_eff or run_10k_eff):
        print("Nessun test selezionato. Esecuzione terminata.")
        print("Usa -h o --help per vedere le opzioni disponibili.")
        sys.exit(0)

    # 5. Inizializza Prolog (solo se serve)
    print("Uno o più test selezionati. Avvio Prolog...")
    prolog = setup_prolog()
    if not prolog:
        sys.exit(1)

    # 6. Esegui i test selezionati
    if run_eff:
        run_efficiency_tests(prolog)
    
    if run_std_eff:
        run_effectiveness_tests(prolog)
    
    if run_10k_eff:
        run_additional_effectiveness_tests(prolog)

    print("\n--- Esperimenti completati ---")

if __name__ == "__main__":
    main()