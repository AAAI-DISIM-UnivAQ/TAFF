import csv
import statistics
import sys
import argparse
import os
from pyswip import Prolog
from pyswip.prolog import PrologError

# 1. .pl files to load
PROLOG_CORE_FILE = 'neabt_core_lists.pl'
PROLOG_IO_FILE = 'neabt_io_lists.pl'

# 2. Efficiency Configuration
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

# 3. Effectiveness Configuration
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

# 4. Filename for the additional test report
ADDITIONAL_EFFECTIVENESS_REPORT_FILE = 'effectiveness_report_10k_additional.csv'

# 5. Additional Effectiveness Configuration (10k only)
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
    """Initializes Prolog and loads the necessary scripts."""
    print("Initializing SWI-Prolog and loading scripts...")
    prolog = Prolog()
    
    try:
        prolog.consult(PROLOG_CORE_FILE)
        prolog.consult(PROLOG_IO_FILE)
        print(f"Loaded: {PROLOG_CORE_FILE}, {PROLOG_IO_FILE}\n")
        return prolog
    except PrologError as e:
        print(f"ERROR: Could not load Prolog files: {e}")
        print("Ensure the .pl files are in the same folder and SWI-Prolog is installed.")
        sys.exit(1)

def run_efficiency_tests(prolog):
    """Runs the efficiency tests 100 times and saves the results."""
    print(f"--- Starting Efficiency Test ({EFFICIENCY_RUNS} runs) ---")
    
    all_results = {file_name: {'load_ms': [], 'exec_ms': []} for file_name in EFFICIENCY_FILES}
    
    prolog_file_list = str(EFFICIENCY_FILES)

    try:
        with open(EFFICIENCY_REPORT_FILE, 'w', newline='') as f:
            writer = csv.writer(f, delimiter=';') 
            
            writer.writerow(['Run', 'File', 'n_obs', 'load_ms', 'exec_ms'])
            
            for i in range(EFFICIENCY_RUNS):
                print(f"  Efficiency Run {i + 1}/{EFFICIENCY_RUNS}...")
                
                query = f"run_efficiency({prolog_file_list}, Report)."
                result = list(prolog.query(query))
                
                if not result:
                    print(f"ERROR: Efficiency query returned no results (Run {i+1}).")
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

            print(f"\n  Raw data saved to {EFFICIENCY_REPORT_FILE}")

            writer.writerow([])
            writer.writerow(['--- Average Values ---'])
            writer.writerow(['File', 'n_obs', 'Avg_Load_ms', 'Avg_Exec_ms'])
            
            for file_name in EFFICIENCY_FILES:
                n_obs = file_name.split('_')[2].split('.')[0]
                avg_load = statistics.mean(all_results[file_name]['load_ms'])
                avg_exec = statistics.mean(all_results[file_name]['exec_ms'])
                
                avg_load_str = f"{avg_load:.2f}".replace('.', ',')
                avg_exec_str = f"{avg_exec:.2f}".replace('.', ',')
                
                writer.writerow([file_name, n_obs, avg_load_str, avg_exec_str])
                
            print(f"  Average values added to {EFFICIENCY_REPORT_FILE}")

    except PrologError as e:
        print(f"ERROR during efficiency run: {e}")
    except Exception as e:
        print(f"ERROR (Python): {e}")

def run_effectiveness_tests(prolog):
    """Runs the standard effectiveness tests on all files and saves the results."""
    print(f"\n--- Starting Effectiveness Test (Standard) ---")
    
    try:
        with open(EFFECTIVENESS_REPORT_FILE, 'w', newline='') as f:
            writer = csv.writer(f, delimiter=';')
            writer.writerow(['File', 'Test_Name', 'DropSpec', 'Accuracy', 'Total_Obs', 'Matches'])
            
            for file_name in EFFICIENCY_FILES:
                print(f"  Running standard tests on {file_name}...")
                
                for test_name, drop_spec_list in EFFECTIVENESS_TESTS.items():
                    
                    which_atom = drop_spec_list[0]
                    keys_list = drop_spec_list[1]
                    keys_str = str(keys_list).replace("'", "") 
                    drop_spec_str = f"[{which_atom}, {keys_str}]"
                    
                    query = f"run_effectiveness('{file_name}', {drop_spec_str}, Accuracy, Details)."
                    
                    try:
                        result = list(prolog.query(query))
                        
                        if not result:
                            print(f"    ERROR: Test '{test_name}' returned no results.")
                            continue
                            
                        res = result[0]
                        accuracy = res['Accuracy']
                        total_obs = res['Details'][1]
                        matches = res['Details'][3]
                        
                        accuracy_str = f"{accuracy:.4f}".replace('.', ',')
                        
                        writer.writerow([file_name, test_name, drop_spec_str, accuracy_str, total_obs, matches])
                        
                    except PrologError as e:
                        print(f"    ERROR (Prolog) on test '{test_name}': {e}")
                        return

            print(f"\n  Standard Effectiveness Report saved to {EFFECTIVENESS_REPORT_FILE}")

    except Exception as e:
        print(f"ERROR (Python) during effectiveness run: {e}")

def run_additional_effectiveness_tests(prolog):
    """Runs additional effectiveness tests ONLY on the 10k file and saves to a separate file."""
    print(f"\n--- Starting Additional Effectiveness Tests ({EFFECTIVENESS_10K_FILE} only) ---")
    
    file_name = EFFECTIVENESS_10K_FILE
    report_file = ADDITIONAL_EFFECTIVENESS_REPORT_FILE 
    
    if file_name not in EFFICIENCY_FILES:
        print(f"WARNING: File {file_name} is not in EFFICIENCY_FILES. Skipping additional tests.")
        return

    try:
        with open(report_file, 'w', newline='') as f:
            writer = csv.writer(f, delimiter=';')
            
            writer.writerow(['File', 'Test_Name', 'DropSpec', 'Accuracy', 'Total_Obs', 'Matches'])
            print(f"  Saving report to: {report_file}")

            print(f"  Running additional tests on {file_name}...")
            
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
                        print(f"    ERROR: Test '{test_name}' returned no results.")
                        continue
                        
                    res = result[0]
                    accuracy = res['Accuracy']
                    total_obs = res['Details'][1]
                    matches = res['Details'][3]
                    
                    accuracy_str = f"{accuracy:.4f}".replace('.', ',')
                    
                    writer.writerow([file_name, test_name, drop_spec_str, accuracy_str, total_obs, matches])
                    
                except PrologError as e:
                    print(f"    ERROR (Prolog) on test '{test_name}': {e}")
                    continue

            print(f"\n  Additional Effectiveness Report saved to {report_file}")

    except Exception as e:
        print(f"ERROR (Python) during additional effectiveness run: {e}")

def main():
    # 1. Setup the parser
    parser = argparse.ArgumentParser(description="Script to run NEABT tests.")
    parser.add_argument('--efficiency_all', 
                        action='store_true', 
                        help='Run Efficiency tests on all files.')
    parser.add_argument('--effectiveness_all', 
                        action='store_true', 
                        help='Run standard Effectiveness tests on all files.')
    parser.add_argument('--drop_10000', 
                        action='store_true', 
                        help='Run additional Effectiveness tests on the 10k file only.')
    parser.add_argument('--all', 
                        action='store_true', 
                        help='Run ALL available tests (efficiency + effectiveness + drop_10000).')

    # 2. Parse arguments
    args = parser.parse_args()

    # 3. Logic to determine which tests to run
    if args.all:
        run_eff = True
        run_std_eff = True
        run_10k_eff = True
    else:
        run_eff = args.efficiency_all
        run_std_eff = args.effectiveness_all
        run_10k_eff = args.drop_10000

    # 4. Check if at least one test was selected
    if not (run_eff or run_std_eff or run_10k_eff):
        print("No test selected. Exiting.")
        print("Use -h or --help to see available options.")
        sys.exit(0)

    # 5. Initialize Prolog (only if needed)
    print("One or more tests selected. Starting Prolog...")
    prolog = setup_prolog()
    if not prolog:
        sys.exit(1)

    # 6. Run the selected tests
    if run_eff:
        run_efficiency_tests(prolog)
    
    if run_std_eff:
        run_effectiveness_tests(prolog)
    
    if run_10k_eff:
        run_additional_effectiveness_tests(prolog)

    print("\n--- Experiments complete ---")

if __name__ == "__main__":
    main()