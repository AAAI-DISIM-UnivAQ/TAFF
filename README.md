# NEABT Prolog Experimentation Framework

This project contains a set of Python scripts designed to automate the **efficiency** and **effectiveness** testing of a **NEABT (Neuro-Symbolic)** system implemented in **Prolog**.

The framework uses Python's **pyswip** library to interface with an **SWI-Prolog** backend.  
It runs two main types of experiments:

- **Efficiency:** Measures Prolog's execution and data-loading time as the number of input observations increases.  
- **Effectiveness:** Measures the system's accuracy when parts of the input knowledge (Prolog facts) are intentionally omitted, comparing the result against a "ground truth" (the run with all facts present).

> **Associated Pre-print:**
>
> **Neural Empathy-Aware Behavior Trees meets Knowledge Graphs for Affective Human-AI Teaming**
>
> *Glenda Amaral, University of Twente, Enschede, The Netherlands*
> *Stefania Costantini, University of L'Aquila, Italy*
> *Giovanni De Gasperis, University of L'Aquila, Italy*
> *Lorenzo De Lauretis, University of L'Aquila, Italy*
> *Pierangelo Dell'Acqua, Linköping University, Linköping, Sweden*
> *Giancarlo Guizzardi, University of Twente, Enschede, The Netherlands*
> *Francesco Gullo, University of L'Aquila, Italy*
> *Andrea Rafanelli, University of Pisa, Italy*

---

## 1. Prerequisites

Before running the experiments, you must have the following installed:

- **SWI-Prolog**: The Python script requires SWI-Prolog to be installed on your system and accessible from your PATH.
- **Python 3**: The scripts are written in Python 3.
- **pyswip**: The Python library used to bridge Python and Prolog.

Install it via pip:

```bash
pip install pyswip
```

---

## 2. Setup

### Step 1: Add Core Prolog Files

This repository does **not** include the core Prolog logic.  
Before running any tests, you must add the following two files (which are referenced by `execute_NEATB_tests.py`) to the root of the project:

- `neabt_core_lists.pl`
- `neabt_io_lists.pl`

---

### Step 2: Generate Input Data

The testing script (`execute_NEATB_tests.py`) depends on several input files (`NEABT_input_*.pl`) that contain the simulated observations.

You can generate these files using `generate_NEABT_input.py`.  
Run the following commands from your terminal:

```bash
python generate_NEABT_input.py 100
python generate_NEABT_input.py 500
python generate_NEABT_input.py 1000
python generate_NEABT_input.py 2000
python generate_NEABT_input.py 5000
python generate_NEABT_input.py 10000
```

This will create the six `NEABT_input_*.pl` files required by the experiment configuration.

---

## 3. Running the Experiments

The `execute_NEATB_tests.py` script is used to run all experiments.  
You can run all tests at once or select specific test suites using command-line arguments.

### To Run All Tests

This is the simplest way to run everything.  
It will execute the efficiency tests, the standard effectiveness tests, and the additional 10k effectiveness tests.

```bash
python execute_NEATB_tests.py --all
```

---

### To Run Specific Tests

**Efficiency Tests Only:**

```bash
python execute_NEATB_tests.py --efficiency_all
```

This runs the `run_efficiency` predicate (100 times, as configured in the script) on all input files and saves the aggregated results to `efficiency_report.csv`.

---

**Standard Effectiveness Tests Only:**

```bash
python execute_NEATB_tests.py --effectiveness_all
```

This runs the standard set of “fact-dropping” tests (defined in `EFFECTIVENESS_TESTS`) on all input files.  
Results are saved to `effectiveness_report.csv`.

---

**Additional Effectiveness Tests (on 10k file) Only:**

```bash
python execute_NEATB_tests.py --drop_10000
```

This runs an additional set of “fact-dropping” tests (defined in `ADDITIONAL_EFFECTIVENESS_TESTS`) only on the largest `NEABT_input_10000.pl` file.  
Results are saved to `effectiveness_report_10k_additional.csv`.

---

### To See All Options

To see the built-in help menu for the script, run:

```bash
python execute_NEATB_tests.py --help
```

---

## 4. Input Data Structure

Each `NEABT_input_*.pl` file contains a series of *observations*.  
Each observation is composed of **13 Prolog facts (predicates)**, one per line.

These 13 facts are divided into three categories:

---

### **Environment (6 facts)**

| Predicate | Description |
|------------|--------------|
| `traffic_status/1` | Traffic level (`green`, `orange`, `red`, `black`). |
| `driving_hours/1` | Current driving session duration (0–8 hours). |
| `speed/1` | Driving speed (30–150). |
| `stops/1` | Number of stops in the current session (0–5). |
| `avg_stop_duration/1` | Average duration of stops. |
| `location/1` | Current location ID (1–50). |

---

### **KG User Profile (4 facts)**

| Predicate | Description |
|------------|--------------|
| `crashes_last_year/1` | Number of driver crashes in the last year (0–3). |
| `address/1` | Driver’s home address ID (1–50). |
| `driving_safety/1` | Driver safety style (1–5, higher is safer). |
| `driving_experience/1` | Driver experience level (1–5, higher is more experienced). |

---

### **KG Domain Knowledge (3 facts)**

| Predicate | Description |
|------------|--------------|
| `road_conditions/1` | Road surface conditions (`bad`, `fair`, `good`). |
| `weather_conditions/1` | Weather conditions (`sunny`, `cloudy`, `rainy`, `snowy`). |
| `road_dangerousness/1` | Road dangerousness level (1–5, higher is more dangerous). |

---

## License

This project is open-source and may be used, modified, and redistributed under the terms of the Apache-2.0 license
