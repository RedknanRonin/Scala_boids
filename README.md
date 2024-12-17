# Matriculation Data Visualization with the Boids algorithm
- a continuation of the project

## Description
This project is designed to analyze matriculation exam data from various schools. The data is read from CSV files, processed using Python and Pandas, and stored in a PostgreSQL database. The project includes functionality to calculate the frequency of each grade for each school and visualize the results.

![image](https://github.com/user-attachments/assets/89377bff-39e1-4f8a-8ff8-550f895a920d)


## Features
- Read matriculation exam data from CSV files.
- Process and clean the data using Pandas.
- Store the processed data in a PostgreSQL database.
- Calculate the frequency of each grade for each school.
- Visualize the results using Matplotlib.

## Requirements
- Python 3.x
- Pandas
- Psycopg2
- Matplotlib
- PostgreSQL

## Installation
1. Clone the repository:
   ```sh
   git clone https://github.com/RedknanRonin/matriculation-data-analysis.git
   cd matriculation-data-analysis
   ```

2. Install the required Python packages:
   ```sh
   pip install pandas psycopg2 matplotlib
   ```

3. Set up the PostgreSQL database:
   - Create a database named `matriculationData`.
   - Update the database connection details in the `dataHandling.py` file if necessary.

## Usage
1. Run the main script to process the data and store it in the database:
   ```sh
   python src/main/scala/dataHandling.py
   ```

2. The script will read the CSV files, process the data, and insert it into the PostgreSQL database.

3. To visualize the grade frequencies, you can modify the script to generate plots using Matplotlib.

## File Structure
- `data/`: Directory containing the CSV data files.
- `src/main/scala/dataHandling.py`: Main script for data processing and database interaction.
- `README.md`: Project documentation.


## Acknowledgements
- This project uses the Pandas library for data processing.
- Matplotlib is used for data visualization.
- PostgreSQL is used for data storage.
