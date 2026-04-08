import csv
import random
from datetime import datetime
import os
from jinja2 import Template

CSV_FILE = 'data.csv'
OUTPUT = 'index.html'

# Fun list of fictional bank addresses
ADDRESSES = [
    "221B Baker Street, London",
    "177A Bleecker Street, Lower Manhattan, New York City",
    "742 Evergreen Terrace, Springfield",
    "The North Pole",
    "12 Grimmauld Place, London",
    "Bag End, Hobbiton, The Shire",
    "The North Pole",
    "2311 North Los Robles Avenue, Pasadena"
]

TEMPLATE = """
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Markov Bank</title>
    <style>
        :root {
            --cream: #F8F4E7;
            --brown: #552800;
            --link-blue: #0000A0;
            --visited-purple: #A000A0;
            --active-green: #00A000;
        }
        
        body {
            font-family: 'Courier New', monospace;
            background-color: var(--cream);
            color: var(--brown);
            max-width: 900px;
            margin: 0 auto;
            padding: 20px;
            line-height: 1.5;
        }
        
        a {
            color: var(--link-blue);
            text-decoration: none;
        }
        
        }
        
        a:active, a:hover {
            color: var(--active-green);
        }
        
        .bank-header {
            display: flex;
            align-items: center;
            gap: 20px;
            margin-bottom: 30px;
            flex-wrap: wrap;
        }
        
        .bank-logo {
            height: 80px;
            width: auto;
        }
        
        .bank-name {
            font-size: 1.5rem;
            font-weight: bold;
        }
        
        .bank-address {
            font-size: 0.8rem;
            opacity: 0.8;
        }
        
        .balances-table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }
        
        .balances-table td {
            padding: 5px 10px;
            border-bottom: 1px dotted #ccc;
        }
        
        .balances-table td:last-child {
            text-align: right;
            font-family: monospace;
        }
        
        .disclaimer {
            font-size: 0.8rem;
            margin-top: 30px;
            opacity: 0.7;
        }
        
        .mono {
            font-family: monospace;
        }
        
        @media (max-width: 600px) {
            .bank-header {
                flex-direction: column;
                align-items: flex-start;
                gap: 10px;
            }
        }
    </style>
</head>
<body>
    <header class="bank-header">
        <img src="markov-bank.webp" alt="Bank Logo" class="bank-logo">
        <div>
            <div class="bank-name">Markov Bank</div>
            <div class="bank-address">{{ bank_address }}</div>
        </div>
    </header>

    <main>
        <p>Account balances as of <time datetime="{{ update_date }}">{{ display_date }}</time>:</p>
        
        <div class="balances-container">
            <table class="balances-table">
                {% for contributor in contributors %}
                <tr>
                    <td>{{ contributor.name }}</td>
                    <td><span class="mono">{{ "%.2fM"|format(contributor.balance) }}</span></td>
                </tr>
                {% endfor %}
            </table>
        </div>
        
        <p>(Par value is <span class="mono">1M</span> = INR 15 = Cost of 1 plate Maggi in Chennai Mathematical Institute Mess)</p>
        
        <p>(These account balances reflect errors found in this <a href="https://github.com/TheArjunAgarwal/haskell2025" target="_blank" rel="noopener noreferrer">Haskell textbook</a> by Ryan Hota, Shubh Sharma and Arjun Maneesh Agarwal.)</p>
    </main>

    <footer class="disclaimer">
        <p>Site and logo by <a href="https://thearjunagarwal.github.io/" target="_blank" rel="noopener noreferrer">Arjun Maneesh Agarwal</a>, based on <a href="https://www-cs-faculty.stanford.edu/~knuth/boss.html" target="_blank">The Bank of San Serriffe</a> </p>
        <p>To request withdrawals, please contact the authors at Chennai Mathematical Institute. (Withdrawn funds do not decrease the grand totals shown above; but you aren't supposed to withdraw a reward twice.)</p>
    </footer>
</body>
</html>
"""

def read_contributors(csv_file):
    """Read and sort contributor data by balance (highest first)"""
    contributors = []
    with open(csv_file, mode='r') as file:
        reader = csv.DictReader(file, skipinitialspace=True)
        for row in reader:
            contributors.append({
                'name': row['name'].strip(),
                'balance': float(row['balance'].strip())  # Convert to float
            })
    contributors.sort(key=lambda x: x['balance'], reverse=True)
    return contributors

def generate_html(contributors, bank_address):
    """Generate HTML using template"""
    today = datetime.now()
    template = Template(TEMPLATE)
    
    return template.render(
        contributors=contributors,
        bank_address=bank_address,
        update_date=today.strftime("%Y-%m-%d"),
        display_date=today.strftime("%d %B %Y")
    )

def save_html(html_content, output_file):
    """Save HTML to file"""
    with open(output_file, 'w') as file:
        file.write(html_content)
    print(f"HTML saved to {output_file}")

def main():
    # Read data from CSV
    contributors = read_contributors(CSV_FILE)
    
    # Select random bank address
    bank_address = random.choice(ADDRESSES)
    
    # Generate HTML
    html_content = generate_html(contributors, bank_address)
    save_html(html_content, OUTPUT)

if __name__ == "__main__":
    main()