import pandas as pd
import matplotlib.pyplot as plt
from io import StringIO

# Paste your data as a string (for demonstration)
data_str = """
author,file, word_count
Arjun Maneesh Agarwal,chapters-typ/ch01_functions.typ,1974.18
Owl-Boy,chapters-typ/ch01_functions.typ,227.79
RyanHota,chapters-typ/ch01_functions.typ,5163.24
TheArjunAgarwal,chapters-typ/ch01_functions.typ,151.86
RyanHota,chapters-typ/ch02_setup.typ,980.00
Arjun Maneesh Agarwal,chapters-typ/ch03_datatypes.typ,4149.12
Owl-Boy,chapters-typ/ch03_datatypes.typ,0
Ryan-Hota,chapters-typ/ch03_datatypes.typ,0
RyanHota,chapters-typ/ch03_datatypes.typ,7390.62
TheArjunAgarwal,chapters-typ/ch03_datatypes.typ,1166.94
Arjun Maneesh Agarwal,chapters-typ/ch04_tuples.typ,0
Owl-Boy,chapters-typ/ch04_tuples.typ,0
Ryan-Hota,chapters-typ/ch04_tuples.typ,2619.34
RyanHota,chapters-typ/ch04_tuples.typ,3308.64
TheArjunAgarwal,chapters-typ/ch04_tuples.typ,827.16
Arjun Maneesh Agarwal,chapters-typ/ch05_lists.typ,958.08
Owl-Boy,chapters-typ/ch05_lists.typ,0
RyanHota,chapters-typ/ch05_lists.typ,4610.76
TheArjunAgarwal,chapters-typ/ch05_lists.typ,359.28
Arjun Maneesh Agarwal,chapters-typ/ch06_polymorphism.typ,0
Owl-Boy,chapters-typ/ch06_polymorphism.typ,5157.91
RyanHota,chapters-typ/ch06_polymorphism.typ,65.29
TheArjunAgarwal,chapters-typ/ch06_polymorphism.typ,1109.93
Arjun Maneesh Agarwal,chapters-typ/ch07_advanced_lists.typ,14488.11
Owl-Boy,chapters-typ/ch07_advanced_lists.typ,0
RyanHota,chapters-typ/ch07_advanced_lists.typ,318.42
TheArjunAgarwal,chapters-typ/ch07_advanced_lists.typ,796.05
Arjun Maneesh Agarwal,chapters-typ/ch08_precomp-datatypes.typ,0
Owl-Boy,chapters-typ/ch08_precomp-datatypes.typ,4784.47
TheArjunAgarwal,chapters-typ/ch08_precomp-datatypes.typ,2213.71
Arjun Maneesh Agarwal,chapters-typ/ch09_computation.typ,34.00
Arjun Maneesh Agarwal,chapters-typ/ch10_complexity.typ,4424.25
TheArjunAgarwal,chapters-typ/ch10_complexity.typ,4164.00
Arjun Maneesh Agarwal,chapters-typ/ch11_postcomp-datatypes.typ,312.12
TheArjunAgarwal,chapters-typ/ch11_postcomp-datatypes.typ,7412.85
Arjun Maneesh Agarwal,chapters-typ/ch12_typeclasses.typ,0
Owl-Boy,chapters-typ/ch12_typeclasses.typ,25.44
RyanHota,chapters-typ/ch12_typeclasses.typ,2467.68
Arjun Maneesh Agarwal,extra-typ/appendix.typ,6.30
RyanHota,extra-typ/appendix.typ,10.65
RyanHota,extra-typ/preface.typ,1815.00
"""


# Load data
df = pd.read_csv(StringIO(data_str), skipinitialspace=True)
df['word_count'] = df['word_count'].astype(int)

# Normalize authors
author_map = {
    'Ryan-Hota': 'Ryan Hota',
    'RyanHota': 'Ryan Hota',
    'TheArjunAgarwal': 'Arjun Agarwal',
    'Arjun Maneesh Agarwal': 'Arjun Agarwal',
    'Owl-Boy' : 'Shubh Sharma'
}
df['author_normalized'] = df['author'].map(lambda x: author_map.get(x, x))

# Consistent chapter/file order
file_order = sorted(df['file'].unique())
df['file'] = pd.Categorical(df['file'], categories=file_order, ordered=True)

# Pivot table
pivot_df = df.pivot_table(
    index='author_normalized',
    columns='file',
    values='word_count',
    aggfunc='sum',
    fill_value=0
)
pivot_df = pivot_df[file_order]

# Plot stacked bar chart
fig, ax = plt.subplots(figsize=(12, 7))
colors = plt.cm.tab20.colors
bars = pivot_df.plot(kind='bar', stacked=True, ax=ax, color=colors[:len(pivot_df.columns)])

# Add labels
for i, author in enumerate(pivot_df.index):
    bottom = 0
    total = pivot_df.loc[author].sum()
    for j, file in enumerate(pivot_df.columns):
        count = pivot_df.loc[author, file]
        if count == 0:
            continue
        height_ratio = count / total
        # Threshold for text inside segment
        if height_ratio > 0.05:
            # Place text inside
            ax.text(i, bottom + count / 2, str(count), ha='center', va='center', fontsize=8)
        bottom += count
    # Total on top
    ax.text(i, bottom + total*0.01, f"Total: {total}", ha='center', va='bottom', fontsize=10, fontweight='bold')

# Guidelines for readability
from datetime import datetime

today = datetime.today().strftime('%Y-%m-%d')
ax.set_title(f"Words per Author per Chapter/File ({today})", fontsize=16, fontweight='bold')
ax.set_ylabel("Word Count", fontsize=12)
ax.set_xlabel("Author", fontsize=12)
ax.set_xticks(range(len(pivot_df.index)))
ax.set_xticklabels(pivot_df.index, rotation=45, ha='right')
ax.legend(
    title='File',
    bbox_to_anchor=(1.02, 1),
    loc='upper left',
    fontsize=8
)
ax.grid(axis='y', linestyle='--', alpha=0.7)

plt.tight_layout(rect=[0, 0, 0.8, 1])
plt.show()