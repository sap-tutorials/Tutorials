import pandas as pd

# Read Excel
df = pd.read_excel("payment-delay-data.xlsx")

# Convert problematic columns to string
df["Days Late"] = df["Days Late"].astype(str)
df["Risk Score"] = df["Risk Score"].astype(str)

# Save as Parquet
df.to_parquet("payment_transactions.parquet", index=False)

print("Parquet file generated successfully!")