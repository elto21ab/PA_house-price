#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#   "pandas",
# ]
# ///
"""
Collapses weekly bond interest rate data (bondInt.csv) into quarterly
means, producing bondInt_quarterly.csv with a Quarter key in the format
YYYYKN (e.g. 2024K1) to match kbh_quarter_sqm_price.csv.

ISO week assignment:
  Each week is represented by its Thursday (ISO 8601 standard), which
  determines unambiguously which calendar quarter it belongs to.

Usage:
  uv run weekly_to_quarterly_bond.py
"""

import pandas as pd
from pathlib import Path

INPUT  = Path(__file__).parent / "bondInt.csv"
OUTPUT = Path(__file__).parent / "bondInt_quarterly.csv"

# ------------------------------------------------------------------
# Load
# ------------------------------------------------------------------
df = pd.read_csv(INPUT)
df.columns = ["year", "week", "rate"]

# ------------------------------------------------------------------
# Assign each ISO week to a quarter via its Thursday date
# ------------------------------------------------------------------
# '%G-W%V-%u' parses ISO year + ISO week + weekday (4 = Thursday)
df["date"] = pd.to_datetime(
    df["year"].astype(str) + "-W" + df["week"].astype(str).str.zfill(2) + "-4",
    format="%G-W%V-%u",
)
df["quarter"] = df["date"].dt.quarter
df["cal_year"] = df["date"].dt.year          # may differ from ISO year near Jan 1
df["Quarter"]  = df["cal_year"].astype(str) + "K" + df["quarter"].astype(str)

# ------------------------------------------------------------------
# Aggregate: mean rate per quarter, sorted chronologically
# ------------------------------------------------------------------
quarterly = (
    df.groupby("Quarter", sort=False)
    .agg(mean_rate=("rate", "mean"), _date=("date", "min"))
    .reset_index()
    .sort_values("_date")
    .drop(columns="_date")
    .round({"mean_rate": 4})
)

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------
quarterly.to_csv(OUTPUT, index=False)
print(f"Written {len(quarterly)} quarterly observations to {OUTPUT.name}")
print(quarterly.head(10).to_string(index=False))
