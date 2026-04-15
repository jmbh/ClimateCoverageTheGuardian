# jonashaslbeck@protonmail.com; April 2026

import requests
import csv
from datetime import date, timedelta
import os

API_KEY = os.getenv("GUARDIAN_API_KEY")
BASE = "https://content.guardianapis.com/search"

import time

"""Script to retrieve daily or monthly article counts
for The Guardian. Filters can be applied to
exclude sections and restrict to tag 'type/article'."""

exclude_sections = [
    "housing-network",
    "culture-professionals-network",
    "public-leaders-network",
    "culture-network",
    "teacher-network",
    "media-network",
    "social-enterprise-network",
    "voluntary-sector-network",
    "enterprise-network",
    "social-care-network",
    "healthcare-network",
    "small-business-network",
    "global-development-professionals-network",
    "higher-education-network",
    "local-government-network",
    "guardian-masterclasses",
    "recruiters"
]

def print_sample_query():
    sections = [f"-{section}" for section in exclude_sections]
    params = {
        "from-date": "2010-01-01",
        "to-date":   "2010-01-31",
        "page-size": 1,
        "tag": "type/article",
        "section": ",".join(sections),
    }
    """Just print what `requests.get(BASE, params=params)` would look like."""
    req = requests.Request('GET', BASE, params=params).prepare()
    print(req.url)

def get_counts(start, end, use_monthly):
    delta = timedelta(days=1)
    sections = [f"-{section}" for section in exclude_sections]
    with open("Files/guardian_daily_counts.csv", "a", newline="") as f:
        writer = csv.writer(f)
        writer.writerow(["date","article_count"])
        d = start
        params = {
            "from-date": d.isoformat(),
            "to-date":   d.isoformat(),
            "page-size": 1,  # 1 is enough to get total
            "api-key": API_KEY,
            "tag": "type/article",
            "section": ",".join(sections),
        }
        while d <= end:
            if use_monthly:
                # Calculate the end of the month
                next_month = d.replace(day=28) + timedelta(days=4)  # This will always go to the next month
                end_of_month = next_month - timedelta(days=next_month.day)
                params["from-date"] = d.isoformat()
                params["to-date"] = end_of_month.isoformat()
            else:
                params["from-date"] = d.isoformat()
                params["to-date"] = d.isoformat()
            resp = requests.get(BASE, params=params)
            data = resp.json()
            if "response" not in data:
                print(f"Error for date {d.isoformat()}: {data}")
                # d += delta
                # continue
                print(data)
                break
            count = data["response"]["total"]
            writer.writerow([d.isoformat(), count])
            if use_monthly:
                # Increment by one month
                if d.month == 12:
                    d = d.replace(year=d.year + 1, month=1)
                else:
                    d = d.replace(month=d.month + 1)
            else:
                d += delta
            time.sleep(1)

if __name__ == "__main__":
    start = date(2010,1,1)
    end   = date(2025,12,31)
    use_monthly = False  # Set to False to go by day
    get_counts(start, end, use_monthly)
    # print_sample_query()

    # Manual sanity check
    # params = {
    #     "from-date": date(2010,1,1).isoformat(),
    #     "to-date":   date(2010,1,31).isoformat(),
    #     "page-size": 1,  # 1 is enough to get total
    #     "api-key": API_KEY
    # }
    # resp = requests.get(BASE, params=params)
    # data = resp.json()
    # print(data)
