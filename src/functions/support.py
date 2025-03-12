from datetime import datetime

def month_year_generator(start_month, start_year, end_month, end_year):
    """Generates ('Mon', Year) tuples from start to end month-year."""
    current = datetime(start_year, start_month, 1)
    end = datetime(end_year, end_month, 1)

    while current <= end:
        yield current, current.year  
        # Move to next month
        if current.month == 12:
            current = datetime(current.year + 1, 1, 1)
        else:
            current = datetime(current.year, current.month + 1, 1)
