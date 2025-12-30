# -*- coding: utf-8 -*-

import csv
from random import randint


def generator(start: int = 1, end: int = 5) -> None:
    
    data = [
        # ["ID",   "Salary"],
        # ["101", "6200.50"]
    ]
    
    size = len(str(end))
    
    for i in range(start, end):
        ID = f"{i:0{size}d}"
        Salary = f"{randint(100000, 800000) / 100:.2f}"
        data.append([ID, Salary])

    # Export to CSV
    with open('payroll.csv', 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerows(data)


if __name__ == '__main__':
    generator(1, 10000)