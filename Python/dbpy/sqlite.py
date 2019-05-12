#!/usr/bin/env python3
import sqlite3
from employee import Employee

with sqlite3.connect(":memory:") as conn:
    c = conn.cursor()
    c.execute("""CREATE TABLE employees(
        first TEXT,
        last TEXT,
        pay INTEGER
    ) """)

    john = Employee("John", "Doe", 6000)
    jay = Employee("Jay", "Doe", 800)

    c.execute(
        f"INSERT INTO employees VALUES (?, ?, ?)",
        (john.first, john.last, john.pay)
    )

    c.execute(
        f"INSERT INTO employees VALUES (:first, :last, :pay)",
        jay.__dict__
    )

    c.execute("SELECT * FROM employees WHERE last=?", ('Shafer',))
    print(c.fetchall())

    c.execute("SELECT * FROM employees WHERE last=:last", {'last': 'Doe'})
    print(c.fetchall())

    conn.commit()
