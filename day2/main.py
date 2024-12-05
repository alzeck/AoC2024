from typing import Literal

file_name = 'input.txt'
file = open(file_name, 'r').readlines()

data = [[int(y) for y in x.strip().split(' ')] for x in file]


def is_report_safe(report: list[int]) -> bool:
    if report[0] - report[1] == 0:
        return False
    is_safe = True
    mode: Literal["inc"] | Literal["dec"] = 'inc' \
        if report[0] < report[1] else 'dec'

    for i in range(0, len(report) - 1):
        diff = report[i + 1] - report[i]
        if diff == 0 or (mode == 'inc' and diff < 0)\
                or (mode == 'dec' and diff > 0) \
                or abs(diff) > 3:
            is_safe = False
            continue

    return is_safe


safe_count = 0
safe_count_with_dampening = 0

for report in data:
    if is_report_safe(report):
        safe_count += 1
        safe_count_with_dampening += 1
    else:
        for i in range(0, len(report)):
            rep = report.copy()
            rep.pop(i)
            if is_report_safe(rep):
                safe_count_with_dampening += 1
                break

print(f"Part 1: {safe_count}")
print(f"Part 2: {safe_count_with_dampening}")
