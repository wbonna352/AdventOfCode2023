import re
from typing import Optional


def calibration_value(line: str) -> int:
    for c in line:
        if c.isdigit():
            first = c
            break

    for c in line[::-1]:
        if c.isdigit():
            last = c
            break

    return int(first + last)


def extract_first(pattern: str, string: str) -> Optional[str]:
    if match := re.search(pattern, string):
        return match.group(0)


def calibration_value_with_words(line: str) -> int:

    digits = {str(i): i for i in range(1, 10)}
    words = {
        "one": 1,
        "two": 2,
        "three": 3,
        "four": 4,
        "five": 5,
        "six": 6,
        "seven": 7,
        "eight": 8,
        "nine": 9
    }
    mapper = {**digits, **words}
    pattern = "|".join(mapper)

    matches = (extract_first(pattern, line[i:]) for i in range(len(line)))
    first = next(matches)
    last = first
    for m in matches:
        if m:
            last = m

    return int(f"{mapper[first]}{mapper[last]}")


def part_one():

    with open("../inputs/Day01.input") as f:
        input_data = f.readlines()

    result = sum(map(calibration_value, input_data))
    print(result)


def part_two():

    with open("../inputs/Day01.input") as f:
        input_data = f.readlines()

    result = sum(map(calibration_value_with_words, input_data))
    print(result)


if __name__ == "__main__":
    part_one()
    part_two()
