def calibration_value(input: str) -> int:
    for c in input:
        if c.isdigit():
            first = c
            break

    for c in input[::-1]:
        if c.isdigit():
            last = c
            break

    return int(first + last)


def part_one():

    with open("../inputs/Day01.input") as f:
        input_data = f.readlines()

    result = sum(map(calibration_value, input_data))
    print(result)


if __name__ == "__main__":
    part_one()
