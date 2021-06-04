import pandas as pd


def main():
    df = pd.read_csv("app/data/covid_confirmed_usafacts.csv")

    # Only NC Counties
    df = df[df.State == "NC"]
    df = df[df.countyFIPS != 0]
    # Fix the county name
    df["County Name"] = df["County Name"].apply(lambda x: x.replace(" County", ""))
    # Drop Useless Columns
    df = df.drop(["State", "StateFIPS"], axis=1).copy()

    # Melt
    df_final = pd.melt(
        df, id_vars=["County Name", "countyFIPS"], value_vars=df.columns[2:]
    )
    df_final.columns = ["County", "County FIPS", "Date", "Total Confirmed Cases"]
    df_final["County"] = df_final["County"].str.strip()

    # Fix Macon
    df_final.loc[
        (df_final["County"] == "Macon") & (df_final.Date == "2020-12-28"),
        "Total Confirmed Cases",
    ] = 1497

    df_final["Cumulative Cases"] = 0
    gb = df_final.groupby(["County"])
    keys = gb.groups.keys()
    for key in keys:
        print(f"Working with County: {key}")
        group = gb.get_group(key)
        values = [0]
        for i, value in enumerate(group["Total Confirmed Cases"][1:]):
            if value > values[i]:
                values.append(value)
            else:
                values.append(values[-1])
        df_final.loc[group.index, "Cumulative Cases"] = values

    df_final["New Cases"] = gb["Cumulative Cases"].diff().fillna(0)
    df_final.to_csv("app/data/covid19_cases.csv", index=False)


if __name__ == "__main__":
    main()


count = 0
for i in range(100, 1000):
    ic = i ** 3
    for j in range(100, i):
        for k in range(100, j):
            count += 1
            continue


for i in range(100, 1000):
    for j in range(100, i):
        d = i + j
        left_side = (i ** 2) - i * j + (j ** 2)


for i in range(1, 21):
    print(i ** 3)


x = 2000
y = -1222
z = -1111

x ** 3 + y ** 3 + z ** 3

left_side = x ** 2 - x * y + y ** 2
left_side2 = left_side * (x + y)
left_side3 = left_side2 - 4803876321

for i in range(1001, 10000, 10):
    if i ** 3 == left_side3:
        print(i)


find = 12
count = 0
for x in range(1, 1000):
    for y in range(1, x + 1):
        count += 1
        d = x + y
        left_side = x ** 2 - (x * y) + y ** 2
        left_side2 = left_side * d
        for test in [-1, 1]:
            left_side3 = left_side2 + (test) * find
            t = abs(left_side3) ** (1 / 3)
            if round(t) ** 3 == left_side3:
                print(f"The Solution for {find} is:")
                z = -int((left_side3) ** (1 / 3))
                print(f"x: {x}, y: {y}, z:{z}")
                raise
