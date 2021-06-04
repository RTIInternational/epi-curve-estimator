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
