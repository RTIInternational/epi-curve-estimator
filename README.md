# EPI Curve SEIR Modeling

### Intrusctions for Obtaining Data

- Daily case data can be found [here](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/state/north-carolina) or by clicking [this](https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv?_ga=2.244758405.1872175487.1616527141-1160255462.1614173392)
- Place the data here: `app/data/covid_confirmed_usafacts.csv`
- From the root directory, run this script: 
	```
	python src/prep_data.py 
	```
	

### Run App Locally


```
R -e "shiny::runApp('app/')"
```


### Run Shiny App Using Docker

Build image:

```
docker build -t nsf-shiny .
```

Run on Server:

```
docker run -d --rm -p 3840:3838 nsf-shiny
```

## Disclaimer

This material is based upon work supported by the National Science Foundation under Grant Number 2027802

Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.