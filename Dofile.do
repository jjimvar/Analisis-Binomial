cls
cd "C:\Users\jjime\OneDrive\Escritorio\3ro GANE\Mercados\Stata\2"
use  "Datos.dta", clear

*INTRODUCCIÓN
keep gasto_azucar gastot_pc renta_pc gasto_cafenormal gasto_cafecapsulas gasto_lecheentera gasto_lechedescremada estudios edadsp densidad_alta densidad_media

*PARTE I: ANÁLISIS ESTADÍSTICO
**Gasto Azucar
replace gasto_azucar=0 if gasto_azucar==.
gen gasazu=0
replace gasazu=1 if gasto_azucar>0
tab gasazu
**Renta
gen gast=gastot_pc/12
gen rent=renta_pc/12
sum gast rent, detail
egen r1=pctile(rent), p(1)
keep if rent>r1
replace rent=rent/100
sum rent, detail
**Bien complementario
replace gasto_cafenormal=0 if gasto_cafenormal==.
replace gasto_cafecapsulas=0 if gasto_cafecapsulas==.
replace gasto_lecheentera=0 if gasto_lecheentera==.
replace gasto_lechedescremada=0 if gasto_lechedescremada==.
gen caflec=(gasto_cafenormal+gasto_cafecapsulas+gasto_lecheentera+gasto_lechedescremada)/100
sum caflec, detail
**Educación
gen educ=.
replace educ=0 if estudios<=3
replace educ=1 if estudios>=4
label define ed 0 "Educación Obligatoria" 1 "Educación Posobligatoria"
label values educ ed
**Edad
gen edad=edadsp/10
**Resumen
global x rent caflec educ edad densidad_alta densidad_media
sum gasazu $x if gasazu==1
sum gasazu $x if gasazu==0 

*PARTE II: MODELIZACIÓN
**Analizamos modelo inicial
logit gasazu $x, robust
**Analizamos efectos marginales
margins, dydx (*)
***Sobre la renta
sum rent, detail
scalar a5=r(p5)
scalar a25=r(p25)
scalar a50=r(p50)
scalar a75=r(p75)
scalar a95=r(p95)
scalar list a5 a25 a50 a75 a95
margins, dydx(rent) at(rent=(3.16  5.92 8.53 12.49 21.78))
marginsplot, saving(EMG.gph, replace)
**Evaluación bondad del ajuste
***Predicciones
qui logit gasazu $x, robust
predict pgasazu, pr
sum pgasazu gasazu
***Matriz de confusión
qui logit gasazu $x, robust
estat classification
lsens gasazu
***Curva de ROC
estat classification, cutoff(0.47)
qui logit gasazu $x, robust
lroc

*PARTE III: SIMULACIÓN
**Suponemos una sociedad mas obsesionada con el rendimiento profesional:
replace caflec=caflec+1
predict pgasazu2, pr
sum pgasazu2 pgasazu gasazu