// Reporte empleo
set maxvar 20000 // Aumento el número de variables permitidas
cd"C:\Users\borqu\Documents\reentry-data-pipeline\Stata" // Directorio
use base_general_wide.dta, clear
drop if reg_folio==10011 | reg_folio==40280 // Elimino los dos casos que no son de la muestra
set more off, permanently

// Presente en todas las olas

gen todas_olas=1 if reg_fecha_w2!="" & reg_fecha_w3!="" & reg_fecha_w4!="" 
replace todas_olas=0 if missing(todas_olas)
label define muestra 0"No" 1"Si"
label value todas_olas muestra
label variable todas_olas "Esta en 2M/6M/12M"
tab todas_olas, m // 169 casos

////// 2.A ////////////////////////////////////////////////

// BUSQUEDA DE TRABAJO

// Ha buscado trabajo desde la última vez que se vieron
codebook eaf_21_w1 eaf_21_w2 eaf_21_w3 eaf_21_w4
codebook eaf_21_w2
tab1 eaf_21_w3, m
tab1 eaf_21_w4, m

* Buscó trabajo en al menos una ola
gen busco_trab=1 if eaf_21_w1==1 | eaf_21_w2==1 | eaf_21_w3==1 | eaf_21_w4==1
replace busco_trab=0 if missing(busco_trab)
tab busco_trab
replace busco_trab=. if eaf_21_w1==. & eaf_21_w2==. & eaf_21_w3==. & eaf_21_w4==.
replace busco_trab=-7 if eaf_21_w1==-7 & eaf_21_w2==-7 & eaf_21_w3==-7 & eaf_21_w4==-7
replace busco_trab=-9 if eaf_21_w1==-9 & eaf_21_w2==-9 & eaf_21_w3==-9 & eaf_21_w4==-9
tab busco_trab, m

tab1 eaf_21_w1 if todas_olas==1, m 
tab1 eaf_21_w2 if todas_olas==1, m 
tab1 eaf_21_w3 if todas_olas==1, m 
tab1 eaf_21_w4 if todas_olas==1, m 
tab busco_trab if todas_olas==1, m

// 2.B ///////////////////////////////////////////

/// DIFICULTAD PARA ENCONTRAR/MANTENER TRABAJO

* LB // 1=Muy facil; 2=Facil; 3=Dificil; 4=Muy dificil; -7=NA; -8=NS; -9=NR
tab1 spg_7_8_w0, m
tab1 spg_7_8_w0 if busco_trab==1 & todas_olas==1, m

* 1 sem // 1=Para nada dificil; 2=Un poco dificil; 3=Dificil; 4=Muy dificil;
tab1 spg_10_7_w1

* 2/6/12 meses Buscar trabajo
tab1 spg_10_8_w2 spg_10_8_w3 spg_10_8_w4
codebook spg_10_8_w2 spg_10_8_w3 spg_10_8_w4
gen dif_busc_emp=1 if spg_10_8_w2==3 | spg_10_8_w2==4 | spg_10_8_w3==3 | spg_10_8_w3==4 | spg_10_8_w4==3 | spg_10_8_w4==4
replace dif_busc_emp=0 if missing(dif_busc_emp)
replace dif_busc_emp=. if spg_10_8_w2==. & spg_10_8_w3==. & spg_10_8_w4==.
tab dif_busc_emp if busco_trab==1 & todas_olas==1, m
tab dif_busc_emp if busco_trab==1, m

tab1 spg_10_8_w2 spg_10_8_w3 spg_10_8_w4 if busco_trab==1 & todas_olas==1


// 2.C //////////////////////////////////////////////////

// RECHAZO POR ANTECEDENTES
tab1 eaf_23_w2 eaf_23_w3 eaf_23_w4, m

gen rec_ant_2=eaf_23_w2
gen rec_ant_6=eaf_23_w3
gen rec_ant_12=eaf_23_w4

recode rec_ant_2 rec_ant_6 rec_ant_12 (-7 -8 -9=-1)
tab1 rec_ant_2 rec_ant_6 rec_ant_12

gen rec_ant=1 if eaf_23_w2==1 | eaf_23_w3==1 | eaf_23_w4==1
replace rec_ant=0 if missing(rec_ant)
replace rec_ant=. if rec_ant_2==-1 & rec_ant_6==-1 & rec_ant_12==-1
tab rec_ant, m

tab1 eaf_23_w2 eaf_23_w3 eaf_23_w4 if todas_olas==1, m
tab rec_ant if todas_olas==1 & busco_trab==1, m

// 2.D ///////////////////////////

// MEDIOS PARA BUSCAR TRABAJO LB

tab1 eaf_15_1_w0 eaf_15_2_w0 eaf_15_3_w0 eaf_15_4_w0 eaf_15_5_w0 eaf_15_6_w0 eaf_15_7_w0 eaf_15_8_w0 eaf_15_9_w0 eaf_15_10_w0 eaf_15_11_w0 eaf_15_12_w0 if todas_olas==1
codebook eaf_22_1_w1

// MEDIOS QUE HA UTLIZADO
* Amigos
codebook eaf_22_1_w1 eaf_22_1_w2 eaf_22_1_w3 eaf_22_1_w4
recode eaf_22_1_w1 eaf_22_1_w2 eaf_22_1_w3 eaf_22_1_w4 (-7 -8 -9=-2)
gen me_amigos=1 if eaf_22_1_w1==1 | eaf_22_1_w2==1 | eaf_22_1_w3==1 | eaf_22_1_w4==1
replace me_amigos=0 if missing(me_amigos)
replace me_amigos=-2 if eaf_22_1_w1==-2 & eaf_22_1_w2==-2 & eaf_22_1_w3==-2 & eaf_22_1_w4==-2
replace me_amigos=-1 if busco_trab==0 & todas_olas==1
tab me_amigos if todas_olas==1

* Familia
codebook eaf_22_2_w1 eaf_22_2_w2 eaf_22_2_w3 eaf_22_2_w4
recode eaf_22_2_w1 eaf_22_2_w2 eaf_22_2_w3 eaf_22_2_w4 (-7 -8 -9=-2)
gen me_familia=1 if eaf_22_2_w1==1 | eaf_22_2_w2==1 | eaf_22_2_w3==1 | eaf_22_2_w4==1
replace me_familia=0 if missing(me_familia)
replace me_familia=-2 if eaf_22_2_w1==-2 & eaf_22_2_w2==-2 & eaf_22_2_w3==-2 & eaf_22_2_w4==-2
replace me_familia=-1 if busco_trab==0 & todas_olas==1
tab me_familia if todas_olas==1

* Pareja o ex-pareja
codebook eaf_22_3_w1 eaf_22_3_w2 eaf_22_3_w3 eaf_22_3_w4
recode eaf_22_3_w1 eaf_22_3_w2 eaf_22_3_w3 eaf_22_3_w4 (-7 -8 -9=-2)
gen me_pareja=1 if eaf_22_3_w1==1 | eaf_22_3_w2==1 | eaf_22_3_w3==1 | eaf_22_3_w4==1
replace me_pareja=0 if missing(me_pareja)
replace me_pareja=-2 if eaf_22_3_w1==-2 & eaf_22_3_w2==-2 & eaf_22_3_w3==-2 & eaf_22_3_w4==-2
replace me_pareja=-1 if busco_trab==0 & todas_olas==1
tab me_pareja if todas_olas==1

* Antiguo empleador
codebook eaf_22_4_w1 eaf_22_4_w2 eaf_22_4_w3 eaf_22_4_w4
recode eaf_22_4_w1 eaf_22_4_w2 eaf_22_4_w3 eaf_22_4_w4 (-7 -8 -9=-2)
gen me_empleador=1 if eaf_22_4_w1==1 | eaf_22_4_w2==1 | eaf_22_4_w3==1 | eaf_22_4_w4==1
replace me_empleador=0 if missing(me_empleador)
replace me_empleador=-2 if eaf_22_4_w1==-2 & eaf_22_4_w2==-2 & eaf_22_4_w3==-2 & eaf_22_4_w4==-2
replace me_empleador=-1 if busco_trab==0 & todas_olas==1
tab me_empleador if todas_olas==1

* Asistencia pública
codebook eaf_22_5_w1 eaf_22_5_w2 eaf_22_5_w3 eaf_22_5_w4
recode eaf_22_5_w1 eaf_22_5_w2 eaf_22_5_w3 eaf_22_5_w4 (-7 -8 -9=-2)
gen me_asispub=1 if eaf_22_5_w1==1 | eaf_22_5_w2==1 | eaf_22_5_w3==1 | eaf_22_5_w4==1
replace me_asispub=0 if missing(me_asispub)
replace me_asispub=-2 if eaf_22_5_w1==-2 & eaf_22_5_w2==-2 & eaf_22_5_w3==-2 & eaf_22_5_w4==-2
replace me_asispub=-1 if busco_trab==0 & todas_olas==1
tab me_asispub if todas_olas==1

* Diarios o anuncios
codebook eaf_22_6_w1 eaf_22_6_w2 eaf_22_6_w3 eaf_22_6_w4
recode eaf_22_6_w1 eaf_22_6_w2 eaf_22_6_w3 eaf_22_6_w4 (-7 -8 -9=-2)
gen me_diarios=1 if eaf_22_6_w1==1 | eaf_22_6_w2==1 | eaf_22_6_w3==1 | eaf_22_6_w4==1
replace me_diarios=0 if missing(me_diarios)
replace me_diarios=-2 if eaf_22_6_w1==-2 & eaf_22_6_w2==-2 & eaf_22_6_w3==-2 & eaf_22_6_w4==-2
replace me_diarios=-1 if busco_trab==0 & todas_olas==1
tab me_diarios if todas_olas==1

* Inet
codebook eaf_22_7_w1 eaf_22_7_w2 eaf_22_7_w3 eaf_22_7_w4
recode eaf_22_7_w1 eaf_22_7_w2 eaf_22_7_w3 eaf_22_7_w4 (-7 -8 -9=-2)
gen me_inet=1 if eaf_22_7_w1==1 | eaf_22_7_w2==1 | eaf_22_7_w3==1 | eaf_22_7_w4==1
replace me_inet=0 if missing(me_inet)
replace me_inet=-2 if eaf_22_7_w1==-2 & eaf_22_7_w2==-2 & eaf_22_7_w3==-2 & eaf_22_7_w4==-2
replace me_inet=-1 if busco_trab==0 & todas_olas==1
tab me_inet if todas_olas==1

* Postpenitenciario
codebook eaf_22_8_w1 eaf_22_8_w2 eaf_22_8_w3 eaf_22_8_w4
recode eaf_22_8_w1 eaf_22_8_w2 eaf_22_8_w3 eaf_22_8_w4 (-7 -8 -9=-2)
gen me_postpeni=1 if eaf_22_8_w1==1 | eaf_22_8_w2==1 | eaf_22_8_w3==1 | eaf_22_8_w4==1
replace me_postpeni=0 if missing(me_postpeni)
replace me_postpeni=-2 if eaf_22_8_w1==-2 & eaf_22_8_w2==-2 & eaf_22_8_w3==-2 & eaf_22_8_w4==-2
replace me_postpeni=-1 if busco_trab==0 & todas_olas==1
tab me_postpeni if todas_olas==1

* Personal gendarmería
codebook eaf_22_9_w1 eaf_22_9_w2 eaf_22_9_w3 eaf_22_9_w4
recode eaf_22_9_w1 eaf_22_9_w2 eaf_22_9_w3 eaf_22_9_w4 (-7 -8 -9=-2)
gen me_gendarmeria=1 if eaf_22_9_w1==1 | eaf_22_9_w2==1 | eaf_22_9_w3==1 | eaf_22_9_w4==1
replace me_gendarmeria=0 if missing(me_gendarmeria)
replace me_gendarmeria=-2 if eaf_22_9_w1==-2 & eaf_22_9_w2==-2 & eaf_22_9_w3==-2 & eaf_22_9_w4==-2
replace me_gendarmeria=-1 if busco_trab==0 & todas_olas==1
tab me_gendarmeria if todas_olas==1

* ONG
codebook eaf_22_10_w4
tab eaf_22_10_w4 if todas_olas==1

* Otro
codebook eaf_22_11_w1 eaf_22_11_w2 eaf_22_11_w3 eaf_22_11_w4
recode eaf_22_11_w1 eaf_22_11_w2 eaf_22_11_w3 eaf_22_11_w4 (-7 -8 -9=-2)
gen me_otro=1 if eaf_22_11_w1==1 | eaf_22_11_w2==1 | eaf_22_11_w3==1 | eaf_22_11_w4==1
replace me_otro=0 if missing(me_otro)
replace me_otro=-2 if eaf_22_11_w1==-2 & eaf_22_11_w2==-2 & eaf_22_11_w3==-2 & eaf_22_11_w4==-2
replace me_otro=-1 if busco_trab==0 & todas_olas==1
tab me_otro if todas_olas==1

gen uno=1 // genero una constante para sacar los descriptivos con tabout

tabout me_amigos me_familia me_pareja me_empleador me_asispub me_diarios me_inet me_postpeni me_gendarmeria me_otro uno if busco_tra==1 & todas_olas==1 using asd.xls, replace cells(freq col) format(1p) stats(chi2)  
tabout eaf_15_1_w0 eaf_15_2_w0 eaf_15_3_w0 eaf_15_4_w0 eaf_15_5_w0 eaf_15_6_w0 eaf_15_7_w0 eaf_15_8_w0 eaf_15_9_w0 eaf_15_10_w0 eaf_15_11_w0 eaf_15_12_w0 if busco_trab==0 & todas_olas==1 using asd2.xls, replace cells(freq col) format(1p) stats(chi2)  
tabout me_amigos me_familia me_pareja me_empleador me_asispub me_diarios me_inet me_postpeni me_gendarmeria me_otro uno if todas_olas==1 using asd1.xls, replace cells(freq col) format(1p) stats(chi2)  

**** Entre quienes trabajaron
recode me_amigos me_familia me_pareja me_empleador me_asispub me_diarios me_inet me_postpeni me_gendarmeria me_otro (-1=.)
tab1 me_amigos me_familia me_pareja me_empleador me_asispub me_diarios me_inet me_postpeni me_gendarmeria me_otro

// 2.E ///////////////////////////
// Razones no búsqueda, cuali
* Realizado en excel
* eaf_24_w2 eaf_24_w3 eaf_24_w4

label define raz_no_bus 0"Ya tiene un trabajo" 1"Antecedentes" 2"Consumo de drogas" ///
3"Cuidado de otros o tareas domesticas" 4"Delinque" 5"Esta privada de libertad" ///
6"No le gustan trabajos disponibles" 7"No lo necesita" 8"No quiere buscar" 9"Otro" ///
10"Pareja no le permite" 11"Salud" 12"Se aburrio de buscar"

* 2 meses
gen raz_no_bus_w2=.
replace raz_no_bus_w2=1 if reg_folio==50190
replace raz_no_bus_w2=1 if reg_folio==50072
replace raz_no_bus_w2=1 if reg_folio==10153
replace raz_no_bus_w2=1 if reg_folio==30193
replace raz_no_bus_w2=1 if reg_folio==10244
replace raz_no_bus_w2=1 if reg_folio==30164
replace raz_no_bus_w2=1 if reg_folio==40297
replace raz_no_bus_w2=1 if reg_folio==10003
replace raz_no_bus_w2=1 if reg_folio==30232
replace raz_no_bus_w2=1 if reg_folio==20208
replace raz_no_bus_w2=1 if reg_folio==10172
replace raz_no_bus_w2=1 if reg_folio==20201
replace raz_no_bus_w2=1 if reg_folio==40026
replace raz_no_bus_w2=1 if reg_folio==10291
replace raz_no_bus_w2=1 if reg_folio==20058
replace raz_no_bus_w2=1 if reg_folio==10293
replace raz_no_bus_w2=2 if reg_folio==20176
replace raz_no_bus_w2=2 if reg_folio==30173
replace raz_no_bus_w2=2 if reg_folio==20262
replace raz_no_bus_w2=2 if reg_folio==10027
replace raz_no_bus_w2=3 if reg_folio==50133
replace raz_no_bus_w2=3 if reg_folio==10074
replace raz_no_bus_w2=3 if reg_folio==10015
replace raz_no_bus_w2=3 if reg_folio==30118
replace raz_no_bus_w2=3 if reg_folio==40090
replace raz_no_bus_w2=4 if reg_folio==40184
replace raz_no_bus_w2=4 if reg_folio==30051
replace raz_no_bus_w2=4 if reg_folio==40179
replace raz_no_bus_w2=4 if reg_folio==40271
replace raz_no_bus_w2=4 if reg_folio==40084
replace raz_no_bus_w2=4 if reg_folio==40169
replace raz_no_bus_w2=4 if reg_folio==40287
replace raz_no_bus_w2=4 if reg_folio==40299
replace raz_no_bus_w2=4 if reg_folio==50251
replace raz_no_bus_w2=4 if reg_folio==10158
replace raz_no_bus_w2=4 if reg_folio==40215
replace raz_no_bus_w2=6 if reg_folio==20256
replace raz_no_bus_w2=6 if reg_folio==20239
replace raz_no_bus_w2=6 if reg_folio==20224
replace raz_no_bus_w2=6 if reg_folio==10077
replace raz_no_bus_w2=7 if reg_folio==40113
replace raz_no_bus_w2=7 if reg_folio==20260
replace raz_no_bus_w2=7 if reg_folio==10177
replace raz_no_bus_w2=7 if reg_folio==40019
replace raz_no_bus_w2=7 if reg_folio==30188
replace raz_no_bus_w2=7 if reg_folio==30241
replace raz_no_bus_w2=7 if reg_folio==40156
replace raz_no_bus_w2=7 if reg_folio==50037
replace raz_no_bus_w2=7 if reg_folio==40107
replace raz_no_bus_w2=8 if reg_folio==30207
replace raz_no_bus_w2=8 if reg_folio==10233
replace raz_no_bus_w2=8 if reg_folio==30264
replace raz_no_bus_w2=8 if reg_folio==20004
replace raz_no_bus_w2=8 if reg_folio==30021
replace raz_no_bus_w2=8 if reg_folio==30168
replace raz_no_bus_w2=8 if reg_folio==10007
replace raz_no_bus_w2=8 if reg_folio==30266
replace raz_no_bus_w2=8 if reg_folio==20255
replace raz_no_bus_w2=9 if reg_folio==40106
replace raz_no_bus_w2=9 if reg_folio==30057
replace raz_no_bus_w2=9 if reg_folio==10202
replace raz_no_bus_w2=10 if reg_folio==20257
replace raz_no_bus_w2=10 if reg_folio==10213
replace raz_no_bus_w2=10 if reg_folio==30183
replace raz_no_bus_w2=11 if reg_folio==40152
replace raz_no_bus_w2=11 if reg_folio==40199
replace raz_no_bus_w2=11 if reg_folio==10099
replace raz_no_bus_w2=11 if reg_folio==30206
replace raz_no_bus_w2=11 if reg_folio==10044
replace raz_no_bus_w2=11 if reg_folio==40065
replace raz_no_bus_w2=11 if reg_folio==40050
replace raz_no_bus_w2=11 if reg_folio==40030
replace raz_no_bus_w2=11 if reg_folio==10288
replace raz_no_bus_w2=12 if reg_folio==40284
replace raz_no_bus_w2=0 if reg_folio==20081
replace raz_no_bus_w2=0 if reg_folio==10102
replace raz_no_bus_w2=0 if reg_folio==50281
replace raz_no_bus_w2=0 if reg_folio==40022
replace raz_no_bus_w2=0 if reg_folio==40045
replace raz_no_bus_w2=0 if reg_folio==20041
replace raz_no_bus_w2=0 if reg_folio==40066
replace raz_no_bus_w2=0 if reg_folio==10035
replace raz_no_bus_w2=0 if reg_folio==10075
replace raz_no_bus_w2=0 if reg_folio==40059
replace raz_no_bus_w2=0 if reg_folio==40006
replace raz_no_bus_w2=0 if reg_folio==40275
replace raz_no_bus_w2=0 if reg_folio==20017
replace raz_no_bus_w2=0 if reg_folio==10023
replace raz_no_bus_w2=0 if reg_folio==30247
replace raz_no_bus_w2=0 if reg_folio==30303
replace raz_no_bus_w2=0 if reg_folio==30082
replace raz_no_bus_w2=0 if reg_folio==40108
replace raz_no_bus_w2=0 if reg_folio==30227
replace raz_no_bus_w2=0 if reg_folio==10053
replace raz_no_bus_w2=0 if reg_folio==10228
replace raz_no_bus_w2=0 if reg_folio==40268
replace raz_no_bus_w2=0 if reg_folio==50226
replace raz_no_bus_w2=0 if reg_folio==50216

label value raz_no_bus_w2 raz_no_bus

* 6 meses
gen raz_no_bus_w3=.
replace raz_no_bus_w3=1 if reg_folio==10099
replace raz_no_bus_w3=1 if reg_folio==20058
replace raz_no_bus_w3=1 if reg_folio==40287
replace raz_no_bus_w3=1 if reg_folio==10197
replace raz_no_bus_w3=1 if reg_folio==20283
replace raz_no_bus_w3=1 if reg_folio==40194
replace raz_no_bus_w3=1 if reg_folio==50249
replace raz_no_bus_w3=1 if reg_folio==20161
replace raz_no_bus_w3=1 if reg_folio==20201
replace raz_no_bus_w3=1 if reg_folio==20219
replace raz_no_bus_w3=1 if reg_folio==40106
replace raz_no_bus_w3=1 if reg_folio==30178
replace raz_no_bus_w3=1 if reg_folio==10074
replace raz_no_bus_w3=2 if reg_folio==40210
replace raz_no_bus_w3=2 if reg_folio==40012
replace raz_no_bus_w3=2 if reg_folio==10075
replace raz_no_bus_w3=2 if reg_folio==30001
replace raz_no_bus_w3=2 if reg_folio==10285
replace raz_no_bus_w3=2 if reg_folio==40184
replace raz_no_bus_w3=3 if reg_folio==40002
replace raz_no_bus_w3=3 if reg_folio==50236
replace raz_no_bus_w3=3 if reg_folio==10023
replace raz_no_bus_w3=3 if reg_folio==30188
replace raz_no_bus_w3=3 if reg_folio==50180
replace raz_no_bus_w3=3 if reg_folio==30082
replace raz_no_bus_w3=3 if reg_folio==30232
replace raz_no_bus_w3=3 if reg_folio==30092
replace raz_no_bus_w3=3 if reg_folio==30206
replace raz_no_bus_w3=3 if reg_folio==10052
replace raz_no_bus_w3=3 if reg_folio==20017
replace raz_no_bus_w3=3 if reg_folio==40090
replace raz_no_bus_w3=4 if reg_folio==50049
replace raz_no_bus_w3=4 if reg_folio==20260
replace raz_no_bus_w3=4 if reg_folio==40271
replace raz_no_bus_w3=4 if reg_folio==10233
replace raz_no_bus_w3=4 if reg_folio==50230
replace raz_no_bus_w3=4 if reg_folio==20224
replace raz_no_bus_w3=4 if reg_folio==40169
replace raz_no_bus_w3=4 if reg_folio==40299
replace raz_no_bus_w3=4 if reg_folio==20208
replace raz_no_bus_w3=4 if reg_folio==40006
replace raz_no_bus_w3=4 if reg_folio==20157
replace raz_no_bus_w3=4 if reg_folio==30116
replace raz_no_bus_w3=4 if reg_folio==50251
replace raz_no_bus_w3=4 if reg_folio==10244
replace raz_no_bus_w3=4 if reg_folio==40084
replace raz_no_bus_w3=4 if reg_folio==10149
replace raz_no_bus_w3=4 if reg_folio==20255
replace raz_no_bus_w3=4 if reg_folio==40215
replace raz_no_bus_w3=4 if reg_folio==20256
replace raz_no_bus_w3=4 if reg_folio==10158
replace raz_no_bus_w3=5 if reg_folio==10153
replace raz_no_bus_w3=7 if reg_folio==40174
replace raz_no_bus_w3=7 if reg_folio==10177
replace raz_no_bus_w3=7 if reg_folio==40166
replace raz_no_bus_w3=7 if reg_folio==20257
replace raz_no_bus_w3=7 if reg_folio==10043
replace raz_no_bus_w3=7 if reg_folio==40284
replace raz_no_bus_w3=7 if reg_folio==40235
replace raz_no_bus_w3=7 if reg_folio==10293
replace raz_no_bus_w3=8 if reg_folio==20286
replace raz_no_bus_w3=8 if reg_folio==30264
replace raz_no_bus_w3=8 if reg_folio==30193
replace raz_no_bus_w3=8 if reg_folio==20112
replace raz_no_bus_w3=8 if reg_folio==20239
replace raz_no_bus_w3=8 if reg_folio==20198
replace raz_no_bus_w3=8 if reg_folio==30021
replace raz_no_bus_w3=8 if reg_folio==30237
replace raz_no_bus_w3=8 if reg_folio==30266
replace raz_no_bus_w3=8 if reg_folio==30183
replace raz_no_bus_w3=8 if reg_folio==30269
replace raz_no_bus_w3=9 if reg_folio==30155
replace raz_no_bus_w3=9 if reg_folio==10102
replace raz_no_bus_w3=9 if reg_folio==40022
replace raz_no_bus_w3=9 if reg_folio==40268
replace raz_no_bus_w3=9 if reg_folio==30207
replace raz_no_bus_w3=9 if reg_folio==20261
replace raz_no_bus_w3=9 if reg_folio==20262
replace raz_no_bus_w3=11 if reg_folio==50104
replace raz_no_bus_w3=11 if reg_folio==10053
replace raz_no_bus_w3=11 if reg_folio==10272
replace raz_no_bus_w3=11 if reg_folio==30047
replace raz_no_bus_w3=11 if reg_folio==40050
replace raz_no_bus_w3=11 if reg_folio==30241
replace raz_no_bus_w3=11 if reg_folio==10003
replace raz_no_bus_w3=11 if reg_folio==10288
replace raz_no_bus_w3=11 if reg_folio==40065
replace raz_no_bus_w3=11 if reg_folio==10044
replace raz_no_bus_w3=11 if reg_folio==10027
replace raz_no_bus_w3=12 if reg_folio==40199
replace raz_no_bus_w3=12 if reg_folio==40091
replace raz_no_bus_w3=0 if reg_folio==10192
replace raz_no_bus_w3=0 if reg_folio==20142
replace raz_no_bus_w3=0 if reg_folio==50072
replace raz_no_bus_w3=0 if reg_folio==30300
replace raz_no_bus_w3=0 if reg_folio==20041
replace raz_no_bus_w3=0 if reg_folio==30134
replace raz_no_bus_w3=0 if reg_folio==10223
replace raz_no_bus_w3=0 if reg_folio==40275
replace raz_no_bus_w3=0 if reg_folio==10250
replace raz_no_bus_w3=0 if reg_folio==10172
replace raz_no_bus_w3=0 if reg_folio==40107
replace raz_no_bus_w3=0 if reg_folio==40108
replace raz_no_bus_w3=0 if reg_folio==20263
replace raz_no_bus_w3=0 if reg_folio==30118
replace raz_no_bus_w3=0 if reg_folio==40059
replace raz_no_bus_w3=0 if reg_folio==40045
replace raz_no_bus_w3=0 if reg_folio==10098
replace raz_no_bus_w3=0 if reg_folio==40066
replace raz_no_bus_w3=0 if reg_folio==10035
replace raz_no_bus_w3=0 if reg_folio==40068
replace raz_no_bus_w3=0 if reg_folio==30136
replace raz_no_bus_w3=0 if reg_folio==30227
replace raz_no_bus_w3=0 if reg_folio==40030
replace raz_no_bus_w3=0 if reg_folio==50216
replace raz_no_bus_w3=0 if reg_folio==10187
replace raz_no_bus_w3=0 if reg_folio==10228
replace raz_no_bus_w3=0 if reg_folio==50037
replace raz_no_bus_w3=0 if reg_folio==30278
replace raz_no_bus_w3=0 if reg_folio==50133
replace raz_no_bus_w3=0 if reg_folio==30247
replace raz_no_bus_w3=0 if reg_folio==40156
replace raz_no_bus_w3=0 if reg_folio==10213

label value raz_no_bus_w3 raz_no_bus

* 12 meses
gen raz_no_bus_w4=.
replace raz_no_bus_w4=1 if reg_folio==50249
replace raz_no_bus_w4=1 if reg_folio==30139
replace raz_no_bus_w4=1 if reg_folio==10197
replace raz_no_bus_w4=1 if reg_folio==40012
replace raz_no_bus_w4=1 if reg_folio==40054
replace raz_no_bus_w4=1 if reg_folio==30193
replace raz_no_bus_w4=1 if reg_folio==30266
replace raz_no_bus_w4=1 if reg_folio==40199
replace raz_no_bus_w4=1 if reg_folio==20219
replace raz_no_bus_w4=1 if reg_folio==20147
replace raz_no_bus_w4=1 if reg_folio==20161
replace raz_no_bus_w4=1 if reg_folio==20201
replace raz_no_bus_w4=1 if reg_folio==40106
replace raz_no_bus_w4=1 if reg_folio==20256
replace raz_no_bus_w4=1 if reg_folio==30118
replace raz_no_bus_w4=1 if reg_folio==30001
replace raz_no_bus_w4=1 if reg_folio==30178
replace raz_no_bus_w4=1 if reg_folio==10244
replace raz_no_bus_w4=1 if reg_folio==10182
replace raz_no_bus_w4=1 if reg_folio==20283
replace raz_no_bus_w4=1 if reg_folio==40090
replace raz_no_bus_w4=2 if reg_folio==10075
replace raz_no_bus_w4=2 if reg_folio==10285
replace raz_no_bus_w4=2 if reg_folio==40152
replace raz_no_bus_w4=2 if reg_folio==20008
replace raz_no_bus_w4=2 if reg_folio==20081
replace raz_no_bus_w4=2 if reg_folio==10192
replace raz_no_bus_w4=3 if reg_folio==50209
replace raz_no_bus_w4=3 if reg_folio==50086
replace raz_no_bus_w4=3 if reg_folio==20017
replace raz_no_bus_w4=3 if reg_folio==50236
replace raz_no_bus_w4=3 if reg_folio==10052
replace raz_no_bus_w4=3 if reg_folio==50154
replace raz_no_bus_w4=3 if reg_folio==50150
replace raz_no_bus_w4=3 if reg_folio==30264
replace raz_no_bus_w4=3 if reg_folio==40002
replace raz_no_bus_w4=3 if reg_folio==30206
replace raz_no_bus_w4=3 if reg_folio==10099
replace raz_no_bus_w4=3 if reg_folio==30088
replace raz_no_bus_w4=3 if reg_folio==20042
replace raz_no_bus_w4=3 if reg_folio==40108
replace raz_no_bus_w4=4 if reg_folio==20261
replace raz_no_bus_w4=4 if reg_folio==20176
replace raz_no_bus_w4=4 if reg_folio==40215
replace raz_no_bus_w4=4 if reg_folio==50205
replace raz_no_bus_w4=4 if reg_folio==40235
replace raz_no_bus_w4=4 if reg_folio==40271
replace raz_no_bus_w4=4 if reg_folio==10158
replace raz_no_bus_w4=4 if reg_folio==20208
replace raz_no_bus_w4=4 if reg_folio==20259
replace raz_no_bus_w4=4 if reg_folio==10293
replace raz_no_bus_w4=4 if reg_folio==40169
replace raz_no_bus_w4=4 if reg_folio==20255
replace raz_no_bus_w4=5 if reg_folio==20157
replace raz_no_bus_w4=5 if reg_folio==10153
replace raz_no_bus_w4=5 if reg_folio==50049
replace raz_no_bus_w4=5 if reg_folio==50214
replace raz_no_bus_w4=5 if reg_folio==40084
replace raz_no_bus_w4=6 if reg_folio==20260
replace raz_no_bus_w4=7 if reg_folio==50124
replace raz_no_bus_w4=7 if reg_folio==20058
replace raz_no_bus_w4=7 if reg_folio==10288
replace raz_no_bus_w4=7 if reg_folio==40210
replace raz_no_bus_w4=7 if reg_folio==30051
replace raz_no_bus_w4=7 if reg_folio==40284
replace raz_no_bus_w4=7 if reg_folio==30173
replace raz_no_bus_w4=7 if reg_folio==30082
replace raz_no_bus_w4=7 if reg_folio==40299
replace raz_no_bus_w4=7 if reg_folio==30021
replace raz_no_bus_w4=8 if reg_folio==30237
replace raz_no_bus_w4=8 if reg_folio==20028
replace raz_no_bus_w4=8 if reg_folio==20262
replace raz_no_bus_w4=8 if reg_folio==10233
replace raz_no_bus_w4=8 if reg_folio==30168
replace raz_no_bus_w4=8 if reg_folio==50251
replace raz_no_bus_w4=8 if reg_folio==20224
replace raz_no_bus_w4=8 if reg_folio==30116
replace raz_no_bus_w4=8 if reg_folio==30241
replace raz_no_bus_w4=8 if reg_folio==10003
replace raz_no_bus_w4=8 if reg_folio==20257
replace raz_no_bus_w4=8 if reg_folio==40287
replace raz_no_bus_w4=8 if reg_folio==30232
replace raz_no_bus_w4=8 if reg_folio==10119
replace raz_no_bus_w4=8 if reg_folio==30207
replace raz_no_bus_w4=9 if reg_folio==40068
replace raz_no_bus_w4=9 if reg_folio==40184
replace raz_no_bus_w4=9 if reg_folio==20071
replace raz_no_bus_w4=9 if reg_folio==40040
replace raz_no_bus_w4=9 if reg_folio==50105
replace raz_no_bus_w4=9 if reg_folio==20181
replace raz_no_bus_w4=9 if reg_folio==40006
replace raz_no_bus_w4=9 if reg_folio==40225
replace raz_no_bus_w4=9 if reg_folio==10282
replace raz_no_bus_w4=9 if reg_folio==30047
replace raz_no_bus_w4=9 if reg_folio==40156
replace raz_no_bus_w4=10 if reg_folio==10272
replace raz_no_bus_w4=11 if reg_folio==30269
replace raz_no_bus_w4=11 if reg_folio==50104
replace raz_no_bus_w4=11 if reg_folio==50072
replace raz_no_bus_w4=11 if reg_folio==10202
replace raz_no_bus_w4=11 if reg_folio==10053
replace raz_no_bus_w4=11 if reg_folio==10027
replace raz_no_bus_w4=11 if reg_folio==10044
replace raz_no_bus_w4=11 if reg_folio==40050
replace raz_no_bus_w4=11 if reg_folio==30110
replace raz_no_bus_w4=11 if reg_folio==20198
replace raz_no_bus_w4=11 if reg_folio==10291
replace raz_no_bus_w4=12 if reg_folio==20032
replace raz_no_bus_w4=0 if reg_folio==30303
replace raz_no_bus_w4=0 if reg_folio==10007
replace raz_no_bus_w4=0 if reg_folio==40268
replace raz_no_bus_w4=0 if reg_folio==10223
replace raz_no_bus_w4=0 if reg_folio==10102
replace raz_no_bus_w4=0 if reg_folio==10055
replace raz_no_bus_w4=0 if reg_folio==10077
replace raz_no_bus_w4=0 if reg_folio==40194
replace raz_no_bus_w4=0 if reg_folio==40066
replace raz_no_bus_w4=0 if reg_folio==40275
replace raz_no_bus_w4=0 if reg_folio==20195
replace raz_no_bus_w4=0 if reg_folio==30188
replace raz_no_bus_w4=0 if reg_folio==40065
replace raz_no_bus_w4=0 if reg_folio==10023
replace raz_no_bus_w4=0 if reg_folio==40030
replace raz_no_bus_w4=0 if reg_folio==10098
replace raz_no_bus_w4=0 if reg_folio==20041
replace raz_no_bus_w4=0 if reg_folio==20286
replace raz_no_bus_w4=0 if reg_folio==20013
replace raz_no_bus_w4=0 if reg_folio==50126
replace raz_no_bus_w4=0 if reg_folio==10250
replace raz_no_bus_w4=0 if reg_folio==20263
replace raz_no_bus_w4=0 if reg_folio==30227
replace raz_no_bus_w4=0 if reg_folio==40297
replace raz_no_bus_w4=0 if reg_folio==10177
replace raz_no_bus_w4=0 if reg_folio==40010
replace raz_no_bus_w4=0 if reg_folio==20096
replace raz_no_bus_w4=0 if reg_folio==30136
replace raz_no_bus_w4=0 if reg_folio==30039
replace raz_no_bus_w4=0 if reg_folio==40107
replace raz_no_bus_w4=0 if reg_folio==30278
replace raz_no_bus_w4=0 if reg_folio==20142
replace raz_no_bus_w4=0 if reg_folio==20171
replace raz_no_bus_w4=0 if reg_folio==20200
replace raz_no_bus_w4=0 if reg_folio==10187
replace raz_no_bus_w4=0 if reg_folio==50226
replace raz_no_bus_w4=0 if reg_folio==50037
replace raz_no_bus_w4=0 if reg_folio==50216
replace raz_no_bus_w4=0 if reg_folio==30155
replace raz_no_bus_w4=0 if reg_folio==10213
replace raz_no_bus_w4=0 if reg_folio==10228
replace raz_no_bus_w4=0 if reg_folio==40059
replace raz_no_bus_w4=0 if reg_folio==30247

label value raz_no_bus_w4 raz_no_bus

tab1 raz_no_bus_w2 raz_no_bus_w3 raz_no_bus_w4 if todas_olas==1

* Reemplazo para quienes sí buscaron empleo en cada ola.
replace raz_no_bus_w2=-1 if eaf_21_w2==1 & missing(raz_no_bus_w2)
replace raz_no_bus_w3=-1 if eaf_21_w3==1 & missing(raz_no_bus_w3)
replace raz_no_bus_w4=-1 if eaf_21_w4==1 & missing(raz_no_bus_w4)

* versión 2
gen raz_no_bus_w2_v2=raz_no_bus_w2
gen raz_no_bus_w3_v2=raz_no_bus_w3
gen raz_no_bus_w4_v2=raz_no_bus_w4

recode raz_no_bus_w2_v2 raz_no_bus_w3_v2 raz_no_bus_w4_v2 (0=0) (4=1) (1=2) (2=3) (3=4) ///
(5=5) (10=6) (11=7) (6=8) (7=9) (8=10) (12=11) (9=12)

label define raz_no_bus_v2 -1"Busco trabajo en esa ola" 0"Ya tiene un trabajo" 1"Delinque" 2"Antecedentes" ///
3"Consumo de drogas" 4"Cuidado de otros o tareas domesticas" 5"Esta privada de libertad" ///
6"Pareja no le permite" 7"Salud" 8"No le gustan trabajos disponibles" 9"No lo necesita" ///
10"No quiere buscar" 11"Se aburrio de buscar" 12"Otro" 13"NR"

label value raz_no_bus_w2_v2 raz_no_bus_w3_v2 raz_no_bus_w4_v2 raz_no_bus_v2
tab1 raz_no_bus_w2_v2 raz_no_bus_w3_v2 raz_no_bus_w4_v2 if todas_olas==1

list reg_folio eaf_21_w2 eaf_24_w2 if raz_no_bus_w2_v2==. & todas_olas==1
list reg_folio eaf_21_w3 eaf_24_w3 if raz_no_bus_w3_v2==. & todas_olas==1
list reg_folio eaf_21_w4 eaf_24_w4 if raz_no_bus_w4_v2==. & todas_olas==1

* Elimino los casos con NR en razones
replace raz_no_bus_w2_v2=13 if raz_no_bus_w2_v2==. & todas_olas==1
replace raz_no_bus_w3_v2=13 if raz_no_bus_w3_v2==. & todas_olas==1
replace raz_no_bus_w4_v2=13 if raz_no_bus_w4_v2==. & todas_olas==1

tab1 raz_no_bus_w2_v2 raz_no_bus_w3_v2 raz_no_bus_w4_v2 if todas_olas==1

codebook raz_no_bus_w2_v2

// Versión 3
gen raz_no_bus_w2_v3=raz_no_bus_w2_v2
gen raz_no_bus_w3_v3=raz_no_bus_w3_v2
gen raz_no_bus_w4_v3=raz_no_bus_w4_v2

recode raz_no_bus_w2_v3 raz_no_bus_w3_v3 raz_no_bus_w4_v3 (-1=-1) (0=0) (1=1) (2/7=2) (8/11=3) (12=4) (13=5)
label define raz_no_bus_v3 -1"Busco trabajo en esa ola" 0"Ya tiene un trabajo" 1"Delinque" 2"No puede" 3"No quiere" 4"Otro" 5"NR"
label value raz_no_bus_w2_v3 raz_no_bus_w3_v3 raz_no_bus_w4_v3 raz_no_bus_v3
tab1 raz_no_bus_w2_v3 raz_no_bus_w3_v3 raz_no_bus_w4_v3 if todas_olas==1

* Descriptivos con variables que genero después (por eso tienen asterisco)
* tab raz_no_bus_w2_v3 trab_2mes if todas_olas==1, col chi2 nofreq
* tab raz_no_bus_w3_v3 trab_6mes if todas_olas==1, col chi2 nofreq
* tab raz_no_bus_w4_v3 trab_12mes if todas_olas==1, col chi2 nofreq

// 2.F ///////////////////////////
// LB: % que declara relevancia empleo al salir en libertad
recode eaf_14_w0 (-9=.)	
tab eaf_14_w0 busco_trab if todas_olas==1, row chi2 


// 2.2.A /////////////
// Overlap nunca trabajaron y nunca buscaron

/// TIPOS DE TRABAJO: PARA OTROS (DEPENDIENTE) VS CUENTA PROPIA
/// Cuali realizado en excel
* La descripción del tipo de empleo se realiza en base a dos categorías: 
* la dependencia y la formalidad del empleo. Para la primera de ellas, 
* se distinguió entre empleos dependientes y por cuenta propia. 
* En términos de formalidad, se distingue entre trabajos formales 
* (es decir, empleos donde la mujer tenía contrato, se le pagaban imposiciones o entregaba boleta) 
* y trabajos informales (es decir, aquellos empleos que no poseían las características anteriores). 
* Entre los empleos dependientes reportados se encuentran trabajos asociados a aseo 
* de hogares y empresas, a servicios de alimentación (por ejemplo, ayudante de cocina, garzona), 
* y ventas (en minimarket, en locales de comida, etc.). En menor medida, aparecen labores 
* asociadas al cuidado de otros. Los trabajos de aseo son particularmente frecuentes 
* entre los empleos dependientes. En cuanto a los empleos por cuenta propia, priman 
* labores asociadas a venta (de ropa, de perfumes, comida, principalmente en la feria o en la calle).

// PRIMERA SEMANA
// Tipo de trabajo por cuenta propia primera semana
gen tipotrabajo_cp_1sem=.
replace tipotrabajo_cp_1sem=1  if reg_folio==20142
replace tipotrabajo_cp_1sem=1  if reg_folio==40275
replace tipotrabajo_cp_1sem=1  if reg_folio==10187
replace tipotrabajo_cp_1sem=3  if reg_folio==40090
replace tipotrabajo_cp_1sem=1  if reg_folio==30241
replace tipotrabajo_cp_1sem=3  if reg_folio==50163
replace tipotrabajo_cp_1sem=3  if reg_folio==50072
replace tipotrabajo_cp_1sem=1  if reg_folio==40066
replace tipotrabajo_cp_1sem=3  if reg_folio==50126
replace tipotrabajo_cp_1sem=3  if reg_folio==40010
replace tipotrabajo_cp_1sem=3  if reg_folio==30303
replace tipotrabajo_cp_1sem=3  if reg_folio==20041
replace tipotrabajo_cp_1sem=3  if reg_folio==40297
replace tipotrabajo_cp_1sem=3  if reg_folio==40152
replace tipotrabajo_cp_1sem=3  if reg_folio==30069
replace tipotrabajo_cp_1sem=3  if reg_folio==40106
replace tipotrabajo_cp_1sem=3  if reg_folio==40108
replace tipotrabajo_cp_1sem=3  if reg_folio==30136
replace tipotrabajo_cp_1sem=3  if reg_folio==20171
replace tipotrabajo_cp_1sem=3  if reg_folio==40059
replace tipotrabajo_cp_1sem=3  if reg_folio==30110
replace tipotrabajo_cp_1sem=3  if reg_folio==40012
replace tipotrabajo_cp_1sem=3  if reg_folio==50216
replace tipotrabajo_cp_1sem=3  if reg_folio==30247
replace tipotrabajo_cp_1sem=3  if reg_folio==30057
replace tipotrabajo_cp_1sem=3  if reg_folio==20096
replace tipotrabajo_cp_1sem=3  if reg_folio==30188

label define tipo_trabajo_cp 1"Cuenta propia" 0"Para otros"
recode tipotrabajo_cp_1sem (3=1) (1=0)
label value tipotrabajo_cp_1sem tipo_trabajo_cp
tab tipotrabajo_cp_1sem if todas_olas==1, m

// Tipo de trabajo en hoja trabajo para otros remunerado
gen tipotrabajo_po_1sem=.
replace tipotrabajo_po_1sem=1  if reg_folio==10213
replace tipotrabajo_po_1sem=1  if reg_folio==20095
replace tipotrabajo_po_1sem=1  if reg_folio==20200
replace tipotrabajo_po_1sem=1  if reg_folio==30139
replace tipotrabajo_po_1sem=1  if reg_folio==30303
replace tipotrabajo_po_1sem=1  if reg_folio==40054
replace tipotrabajo_po_1sem=1  if reg_folio==40059
replace tipotrabajo_po_1sem=1  if reg_folio==50126
replace tipotrabajo_po_1sem=1  if reg_folio==50160
tab tipotrabajo_po_1sem if todas_olas==1, m

label define tipotrabajo_po 1"Para otros" 0"Cuenta propia"
label value tipotrabajo_po_1sem tipotrabajo_po
tab tipotrabajo_po_1sem if todas_olas==1, m

// Trabajó para otros
gen trabajo_remunarado_1sem=1 if tipotrabajo_po_1sem==1 | tipotrabajo_cp_1sem==0
replace trabajo_remunarado_1sem=0 if missing(trabajo_remunarado_1sem)
replace trabajo_remunarado_1sem=. if tipotrabajo_po_1sem==. & tipotrabajo_cp_1sem==.
label value trabajo_remunarado_1sem tipotrabajo_po
tab trabajo_remunarado_1sem if todas_olas==1, m

// Trabajó por cuenta propia
gen trabajo_cuentapropia_1sem=1 if tipotrabajo_po_1sem==0 | tipotrabajo_cp_1sem==1
replace trabajo_cuentapropia_1sem=0 if missing(trabajo_cuentapropia_1sem)
replace trabajo_cuentapropia_1sem=. if tipotrabajo_po_1sem==. & tipotrabajo_cp_1sem==.
label value trabajo_cuentapropia_1sem tipo_trabajo_cp
tab trabajo_cuentapropia_1sem if todas_olas==1, m

tab1 trabajo_cuentapropia_1sem trabajo_remunarado_1sem if todas_olas==1

// Tipo trabajos
gen trabajos_1sem= trabajo_cuentapropia_1sem*10+trabajo_remunarado_1sem
tab trabajos_1sem // 1=Solo remunerado 10=Solo cuenta propia 11=Ambos
recode trabajos_1sem (10=2) (11=3)
replace trabajos_1sem=0 if missing(trabajos_1sem)
replace trabajos_1sem=. if reg_fecha_w1==""
label define trabajos 0"No trabajó" 1"Solo remunerado" 2"Solo cuenta propia" 3"Ambos"
label value trabajos_1sem trabajos
tab trabajos_1sem if todas_olas==1, m

// Nùmero de trabajos
gen n_trabajos_1sem=trabajos_1sem
codebook trabajos_1sem
recode n_trabajos_1sem (0=0) (1 2=1) (3=2)
tab n_trabajos_1sem if todas_olas==1

// DOS MESES /////////////////////////////////////////
// Tipo de trabajo por cuenta propia
// Trabajo por cuenta propia 1
gen tipotrabajo_cp1_2mes=.
replace tipotrabajo_cp1_2mes=1  if reg_folio==30232
replace tipotrabajo_cp1_2mes=3  if reg_folio==20041
replace tipotrabajo_cp1_2mes=3  if reg_folio==30118
replace tipotrabajo_cp1_2mes=3  if reg_folio==30139
replace tipotrabajo_cp1_2mes=3  if reg_folio==30136
replace tipotrabajo_cp1_2mes=3  if reg_folio==30073
replace tipotrabajo_cp1_2mes=3  if reg_folio==30178
replace tipotrabajo_cp1_2mes=1  if reg_folio==30057
replace tipotrabajo_cp1_2mes=3  if reg_folio==30188
replace tipotrabajo_cp1_2mes=1  if reg_folio==40152
replace tipotrabajo_cp1_2mes=3  if reg_folio==40122
replace tipotrabajo_cp1_2mes=3  if reg_folio==30273
replace tipotrabajo_cp1_2mes=3  if reg_folio==30069
replace tipotrabajo_cp1_2mes=3  if reg_folio==30082
replace tipotrabajo_cp1_2mes=3  if reg_folio==30088
replace tipotrabajo_cp1_2mes=3  if reg_folio==40090
replace tipotrabajo_cp1_2mes=3  if reg_folio==40068
replace tipotrabajo_cp1_2mes=3  if reg_folio==40297
replace tipotrabajo_cp1_2mes=1  if reg_folio==40012
replace tipotrabajo_cp1_2mes=3  if reg_folio==40006
replace tipotrabajo_cp1_2mes=1  if reg_folio==40225
replace tipotrabajo_cp1_2mes=3  if reg_folio==40210
replace tipotrabajo_cp1_2mes=3  if reg_folio==40275
replace tipotrabajo_cp1_2mes=3  if reg_folio==40166
replace tipotrabajo_cp1_2mes=1  if reg_folio==40046
replace tipotrabajo_cp1_2mes=3  if reg_folio==40010
replace tipotrabajo_cp1_2mes=3  if reg_folio==40059
replace tipotrabajo_cp1_2mes=3  if reg_folio==40045
replace tipotrabajo_cp1_2mes=1  if reg_folio==40066
replace tipotrabajo_cp1_2mes=3  if reg_folio==40108
replace tipotrabajo_cp1_2mes=1  if reg_folio==40026
replace tipotrabajo_cp1_2mes=3  if reg_folio==40019
replace tipotrabajo_cp1_2mes=3  if reg_folio==40106
replace tipotrabajo_cp1_2mes=3  if reg_folio==30227
replace tipotrabajo_cp1_2mes=3  if reg_folio==30247
replace tipotrabajo_cp1_2mes=3  if reg_folio==20262
replace tipotrabajo_cp1_2mes=3  if reg_folio==20061
replace tipotrabajo_cp1_2mes=3  if reg_folio==20286
replace tipotrabajo_cp1_2mes=3  if reg_folio==40268
replace tipotrabajo_cp1_2mes=3  if reg_folio==20171
replace tipotrabajo_cp1_2mes=3  if reg_folio==10053
replace tipotrabajo_cp1_2mes=3  if reg_folio==10087
replace tipotrabajo_cp1_2mes=3  if reg_folio==20263
replace tipotrabajo_cp1_2mes=3  if reg_folio==10228
replace tipotrabajo_cp1_2mes=3  if reg_folio==10187
replace tipotrabajo_cp1_2mes=3  if reg_folio==10052
replace tipotrabajo_cp1_2mes=3  if reg_folio==10023
replace tipotrabajo_cp1_2mes=3  if reg_folio==10011
replace tipotrabajo_cp1_2mes=3  if reg_folio==20048
replace tipotrabajo_cp1_2mes=3  if reg_folio==20198
replace tipotrabajo_cp1_2mes=3  if reg_folio==20017
replace tipotrabajo_cp1_2mes=3  if reg_folio==20096
replace tipotrabajo_cp1_2mes=3  if reg_folio==20112
replace tipotrabajo_cp1_2mes=3  if reg_folio==20058
replace tipotrabajo_cp1_2mes=3  if reg_folio==20229
replace tipotrabajo_cp1_2mes=3  if reg_folio==10291
replace tipotrabajo_cp1_2mes=3  if reg_folio==20147
replace tipotrabajo_cp1_2mes=3  if reg_folio==10119
replace tipotrabajo_cp1_2mes=3  if reg_folio==10102
replace tipotrabajo_cp1_2mes=3  if reg_folio==10099
replace tipotrabajo_cp1_2mes=3  if reg_folio==10055
replace tipotrabajo_cp1_2mes=3  if reg_folio==10043
replace tipotrabajo_cp1_2mes=3  if reg_folio==50037
replace tipotrabajo_cp1_2mes=3  if reg_folio==50063
replace tipotrabajo_cp1_2mes=3  if reg_folio==50070
replace tipotrabajo_cp1_2mes=3  if reg_folio==50226
replace tipotrabajo_cp1_2mes=3  if reg_folio==50076
replace tipotrabajo_cp1_2mes=3  if reg_folio==50124
replace tipotrabajo_cp1_2mes=3  if reg_folio==50236
replace tipotrabajo_cp1_2mes=3  if reg_folio==50271
replace tipotrabajo_cp1_2mes=3  if reg_folio==50056
replace tipotrabajo_cp1_2mes=3  if reg_folio==50060
replace tipotrabajo_cp1_2mes=3  if reg_folio==50216
replace tipotrabajo_cp1_2mes=3  if reg_folio==50072

recode tipotrabajo_cp1_2mes (3=1) (1=0)
label value tipotrabajo_cp1_2mes tipo_trabajo_cp
tab tipotrabajo_cp1 if todas_olas==1, m

// Trabajo por cuenta propia 2 
gen tipotrabajo_cp2_2mes=.
replace tipotrabajo_cp2_2mes=1  if reg_folio==30136
replace tipotrabajo_cp2_2mes=3  if reg_folio==30082
replace tipotrabajo_cp2_2mes=3  if reg_folio==30088
replace tipotrabajo_cp2_2mes=3  if reg_folio==40106
replace tipotrabajo_cp2_2mes=3  if reg_folio==30247
replace tipotrabajo_cp2_2mes=3  if reg_folio==20171
replace tipotrabajo_cp2_2mes=3  if reg_folio==20058
replace tipotrabajo_cp2_2mes=1  if reg_folio==10119
replace tipotrabajo_cp2_2mes=3  if reg_folio==10055
replace tipotrabajo_cp2_2mes=3  if reg_folio==50056
replace tipotrabajo_cp2_2mes=3  if reg_folio==50060
recode tipotrabajo_cp2_2mes (3=1) (1=0)
label value tipotrabajo_cp2_2mes tipo_trabajo_cp
tab tipotrabajo_cp2_2mes if todas_olas==1, m

// Trabajo por cuenta propia 3
gen tipotrabajo_cp3_2mes=.
replace tipotrabajo_cp3_2mes=1  if reg_folio==20171
replace tipotrabajo_cp3_2mes=1  if reg_folio==50060
recode tipotrabajo_cp3_2mes (3=1) (1=0)
label value tipotrabajo_cp3_2mes tipo_trabajo_cp
tab tipotrabajo_cp3_2mes if todas_olas==1, m

// Tipo de trabajo en hoja trabajo para otros remunerado
// Trabajo remunerado (dependiente) 1
gen tipotrabajo_po1_2mes=.
replace tipotrabajo_po1_2mes=1  if reg_folio==30222
replace tipotrabajo_po1_2mes=1  if reg_folio==30303
replace tipotrabajo_po1_2mes=1  if reg_folio==30139
replace tipotrabajo_po1_2mes=1  if reg_folio==30136
replace tipotrabajo_po1_2mes=1  if reg_folio==30273
replace tipotrabajo_po1_2mes=1  if reg_folio==30278
replace tipotrabajo_po1_2mes=1  if reg_folio==40068
replace tipotrabajo_po1_2mes=1  if reg_folio==30155
replace tipotrabajo_po1_2mes=1  if reg_folio==40054
replace tipotrabajo_po1_2mes=1  if reg_folio==30039
replace tipotrabajo_po1_2mes=1  if reg_folio==20061
replace tipotrabajo_po1_2mes=1  if reg_folio==20081
replace tipotrabajo_po1_2mes=1  if reg_folio==20071
replace tipotrabajo_po1_2mes=1  if reg_folio==10087
replace tipotrabajo_po1_2mes=1  if reg_folio==20274
replace tipotrabajo_po1_2mes=1  if reg_folio==20032
replace tipotrabajo_po1_2mes=1  if reg_folio==10075
replace tipotrabajo_po1_2mes=1  if reg_folio==20200
replace tipotrabajo_po1_2mes=1  if reg_folio==10035
replace tipotrabajo_po1_2mes=1  if reg_folio==10213
replace tipotrabajo_po1_2mes=1  if reg_folio==50129
replace tipotrabajo_po1_2mes=1  if reg_folio==50231
replace tipotrabajo_po1_2mes=1  if reg_folio==50126
replace tipotrabajo_po1_2mes=1  if reg_folio==50281
replace tipotrabajo_po1_2mes=1  if reg_folio==20004
replace tipotrabajo_po1_2mes=1  if reg_folio==20095

recode tipotrabajo_po1_2mes (3=0)
label value tipotrabajo_po1_2mes tipotrabajo_po
tab tipotrabajo_po1_2mes if todas_olas==1, m

// Trabajo remunerado (dependiente) 2
gen tipotrabajo_po2_2mes=.
replace tipotrabajo_po2_2mes=1  if reg_folio==40054
replace tipotrabajo_po2_2mes=1  if reg_folio==20032
replace tipotrabajo_po2_2mes=1  if reg_folio==50129
replace tipotrabajo_po2_2mes=1  if reg_folio==20095
tab tipotrabajo_po2_2mes, m
recode tipotrabajo_po2_2mes (3=0)
label value tipotrabajo_po2_2mes tipotrabajo_po
tab tipotrabajo_po2_2mes if todas_olas==1, m

// Trabajo remunerado (dependiente) 3
gen tipotrabajo_po3_2mes=.
replace tipotrabajo_po3_2mes=1  if reg_folio==50129
tab tipotrabajo_po3_2mes, m
recode tipotrabajo_po3_2mes (3=0)
label value tipotrabajo_po3_2mes tipotrabajo_po
tab tipotrabajo_po3_2mes if todas_olas==1, m

// Número de trabajos por cuenta propia
gen anti_po1_2mes=~tipotrabajo_po1_2mes
gen anti_po2_2mes=~tipotrabajo_po2_2mes
gen anti_po3_2mes=~tipotrabajo_po3_2mes
sort reg_folio
egen num_trab_cp_2mes=rowtotal(tipotrabajo_cp1_2mes tipotrabajo_cp2_2mes tipotrabajo_cp3_2mes anti_po1_2mes anti_po2_2mes anti_po3_2mes)
tab num_trab_cp_2mes if todas_olas==1, m
drop anti_po1_2mes anti_po2_2mes anti_po3_2mes

// Número de trabajos remunerados
gen anti_cp1_2mes=~tipotrabajo_cp1_2mes
gen anti_cp2_2mes=~tipotrabajo_cp2_2mes
gen anti_cp3_2mes=~tipotrabajo_cp3_2mes
sort reg_folio
egen num_trab_remu_2mes=rowtotal(tipotrabajo_po1_2mes tipotrabajo_po2_2mes tipotrabajo_po3_2mes anti_cp1_2mes anti_cp2_2mes anti_cp3_2mes)
tab num_trab_remu_2mes if todas_olas==1, m
drop anti_cp1_2mes anti_cp2_2mes anti_cp3_2mes

// Trabajó para otros
gen trabajo_remunarado_2mes=1 if tipotrabajo_po1_2mes==1 | tipotrabajo_po2_2mes==1 | tipotrabajo_po3_2mes==1 | tipotrabajo_cp1_2mes==0 | tipotrabajo_cp2_2mes==0 | tipotrabajo_cp3_2mes==0
replace trabajo_remunarado_2mes=0 if missing(trabajo_remunarado_2mes)
replace trabajo_remunarado_2mes=. if num_trab_cp_2mes==0 & num_trab_remu_2mes==0
label value trabajo_remunarado_2mes tipotrabajo_po
tab trabajo_remunarado_2mes if todas_olas==1, m

// Trabajó por cuenta propia
gen trabajo_cuentapropia_2mes=1 if tipotrabajo_po1_2mes==0 | tipotrabajo_po2_2mes==0 | tipotrabajo_po3_2mes==0 | tipotrabajo_cp1_2mes==1 | tipotrabajo_cp2_2mes==1 | tipotrabajo_cp3_2mes==1
replace trabajo_cuentapropia_2mes=0 if missing(trabajo_cuentapropia_2mes)
replace trabajo_cuentapropia_2mes=. if num_trab_cp_2mes==0 & num_trab_remu_2mes==0
label value trabajo_cuentapropia_2mes tipo_trabajo_cp
tab trabajo_cuentapropia_2mes if todas_olas==1, m

tab1 trabajo_cuentapropia_2mes trabajo_remunarado_2mes if todas_olas==1

// Tipos de Trabajos realizados en Ola Dos Meses
label define tipos_trab 0"No trabajó" 1"Solo remunerado" 2"Solo cuenta propia" 3"Ambos"
gen tipos_trab_2mes=trabajo_cuentapropia_2mes*10 + trabajo_remunarado_2mes
tab tipos_trab_2mes // 1=Solo remunerado 10=Solo cuenta propia 11=Ambos
recode tipos_trab_2mes (10=2) (11=3)
replace tipos_trab_2mes=0 if missing(tipos_trab_2mes)
replace tipos_trab_2mes=. if reg_fecha_w2==""
label value tipos_trab_2mes tipos_trab
tab tipos_trab_2mes if todas_olas==1

// Número de trabajos
sort reg_folio
egen num_trab_2mes=rowtotal(num_trab_cp_2mes num_trab_remu_2mes)
tab num_trab_2mes if todas_olas==1

// SEIS MESES /////////////////////////////////////////

// Tipo de trabajo en hoja trabajo por cuenta propia
// Trabajo por cuenta propia 1
gen tipotrabajo_cp1_6mes=.
replace tipotrabajo_cp1_6mes=3  if reg_folio==40090
replace tipotrabajo_cp1_6mes=3  if reg_folio==40108
replace tipotrabajo_cp1_6mes=3  if reg_folio==50072
replace tipotrabajo_cp1_6mes=3  if reg_folio==30118
replace tipotrabajo_cp1_6mes=3  if reg_folio==50216
replace tipotrabajo_cp1_6mes=3  if reg_folio==20181
replace tipotrabajo_cp1_6mes=3  if reg_folio==10098
replace tipotrabajo_cp1_6mes=3  if reg_folio==40275
replace tipotrabajo_cp1_6mes=3  if reg_folio==10149
replace tipotrabajo_cp1_6mes=3  if reg_folio==20058
replace tipotrabajo_cp1_6mes=3  if reg_folio==40297
replace tipotrabajo_cp1_6mes=3  if reg_folio==10087
replace tipotrabajo_cp1_6mes=1  if reg_folio==40065
replace tipotrabajo_cp1_6mes=1  if reg_folio==30232
replace tipotrabajo_cp1_6mes=3  if reg_folio==20048
replace tipotrabajo_cp1_6mes=3  if reg_folio==10055
replace tipotrabajo_cp1_6mes=3  if reg_folio==40225
replace tipotrabajo_cp1_6mes=3  if reg_folio==20171
replace tipotrabajo_cp1_6mes=1  if reg_folio==40091
replace tipotrabajo_cp1_6mes=3  if reg_folio==30134
replace tipotrabajo_cp1_6mes=3  if reg_folio==10053
replace tipotrabajo_cp1_6mes=3  if reg_folio==20017
replace tipotrabajo_cp1_6mes=1  if reg_folio==20081
replace tipotrabajo_cp1_6mes=3  if reg_folio==10177
replace tipotrabajo_cp1_6mes=3  if reg_folio==20200
replace tipotrabajo_cp1_6mes=3  if reg_folio==40106
replace tipotrabajo_cp1_6mes=3  if reg_folio==20283
replace tipotrabajo_cp1_6mes=3  if reg_folio==50126
replace tipotrabajo_cp1_6mes=3  if reg_folio==30247
replace tipotrabajo_cp1_6mes=3  if reg_folio==10182
replace tipotrabajo_cp1_6mes=3  if reg_folio==20004
replace tipotrabajo_cp1_6mes=3  if reg_folio==20142
replace tipotrabajo_cp1_6mes=3  if reg_folio==40059
replace tipotrabajo_cp1_6mes=3  if reg_folio==30073
replace tipotrabajo_cp1_6mes=3  if reg_folio==20286
replace tipotrabajo_cp1_6mes=3  if reg_folio==40122
replace tipotrabajo_cp1_6mes=3  if reg_folio==10172
replace tipotrabajo_cp1_6mes=3  if reg_folio==10099
replace tipotrabajo_cp1_6mes=3  if reg_folio==50086
replace tipotrabajo_cp1_6mes=3  if reg_folio==40006
replace tipotrabajo_cp1_6mes=3  if reg_folio==50190
replace tipotrabajo_cp1_6mes=3  if reg_folio==50271
replace tipotrabajo_cp1_6mes=1  if reg_folio==10044
replace tipotrabajo_cp1_6mes=1  if reg_folio==40066
replace tipotrabajo_cp1_6mes=3  if reg_folio==40030
replace tipotrabajo_cp1_6mes=3  if reg_folio==20041
replace tipotrabajo_cp1_6mes=3  if reg_folio==50063
replace tipotrabajo_cp1_6mes=3  if reg_folio==10052
replace tipotrabajo_cp1_6mes=3  if reg_folio==20147
replace tipotrabajo_cp1_6mes=3  if reg_folio==10250
replace tipotrabajo_cp1_6mes=3  if reg_folio==20112
replace tipotrabajo_cp1_6mes=3  if reg_folio==40268
replace tipotrabajo_cp1_6mes=1  if reg_folio==40184
replace tipotrabajo_cp1_6mes=3  if reg_folio==10223
replace tipotrabajo_cp1_6mes=3  if reg_folio==40107
replace tipotrabajo_cp1_6mes=3  if reg_folio==10228
replace tipotrabajo_cp1_6mes=3  if reg_folio==30088
replace tipotrabajo_cp1_6mes=3  if reg_folio==40045
replace tipotrabajo_cp1_6mes=1  if reg_folio==40026
replace tipotrabajo_cp1_6mes=3  if reg_folio==50037
replace tipotrabajo_cp1_6mes=3  if reg_folio==30227
replace tipotrabajo_cp1_6mes=3  if reg_folio==30136
replace tipotrabajo_cp1_6mes=3  if reg_folio==50124
replace tipotrabajo_cp1_6mes=3  if reg_folio==40046
replace tipotrabajo_cp1_6mes=3  if reg_folio==50226
replace tipotrabajo_cp1_6mes=1  if reg_folio==10192
replace tipotrabajo_cp1_6mes=1  if reg_folio==10119
replace tipotrabajo_cp1_6mes=3  if reg_folio==40152
replace tipotrabajo_cp1_6mes=3  if reg_folio==10213
replace tipotrabajo_cp1_6mes=3  if reg_folio==50076
replace tipotrabajo_cp1_6mes=3  if reg_folio==20263

recode tipotrabajo_cp1_6mes (3=1) (1=0)
label value tipotrabajo_cp1_6mes tipo_trabajo_cp
tab tipotrabajo_cp1_6mes if todas_olas==1, m

// Trabajo por cuenta propia 2 
gen tipotrabajo_cp2_6mes=.
replace tipotrabajo_cp2_6mes=1  if reg_folio==40108
replace tipotrabajo_cp2_6mes=3  if reg_folio==20058
replace tipotrabajo_cp2_6mes=3  if reg_folio==20004
replace tipotrabajo_cp2_6mes=3  if reg_folio==20142
replace tipotrabajo_cp2_6mes=3  if reg_folio==20286
replace tipotrabajo_cp2_6mes=3  if reg_folio==40006
replace tipotrabajo_cp2_6mes=1  if reg_folio==20041
replace tipotrabajo_cp2_6mes=3  if reg_folio==10223
replace tipotrabajo_cp2_6mes=3  if reg_folio==40107
replace tipotrabajo_cp2_6mes=3  if reg_folio==10228
replace tipotrabajo_cp2_6mes=1  if reg_folio==10119
recode tipotrabajo_cp2_6mes (3=1) (1=0)
label value tipotrabajo_cp2_6mes tipo_trabajo_cp
tab tipotrabajo_cp2_6mes, m

// Trabajo por cuenta propia 3
gen tipotrabajo_cp3_6mes=.
replace tipotrabajo_cp3_6mes=3  if reg_folio==10223
replace tipotrabajo_cp3_6mes=3  if reg_folio==10119
recode tipotrabajo_cp3_6mes (3=1) (1=0)
label value tipotrabajo_cp3_6mes tipo_trabajo_cp
tab tipotrabajo_cp3_6mes, m

// Tipo de trabajo en hoja trabajo para otros remunerado
// Trabajo remunerado (dependiente) 1
gen tipotrabajo_po1_6mes=.
replace tipotrabajo_po1_6mes=1  if reg_folio==30278
replace tipotrabajo_po1_6mes=1  if reg_folio==20181
replace tipotrabajo_po1_6mes=1  if reg_folio==20042
replace tipotrabajo_po1_6mes=1  if reg_folio==40054
replace tipotrabajo_po1_6mes=1  if reg_folio==40297
replace tipotrabajo_po1_6mes=1  if reg_folio==10087
replace tipotrabajo_po1_6mes=1  if reg_folio==30039
replace tipotrabajo_po1_6mes=3  if reg_folio==20048
replace tipotrabajo_po1_6mes=1  if reg_folio==20095
replace tipotrabajo_po1_6mes=1  if reg_folio==20171
replace tipotrabajo_po1_6mes=1  if reg_folio==40040
replace tipotrabajo_po1_6mes=1  if reg_folio==50133
replace tipotrabajo_po1_6mes=1  if reg_folio==40022
replace tipotrabajo_po1_6mes=1  if reg_folio==50231
replace tipotrabajo_po1_6mes=1  if reg_folio==50060
replace tipotrabajo_po1_6mes=1  if reg_folio==30178
replace tipotrabajo_po1_6mes=1  if reg_folio==20096
replace tipotrabajo_po1_6mes=1  if reg_folio==10007
replace tipotrabajo_po1_6mes=1  if reg_folio==20200
replace tipotrabajo_po1_6mes=1  if reg_folio==10077
replace tipotrabajo_po1_6mes=1  if reg_folio==30173
replace tipotrabajo_po1_6mes=1  if reg_folio==20274
replace tipotrabajo_po1_6mes=1  if reg_folio==50175
replace tipotrabajo_po1_6mes=1  if reg_folio==40068
replace tipotrabajo_po1_6mes=1  if reg_folio==40122
replace tipotrabajo_po1_6mes=1  if reg_folio==20032
replace tipotrabajo_po1_6mes=1  if reg_folio==30164
replace tipotrabajo_po1_6mes=1  if reg_folio==40019
replace tipotrabajo_po1_6mes=1  if reg_folio==20013
replace tipotrabajo_po1_6mes=1  if reg_folio==30155
replace tipotrabajo_po1_6mes=1  if reg_folio==50070
replace tipotrabajo_po1_6mes=1  if reg_folio==20061
replace tipotrabajo_po1_6mes=1  if reg_folio==10035
replace tipotrabajo_po1_6mes=1  if reg_folio==20071
replace tipotrabajo_po1_6mes=1  if reg_folio==10187
replace tipotrabajo_po1_6mes=1  if reg_folio==30088
replace tipotrabajo_po1_6mes=1  if reg_folio==50276
replace tipotrabajo_po1_6mes=1  if reg_folio==40010
replace tipotrabajo_po1_6mes=1  if reg_folio==30057
replace tipotrabajo_po1_6mes=1  if reg_folio==30136
replace tipotrabajo_po1_6mes=1  if reg_folio==50124
replace tipotrabajo_po1_6mes=1  if reg_folio==30069
replace tipotrabajo_po1_6mes=1  if reg_folio==20008
replace tipotrabajo_po1_6mes=1  if reg_folio==40152
replace tipotrabajo_po1_6mes=1  if reg_folio==10213
tab tipotrabajo_po1_6mes, m

recode tipotrabajo_po1_6mes (3=0)
label value tipotrabajo_po1_6mes tipotrabajo_po
tab tipotrabajo_po1_6mes, m

// Trabajo remunerado (dependiente) 2
gen tipotrabajo_po2_6mes=.
replace tipotrabajo_po2_6mes=1  if reg_folio==20042
replace tipotrabajo_po2_6mes=1  if reg_folio==30039
replace tipotrabajo_po2_6mes=1  if reg_folio==20048
replace tipotrabajo_po2_6mes=3  if reg_folio==20171
replace tipotrabajo_po2_6mes=1  if reg_folio==20032
replace tipotrabajo_po2_6mes=1  if reg_folio==20013
replace tipotrabajo_po2_6mes=1  if reg_folio==20061
replace tipotrabajo_po2_6mes=1  if reg_folio==20071
tab tipotrabajo_po2_6mes, m
recode tipotrabajo_po2_6mes (3=0)
label value tipotrabajo_po2_6mes tipotrabajo_po
tab tipotrabajo_po2_6mes, m

// Trabajo remunerado (dependiente) 3
gen tipotrabajo_po3_6mes=.
replace tipotrabajo_po3_6mes=1  if reg_folio==20171
replace tipotrabajo_po3_6mes=1  if reg_folio==20061
tab tipotrabajo_po3_6mes, m
recode tipotrabajo_po3_6mes (3=0)
label value tipotrabajo_po3_6mes tipotrabajo_po
tab tipotrabajo_po3_6mes, m

// Número de trabajos por cuenta propia
gen anti_po1_6mes=~tipotrabajo_po1_6mes
gen anti_po2_6mes=~tipotrabajo_po2_6mes
gen anti_po3_6mes=~tipotrabajo_po3_6mes
sort reg_folio
egen num_trab_cp_6mes=rowtotal(tipotrabajo_cp1_6mes tipotrabajo_cp2_6mes tipotrabajo_cp3_6mes anti_po1_6mes anti_po2_6mes anti_po3_6mes)
tab num_trab_cp_6mes if todas_olas==1, m
drop anti_po1_6mes anti_po2_6mes anti_po3_6mes

// Número de trabajos por cuenta propia
gen anti_cp1_6mes=~tipotrabajo_cp1_6mes
gen anti_cp2_6mes=~tipotrabajo_cp2_6mes
gen anti_cp3_6mes=~tipotrabajo_cp3_6mes
sort reg_folio
egen num_trab_remu_6mes=rowtotal(tipotrabajo_po1_6mes tipotrabajo_po2_6mes tipotrabajo_po3_6mes anti_cp1_6mes anti_cp2_6mes anti_cp3_6mes)
tab num_trab_remu_6mes if todas_olas==1, m
drop anti_cp1_6mes anti_cp2_6mes anti_cp3_6mes

// Trabajó para otros
gen trabajo_remunarado_6mes=1 if tipotrabajo_po1_6mes==1 | tipotrabajo_po2_6mes==1 | tipotrabajo_po3_6mes==1 | tipotrabajo_cp1_6mes==0 | tipotrabajo_cp2_6mes==0 | tipotrabajo_cp3_6mes==0
replace trabajo_remunarado_6mes=0 if missing(trabajo_remunarado_6mes)
replace trabajo_remunarado_6mes=. if num_trab_cp_6mes==0 & num_trab_remu_6mes==0
label value trabajo_remunarado_6mes tipotrabajo_po
tab trabajo_remunarado_6mes if todas_olas==1, m

// Trabajó por cuenta propia
gen trabajo_cuentapropia_6mes=1 if tipotrabajo_po1_6mes==0 | tipotrabajo_po2_6mes==0 | tipotrabajo_po3_6mes==0 | tipotrabajo_cp1_6mes==1 | tipotrabajo_cp2_6mes==1 | tipotrabajo_cp3_6mes==1
replace trabajo_cuentapropia_6mes=0 if missing(trabajo_cuentapropia_6mes)
replace trabajo_cuentapropia_6mes=. if num_trab_cp_6mes==0 & num_trab_remu_6mes==0
label value trabajo_cuentapropia_6mes tipo_trabajo_cp
tab trabajo_cuentapropia_6mes if todas_olas==1, m

tab1 trabajo_cuentapropia_6mes trabajo_remunarado_6mes if todas_olas==1

// Tipos de Trabajos realizados en Ola Seis Meses
gen tipos_trab_6mes=trabajo_cuentapropia_6mes*10 + trabajo_remunarado_6mes
tab tipos_trab_6mes // 1=Solo remunerado 10=Solo cuenta propia 11=Ambos
recode tipos_trab_6mes (10=2) (11=3)
replace tipos_trab_6mes=0 if missing(tipos_trab_6mes)
replace tipos_trab_6mes=. if reg_fecha_w3==""
label value tipos_trab_6mes tipos_trab
tab tipos_trab_6mes if todas_olas==1

// Número de trabajos
sort reg_folio
egen num_trab_6mes=rowtotal(num_trab_cp_6mes num_trab_remu_6mes)
tab num_trab_6mes if todas_olas==1

// DOCE MESES /////////////////////////////////////////
// Tipo de trabajo en hoja trabajo por cuenta propia
// Trabajo por cuentra propia 1
gen tipotrabajo_cp1_12mes=.
replace tipotrabajo_cp1_12mes=1  if reg_folio==30232
replace tipotrabajo_cp1_12mes=1  if reg_folio==20283
replace tipotrabajo_cp1_12mes=3  if reg_folio==20041
replace tipotrabajo_cp1_12mes=3  if reg_folio==30118
replace tipotrabajo_cp1_12mes=1  if reg_folio==30183
replace tipotrabajo_cp1_12mes=3  if reg_folio==30136
replace tipotrabajo_cp1_12mes=3  if reg_folio==30051
replace tipotrabajo_cp1_12mes=3  if reg_folio==30073
replace tipotrabajo_cp1_12mes=3  if reg_folio==30173
replace tipotrabajo_cp1_12mes=3  if reg_folio==30188
replace tipotrabajo_cp1_12mes=3  if reg_folio==40152
replace tipotrabajo_cp1_12mes=1  if reg_folio==40122
replace tipotrabajo_cp1_12mes=3  if reg_folio==30273
replace tipotrabajo_cp1_12mes=3  if reg_folio==30069
replace tipotrabajo_cp1_12mes=3  if reg_folio==30110
replace tipotrabajo_cp1_12mes=3  if reg_folio==30088
replace tipotrabajo_cp1_12mes=3  if reg_folio==40090
replace tipotrabajo_cp1_12mes=3  if reg_folio==40297
replace tipotrabajo_cp1_12mes=3  if reg_folio==40107
replace tipotrabajo_cp1_12mes=1  if reg_folio==40030
replace tipotrabajo_cp1_12mes=3  if reg_folio==40225
replace tipotrabajo_cp1_12mes=1  if reg_folio==40275
replace tipotrabajo_cp1_12mes=3  if reg_folio==40065
replace tipotrabajo_cp1_12mes=1  if reg_folio==40184
replace tipotrabajo_cp1_12mes=3  if reg_folio==40059
replace tipotrabajo_cp1_12mes=1  if reg_folio==40066
replace tipotrabajo_cp1_12mes=3  if reg_folio==40108
replace tipotrabajo_cp1_12mes=3  if reg_folio==40106
replace tipotrabajo_cp1_12mes=3  if reg_folio==30227
replace tipotrabajo_cp1_12mes=3  if reg_folio==10074
replace tipotrabajo_cp1_12mes=3  if reg_folio==30247
replace tipotrabajo_cp1_12mes=3  if reg_folio==20286
replace tipotrabajo_cp1_12mes=3  if reg_folio==10288
replace tipotrabajo_cp1_12mes=3  if reg_folio==10087
replace tipotrabajo_cp1_12mes=3  if reg_folio==20263
replace tipotrabajo_cp1_12mes=3  if reg_folio==10177
replace tipotrabajo_cp1_12mes=3  if reg_folio==10250
replace tipotrabajo_cp1_12mes=3  if reg_folio==10228
replace tipotrabajo_cp1_12mes=3  if reg_folio==10023
replace tipotrabajo_cp1_12mes=3  if reg_folio==20048
replace tipotrabajo_cp1_12mes=3  if reg_folio==20198
replace tipotrabajo_cp1_12mes=3  if reg_folio==20017
replace tipotrabajo_cp1_12mes=3  if reg_folio==20112
replace tipotrabajo_cp1_12mes=3  if reg_folio==20058
replace tipotrabajo_cp1_12mes=3  if reg_folio==20142
replace tipotrabajo_cp1_12mes=3  if reg_folio==10291
replace tipotrabajo_cp1_12mes=3  if reg_folio==20147
replace tipotrabajo_cp1_12mes=3  if reg_folio==10119
replace tipotrabajo_cp1_12mes=3  if reg_folio==10102
replace tipotrabajo_cp1_12mes=3  if reg_folio==10099
replace tipotrabajo_cp1_12mes=3  if reg_folio==20200
replace tipotrabajo_cp1_12mes=3  if reg_folio==10077
replace tipotrabajo_cp1_12mes=3  if reg_folio==10182
replace tipotrabajo_cp1_12mes=3  if reg_folio==10055
replace tipotrabajo_cp1_12mes=3  if reg_folio==10098
replace tipotrabajo_cp1_12mes=3  if reg_folio==50037
replace tipotrabajo_cp1_12mes=3  if reg_folio==50105
replace tipotrabajo_cp1_12mes=3  if reg_folio==50070
replace tipotrabajo_cp1_12mes=3  if reg_folio==50226
replace tipotrabajo_cp1_12mes=3  if reg_folio==50190
replace tipotrabajo_cp1_12mes=3  if reg_folio==50133
replace tipotrabajo_cp1_12mes=3  if reg_folio==50076
replace tipotrabajo_cp1_12mes=3  if reg_folio==50124
replace tipotrabajo_cp1_12mes=3  if reg_folio==50175
replace tipotrabajo_cp1_12mes=3  if reg_folio==50271
replace tipotrabajo_cp1_12mes=3  if reg_folio==50216
replace tipotrabajo_cp1_12mes=3  if reg_folio==50072
replace tipotrabajo_cp1_12mes=3  if reg_folio==20004

recode tipotrabajo_cp1_12mes (3=1) (1=0)
label value tipotrabajo_cp1_12mes tipo_trabajo_cp
tab tipotrabajo_cp1_12mes, m

// Trabajo por cuentra propia 2 
gen tipotrabajo_cp2_12mes=.
replace tipotrabajo_cp2_12mes=3  if reg_folio==20041
replace tipotrabajo_cp2_12mes=3  if reg_folio==30088
replace tipotrabajo_cp2_12mes=1  if reg_folio==40297
replace tipotrabajo_cp2_12mes=1  if reg_folio==40225
replace tipotrabajo_cp2_12mes=1  if reg_folio==40108
replace tipotrabajo_cp2_12mes=3  if reg_folio==30247
replace tipotrabajo_cp2_12mes=3  if reg_folio==10228
replace tipotrabajo_cp2_12mes=3  if reg_folio==10077
replace tipotrabajo_cp2_12mes=3  if reg_folio==50216
replace tipotrabajo_cp2_12mes=3  if reg_folio==20004
recode tipotrabajo_cp2_12mes (3=1) (1=0)
label value tipotrabajo_cp2_12mes tipo_trabajo_cp
tab tipotrabajo_cp2_12mes, m

// Trabajo por cuenta propia 3
gen tipotrabajo_cp3_12mes=.
replace tipotrabajo_cp3_12mes=3  if reg_folio==40225
replace tipotrabajo_cp3_12mes=3  if reg_folio==20004
recode tipotrabajo_cp3_12mes (3=1) (1=0)
label value tipotrabajo_cp3_12mes tipo_trabajo_cp
tab tipotrabajo_cp3_12mes, m

// Tipo de trabajo en hoja trabajo para otros remunerado
// Trabajo para otros (dependiente) 1
gen tipotrabajo_po1_12mes=.
replace tipotrabajo_po1_12mes=1  if reg_folio==30029
replace tipotrabajo_po1_12mes=1  if reg_folio==30264
replace tipotrabajo_po1_12mes=1  if reg_folio==30303
replace tipotrabajo_po1_12mes=1  if reg_folio==30300
replace tipotrabajo_po1_12mes=1  if reg_folio==30164
replace tipotrabajo_po1_12mes=1  if reg_folio==30136
replace tipotrabajo_po1_12mes=1  if reg_folio==30134
replace tipotrabajo_po1_12mes=1  if reg_folio==30057
replace tipotrabajo_po1_12mes=1  if reg_folio==40152
replace tipotrabajo_po1_12mes=1  if reg_folio==40122
replace tipotrabajo_po1_12mes=1  if reg_folio==30069
replace tipotrabajo_po1_12mes=1  if reg_folio==30278
replace tipotrabajo_po1_12mes=1  if reg_folio==10272
replace tipotrabajo_po1_12mes=1  if reg_folio==40113
replace tipotrabajo_po1_12mes=1  if reg_folio==40068
replace tipotrabajo_po1_12mes=1  if reg_folio==30155
replace tipotrabajo_po1_12mes=1  if reg_folio==40275
replace tipotrabajo_po1_12mes=1  if reg_folio==40040
replace tipotrabajo_po1_12mes=1  if reg_folio==40166
replace tipotrabajo_po1_12mes=1  if reg_folio==40010
replace tipotrabajo_po1_12mes=1  if reg_folio==40045
replace tipotrabajo_po1_12mes=1  if reg_folio==40026
replace tipotrabajo_po1_12mes=1  if reg_folio==40022
replace tipotrabajo_po1_12mes=1  if reg_folio==40019
replace tipotrabajo_po1_12mes=1  if reg_folio==30039
replace tipotrabajo_po1_12mes=1  if reg_folio==20013
replace tipotrabajo_po1_12mes=1  if reg_folio==20061
replace tipotrabajo_po1_12mes=1  if reg_folio==20071
replace tipotrabajo_po1_12mes=1  if reg_folio==10172
replace tipotrabajo_po1_12mes=1  if reg_folio==20171
replace tipotrabajo_po1_12mes=1  if reg_folio==10223
replace tipotrabajo_po1_12mes=1  if reg_folio==10087
replace tipotrabajo_po1_12mes=1  if reg_folio==10187
replace tipotrabajo_po1_12mes=1  if reg_folio==20048
replace tipotrabajo_po1_12mes=1  if reg_folio==20274
replace tipotrabajo_po1_12mes=1  if reg_folio==20096
replace tipotrabajo_po1_12mes=1  if reg_folio==20032
replace tipotrabajo_po1_12mes=1  if reg_folio==20195
replace tipotrabajo_po1_12mes=1  if reg_folio==10119
replace tipotrabajo_po1_12mes=1  if reg_folio==20200
replace tipotrabajo_po1_12mes=1  if reg_folio==10035
replace tipotrabajo_po1_12mes=1  if reg_folio==10213
replace tipotrabajo_po1_12mes=1  if reg_folio==50063
replace tipotrabajo_po1_12mes=1  if reg_folio==50070
replace tipotrabajo_po1_12mes=1  if reg_folio==50276
replace tipotrabajo_po1_12mes=1  if reg_folio==50129
replace tipotrabajo_po1_12mes=1  if reg_folio==50133
replace tipotrabajo_po1_12mes=1  if reg_folio==50076
replace tipotrabajo_po1_12mes=1  if reg_folio==50231
replace tipotrabajo_po1_12mes=1  if reg_folio==50214
replace tipotrabajo_po1_12mes=1  if reg_folio==50060
replace tipotrabajo_po1_12mes=1  if reg_folio==50126
replace tipotrabajo_po1_12mes=1  if reg_folio==50281
replace tipotrabajo_po1_12mes=1  if reg_folio==20042
replace tipotrabajo_po1_12mes=1  if reg_folio==20095
tab tipotrabajo_po1_12mes, m

recode tipotrabajo_po1_12mes (3=0)
label value tipotrabajo_po1_12mes tipotrabajo_po
tab tipotrabajo_po1_12mes, m

// Trabajo para otros (dependiente) 2
gen tipotrabajo_po2_12mes=.
replace tipotrabajo_po2_12mes=1  if reg_folio==30029
replace tipotrabajo_po2_12mes=1  if reg_folio==30164
replace tipotrabajo_po2_12mes=1  if reg_folio==30136
replace tipotrabajo_po2_12mes=1  if reg_folio==40068
replace tipotrabajo_po2_12mes=1  if reg_folio==40045
replace tipotrabajo_po2_12mes=1  if reg_folio==30039
replace tipotrabajo_po2_12mes=1  if reg_folio==20013
replace tipotrabajo_po2_12mes=1  if reg_folio==20061
replace tipotrabajo_po2_12mes=1  if reg_folio==20171
replace tipotrabajo_po2_12mes=1  if reg_folio==20274
replace tipotrabajo_po2_12mes=1  if reg_folio==20032
replace tipotrabajo_po2_12mes=1  if reg_folio==20200
replace tipotrabajo_po2_12mes=1  if reg_folio==50276
replace tipotrabajo_po2_12mes=1  if reg_folio==50133
replace tipotrabajo_po2_12mes=1  if reg_folio==50060
tab tipotrabajo_po2_12mes, m
recode tipotrabajo_po2_12mes (3=0)
label value tipotrabajo_po2_12mes tipotrabajo_po
tab tipotrabajo_po2_12mes, m

// Trabajo para otros (dependiente) 3
gen tipotrabajo_po3_12mes=.
replace tipotrabajo_po3_12mes=1  if reg_folio==20032
replace tipotrabajo_po3_12mes=1  if reg_folio==50060
tab tipotrabajo_po3_12mes, m
recode tipotrabajo_po3_12mes (3=0)
label value tipotrabajo_po3_12mes tipotrabajo_po
tab tipotrabajo_po3_12mes, m

// Trabajo para otros (dependiente) 4
gen tipotrabajo_po4_12mes=.
replace tipotrabajo_po4_12mes=1  if reg_folio==50060
tab tipotrabajo_po4_12mes, m
recode tipotrabajo_po4_12mes (3=0)
label value tipotrabajo_po4_12mes tipotrabajo_po
tab tipotrabajo_po4_12mes, m

// Número de trabajos por cuenta propia
gen anti_po1_12mes=~tipotrabajo_po1_12mes
gen anti_po2_12mes=~tipotrabajo_po2_12mes
gen anti_po3_12mes=~tipotrabajo_po3_12mes
gen anti_po4_12mes=~tipotrabajo_po4_12mes
sort reg_folio
egen num_trab_cp_12mes=rowtotal(tipotrabajo_cp1_12mes tipotrabajo_cp2_12mes tipotrabajo_cp3_12mes anti_po1_12mes anti_po2_12mes anti_po3_12mes anti_po4_12mes)
tab num_trab_cp_12mes, m
drop anti_po1_12mes anti_po2_12mes anti_po3_12mes anti_po4_12mes

// Número de trabajos por cuenta propia
gen anti_cp1_12mes=~tipotrabajo_cp1_12mes
gen anti_cp2_12mes=~tipotrabajo_cp2_12mes
gen anti_cp3_12mes=~tipotrabajo_cp3_12mes
sort reg_folio
egen num_trab_remu_12mes=rowtotal(tipotrabajo_po1_12mes tipotrabajo_po2_12mes tipotrabajo_po3_12mes anti_cp1_12mes anti_cp2_12mes anti_cp3_12mes)
tab num_trab_remu_12mes, m
drop anti_cp1_12mes anti_cp2_12mes anti_cp3_12mes

// Trabajó para otros
gen trabajo_remunarado_12mes=1 if tipotrabajo_po1_12mes==1 | tipotrabajo_po2_12mes==1 | tipotrabajo_po3_12mes==1 | tipotrabajo_po4_12mes==1 | tipotrabajo_cp1_12mes==0 | tipotrabajo_cp2_12mes==0 | tipotrabajo_cp3_12mes==0
replace trabajo_remunarado_12mes=0 if missing(trabajo_remunarado_12mes)
replace trabajo_remunarado_12mes=. if num_trab_cp_12mes==0 & num_trab_remu_12mes==0
label value trabajo_remunarado_12mes tipotrabajo_po
tab trabajo_remunarado_12mes if todas_olas==1, m

// Trabajó por cuenta propia
gen trabajo_cuentapropia_12mes=1 if tipotrabajo_po1_12mes==0 | tipotrabajo_po2_12mes==0 | tipotrabajo_po3_12mes==0 | tipotrabajo_po4_12mes==1 | tipotrabajo_cp1_12mes==1 | tipotrabajo_cp2_12mes==1 | tipotrabajo_cp3_12mes==1
replace trabajo_cuentapropia_12mes=0 if missing(trabajo_cuentapropia_12mes)
replace trabajo_cuentapropia_12mes=. if num_trab_cp_12mes==0 & num_trab_remu_12mes==0
label value trabajo_cuentapropia_12mes tipo_trabajo_cp
tab trabajo_cuentapropia_12mes if todas_olas==1, m

tab1 trabajo_cuentapropia_12mes trabajo_remunarado_12mes if todas_olas==1

// Tipos de Trabajos en Ola Doce Meses
gen tipos_trab_12mes=trabajo_cuentapropia_12mes*10 + trabajo_remunarado_12mes
tab tipos_trab_12mes // 1=Solo remunerado 10=Solo cuenta propia 11=Ambos
recode tipos_trab_12mes (10=2) (11=3)
replace tipos_trab_12mes=0 if missing(tipos_trab_12mes)
replace tipos_trab_12mes=. if reg_fecha_w4==""
label value tipos_trab_12mes tipos_trab
tab tipos_trab_12mes if todas_olas==1

// Número de trabajos
sort reg_folio
egen num_trab_12mes=rowtotal(num_trab_cp_12mes num_trab_remu_12mes)
tab num_trab_12mes if todas_olas==1

// Trabajó en cada ola 
gen trab_1sem=trabajos_1sem 
recode trab_1sem (1/3=1) (0=0)

gen trab_2mes=tipos_trab_2mes 
recode trab_2mes (1/3=1) (0=0)

gen trab_6mes=tipos_trab_6mes 
recode trab_6mes (1/3=1) (0=0)

gen trab_12mes=tipos_trab_12mes 
recode trab_12mes (1/3=1) (0=0)

tab1 trab_2mes trab_6mes trab_12mes if todas_olas==1

// Trabajó en al menos una ola
gen trabajo=1 if trab_2mes==1 | trab_6mes==1 | trab_12mes==1
replace trabajo=0 if missing(trabajo)
replace trabajo=. if todas_olas==0
tab busco_trab trabajo if todas_olas==1, row chi2
 
// Categorías de trabajo
// Para otros versus cuenta propia
* 0=No trabajo
* 1=Para otros
* 2=Cuenta propia

gen tipos_trab_dependencia_2mes=tipos_trab_2mes
tab1 tipos_trab_dependencia_2mes tipos_trab_2mes
recode tipos_trab_dependencia_2mes (0=0) (1 3=1) (2=2)

gen tipos_trab_dependencia_6mes=tipos_trab_6mes
tab1 tipos_trab_dependencia_6mes tipos_trab_6mes
recode tipos_trab_dependencia_6mes (0=0) (1 3=1) (2=2)

gen tipos_trab_dependencia_12mes=tipos_trab_12mes
tab1 tipos_trab_dependencia_12mes tipos_trab_12mes
recode tipos_trab_dependencia_12mes (0=0) (1 3=1) (2=2)

// FORMALIDAD - Tiene contrato, cotización o boleta servicios en al menos un trabajo declarado por ola
// TRABAJOS REMUNERADOS

// Tiene contrato o boleta de honorarios
tab1 eaf_trcp1_1_1_w2 eaf_trcp1_1_1_w3 eaf_trcp1_1_1_w4 eaf_trcp1_2_1_w2 eaf_trcp1_2_1_w3 eaf_trcp1_2_1_w4 eaf_trcp1_3_1_w2 eaf_trcp1_3_1_w3 eaf_trcp1_3_1_w4 eaf_trcp1_4_1_w2 eaf_trcp1_4_1_w3 eaf_trcp1_4_1_w4

gen contrato_2mes=1 if eaf_trcp1_1_1_w2==1 | eaf_trcp1_2_1_w2==1 | eaf_trcp1_3_1_w2==1 | eaf_trcp1_4_1_w2==1
replace contrato_2mes=0 if missing(contrato_2mes)
codebook eaf_trcp1_1_1_w2 eaf_trcp1_2_1_w2 eaf_trcp1_3_1_w2 eaf_trcp1_4_1_w2
replace contrato_2mes=-7 if eaf_trcp1_1_1_w2==-7 & eaf_trcp1_2_1_w2==-7 & eaf_trcp1_3_1_w2==-7 & eaf_trcp1_4_1_w2==-7
replace contrato_2mes=-1 if reg_fecha_w2==""
tab contrato_2mes

gen contrato_6mes=1 if eaf_trcp1_1_1_w3==1 | eaf_trcp1_2_1_w3==1 | eaf_trcp1_3_1_w3==1 | eaf_trcp1_4_1_w3==1
replace contrato_6mes=0 if missing(contrato_6mes)
codebook eaf_trcp1_1_1_w3 eaf_trcp1_2_1_w3 eaf_trcp1_3_1_w3 eaf_trcp1_4_1_w3
replace contrato_6mes=-7 if eaf_trcp1_1_1_w3==-7 & eaf_trcp1_2_1_w3==-7 & eaf_trcp1_3_1_w3==-7 & eaf_trcp1_4_1_w3==-7
replace contrato_6mes=-1 if reg_fecha_w3==""
tab contrato_6mes

gen contrato_12mes=1 if eaf_trcp1_1_1_w4==1 | eaf_trcp1_2_1_w4==1 | eaf_trcp1_3_1_w4==1 | eaf_trcp1_4_1_w4==1
replace contrato_12mes=0 if missing(contrato_12mes)
codebook eaf_trcp1_1_1_w4 eaf_trcp1_2_1_w4 eaf_trcp1_3_1_w4 eaf_trcp1_4_1_w4
replace contrato_12mes=-7 if eaf_trcp1_1_1_w4==-7 & eaf_trcp1_2_1_w4==-7 & eaf_trcp1_3_1_w4==-7 & eaf_trcp1_4_1_w4==-7
replace contrato_12mes=-1 if reg_fecha_w4==""
tab contrato_12mes

// Tiene cotizaciones
gen cotiza_2mes=1 if eaf_trcp1_1_2_w2==1 | eaf_trcp1_2_2_w2==1 | eaf_trcp1_3_2_w2==1 | eaf_trcp1_4_2_w2==1
replace cotiza_2mes=0 if missing(cotiza_2mes)
codebook eaf_trcp1_1_2_w2 eaf_trcp1_2_2_w2 eaf_trcp1_3_2_w2 eaf_trcp1_4_2_w2
replace cotiza_2mes=-7 if eaf_trcp1_1_2_w2==-7 & eaf_trcp1_2_2_w2==-7 & eaf_trcp1_3_2_w2==-7 & eaf_trcp1_4_2_w2==-7
replace cotiza_2mes=-8 if eaf_trcp1_1_2_w2==-8 & eaf_trcp1_2_2_w2==-8 & eaf_trcp1_3_2_w2==-8 & eaf_trcp1_4_2_w2==-8
replace cotiza_2mes=-1 if reg_fecha_w2==""
tab cotiza_2mes

gen cotiza_6mes=1 if eaf_trcp1_1_2_w3==1 | eaf_trcp1_2_2_w3==1 | eaf_trcp1_3_2_w3==1 | eaf_trcp1_4_2_w3==1
replace cotiza_6mes=0 if missing(cotiza_6mes)
codebook eaf_trcp1_1_2_w3 eaf_trcp1_2_2_w3 eaf_trcp1_3_2_w3 eaf_trcp1_4_2_w3
replace cotiza_6mes=-7 if eaf_trcp1_1_2_w3==-7 & eaf_trcp1_2_2_w3==-7 & eaf_trcp1_3_2_w3==-7 & eaf_trcp1_4_2_w3==-7
replace cotiza_2mes=-8 if eaf_trcp1_1_2_w3==-8 & eaf_trcp1_2_2_w3==-8 & eaf_trcp1_3_2_w3==-8 & eaf_trcp1_4_2_w3==-8
replace cotiza_2mes=-9 if eaf_trcp1_1_2_w3==-9 & eaf_trcp1_2_2_w3==-9 & eaf_trcp1_3_2_w3==-9 & eaf_trcp1_4_2_w3==-9
replace cotiza_6mes=-1 if reg_fecha_w3==""
tab cotiza_6mes

gen cotiza_12mes=1 if eaf_trcp1_1_2_w4==1 | eaf_trcp1_2_2_w4==1 | eaf_trcp1_3_2_w4==1 | eaf_trcp1_4_2_w4==1
replace cotiza_12mes=0 if missing(cotiza_12mes)
codebook eaf_trcp1_1_2_w4 eaf_trcp1_2_2_w4 eaf_trcp1_3_2_w4 eaf_trcp1_4_2_w4
replace cotiza_12mes=-7 if eaf_trcp1_1_2_w4==-7 & eaf_trcp1_2_2_w4==-7 & eaf_trcp1_3_2_w4==-7 & eaf_trcp1_4_2_w4==-7
replace cotiza_12mes=-8 if eaf_trcp1_1_2_w4==-8 & eaf_trcp1_2_2_w4==-8 & eaf_trcp1_3_2_w4==-8 & eaf_trcp1_4_2_w4==-8
replace cotiza_12mes=-9 if eaf_trcp1_1_2_w4==-9 & eaf_trcp1_2_2_w4==-9 & eaf_trcp1_3_2_w4==-9 & eaf_trcp1_4_2_w4==-9
replace cotiza_12mes=-1 if reg_fecha_w4==""
tab cotiza_12mes

// TRABAJOS POR CUENTA PROPIA ****************************************
// Tiene boletas por venta o servicios
codebook eaf_trcp2_1_1_w2 eaf_trcp2_1_1_w3 eaf_trcp2_1_1_w4 
codebook eaf_trcp2_2_1_w2 eaf_trcp2_2_1_w3 eaf_trcp2_2_1_w4 
codebook eaf_trcp2_3_1_w2 eaf_trcp2_3_1_w3 eaf_trcp2_3_1_w4

gen boleta_2mes=1 if eaf_trcp2_1_1_w2==1 | eaf_trcp2_2_1_w2==1 | eaf_trcp2_3_1_w2==1 
replace boleta_2mes=0 if missing(boleta_2mes)
replace boleta_2mes=-7 if eaf_trcp2_1_1_w2==-7 & eaf_trcp2_2_1_w2==-7 & eaf_trcp2_3_1_w2==-7 
replace boleta_2mes=-9 if eaf_trcp2_1_1_w2==-9 & eaf_trcp2_2_1_w2==-9 & eaf_trcp2_3_1_w2==-9
replace boleta_2mes=-1 if reg_fecha_w2==""
tab boleta_2mes

gen boleta_6mes=1 if eaf_trcp2_1_1_w3==1 | eaf_trcp2_2_1_w3==1 | eaf_trcp2_3_1_w3==1 
replace boleta_6mes=0 if missing(boleta_6mes)
replace boleta_6mes=-7 if eaf_trcp2_1_1_w3==-7 & eaf_trcp2_2_1_w3==-7 & eaf_trcp2_3_1_w3==-7 
replace boleta_6mes=-9 if eaf_trcp2_1_1_w3==-9 & eaf_trcp2_2_1_w3==-9 & eaf_trcp2_3_1_w3==-9
replace boleta_6mes=-1 if reg_fecha_w3==""
tab boleta_6mes

gen boleta_12mes=1 if eaf_trcp2_1_1_w4==1 | eaf_trcp2_2_1_w4==1 | eaf_trcp2_3_1_w4==1 
replace boleta_12mes=0 if missing(boleta_12mes)
replace boleta_12mes=-7 if eaf_trcp2_1_1_w4==-7 & eaf_trcp2_2_1_w4==-7 & eaf_trcp2_3_1_w4==-7 
replace boleta_12mes=-9 if eaf_trcp2_1_1_w4==-9 & eaf_trcp2_2_1_w4==-9 & eaf_trcp2_3_1_w4==-9
replace boleta_12mes=-1 if reg_fecha_w4==""
tab boleta_12mes

// FORMAL por ola
gen formal_2mes=1 if contrato_2mes==1 | cotiza_2mes==1 | boleta_2mes==1
replace formal_2mes=0 if missing(formal_2mes)
replace formal_2mes=-7 if contrato_2mes==-7 & cotiza_2mes==-7 & boleta_2mes==-7
replace formal_2mes=-8 if contrato_2mes==-8 & cotiza_2mes==-8 & boleta_2mes==-8
replace formal_2mes=-9 if contrato_2mes==-9 & cotiza_2mes==-9 & boleta_2mes==-9
replace formal_2mes=-1 if reg_fecha_w2==""
tab formal_2mes

gen formal_6mes=1 if contrato_6mes==1 | cotiza_6mes==1 | boleta_6mes==1
replace formal_6mes=0 if missing(formal_6mes)
replace formal_6mes=-7 if contrato_6mes==-7 & cotiza_6mes==-7 & boleta_6mes==-7
replace formal_6mes=-8 if contrato_6mes==-8 & cotiza_6mes==-8 & boleta_6mes==-8
replace formal_6mes=-9 if contrato_6mes==-9 & cotiza_6mes==-9 & boleta_6mes==-9
replace formal_6mes=-1 if reg_fecha_w3==""
tab formal_6mes

gen formal_12mes=1 if contrato_12mes==1 | cotiza_12mes==1 | boleta_12mes==1
replace formal_12mes=0 if missing(formal_12mes)
replace formal_12mes=-7 if contrato_12mes==-7 & cotiza_12mes==-7 & boleta_12mes==-7
replace formal_12mes=-8 if contrato_12mes==-8 & cotiza_12mes==-8 & boleta_12mes==-8
replace formal_12mes=-9 if contrato_12mes==-9 & cotiza_12mes==-9 & boleta_12mes==-9
replace formal_12mes=-1 if reg_fecha_w4==""
tab formal_12mes

// Categorías de trabajo según ola finales
tab1 tipos_trab_dependencia_2mes formal_2mes 
recode tipos_trab_dependencia_2mes tipos_trab_dependencia_6mes tipos_trab_dependencia_12mes (0=1) (1=2) (2=3) // 1=No trab 2=remunerado 3=cta propia
replace tipos_trab_dependencia_2mes=-1 if reg_fecha_w2==""
replace tipos_trab_dependencia_6mes=-1 if reg_fecha_w3==""
replace tipos_trab_dependencia_12mes=-1 if reg_fecha_w4==""
tab1 tipos_trab_dependencia_2mes tipos_trab_dependencia_6mes tipos_trab_dependencia_12mes

recode formal_2mes formal_6mes formal_12mes (-7=7) 
tab1 formal_2mes formal_6mes formal_12mes

* 2 meses
* 1=no trab / 2=remunerado / 3=cta propia / 0=informal / 1=formal
gen tipo_trab_final_2mes=tipos_trab_dependencia_2mes*10+formal_2mes
gen trab_final_2mes=tipo_trab_final_2mes
tab trab_final_2mes
recode trab_final_2mes (-11=-1) (11 17=0) (20=1) (21=2) (30=3) (31=4)
label define tipo_trab_final 0"No trabajó" 1"Remunerado informal" 2"Remunerado formal" 3"Cuenta propia informal" 4"Cuentra propia formal"
label value trab_final_2mes tipo_trab_final
replace trab_final_2mes=-1 if reg_fecha_w2==""
tab trab_final_2mes

* 6 meses
tab1 tipos_trab_dependencia_6mes formal_6mes
gen tipo_trab_final_6mes=tipos_trab_dependencia_6mes*10+formal_6mes
tab tipo_trab_final_6mes
gen trab_final_6mes=tipo_trab_final_6mes
recode trab_final_6mes (-11=-1) (11 17=0) (20=1) (21=2) (30=3) (31=4)
label value trab_final_6mes tipo_trab_final
replace trab_final_6mes=-1 if reg_fecha_w3==""
tab trab_final_6mes

* 12 meses
tab1 tipos_trab_dependencia_12mes formal_12mes
gen tipo_trab_final_12mes=tipos_trab_dependencia_12mes*10+formal_12mes
tab tipo_trab_final_12mes
gen trab_final_12mes=tipo_trab_final_12mes
recode trab_final_12mes (-11=-1) (17=0) (20=1) (21=2) (30=3) (31=4)
label value trab_final_12mes tipo_trab_final
replace trab_final_12mes=-1 if reg_fecha_w4==""
tab trab_final_12mes

tab trab_final_12mes if todas_olas==1

tab1 tipo_trab_final_2mes tipo_trab_final_6mes tipo_trab_final_12mes
tab1 trab_final_2mes trab_final_6mes trab_final_12mes if todas_olas==1

// 3. C ///////////////////////////////////////////////////////
// Satisfacción situación laboral
////////////////////////////////
tab1 spg_9_7_w2 spg_9_7_w3 spg_9_7_w4
replace spg_9_7_w2=-1 if reg_fecha_w2==""
replace spg_9_7_w3=-1 if reg_fecha_w3==""
replace spg_9_7_w4=-1 if reg_fecha_w4==""

tab1 spg_9_7_w2 spg_9_7_w3 spg_9_7_w4 if todas_olas==1

// 3.D ///////////////////////////////////////////////////////
// EN R - Empleos mas comunes

// 3.E ///////////////////////////////////////////////////////
// Estabilidad

tab trab_2mes trab_6mes if todas_olas==1, row chi2 
tab trab_6mes trab_12mes if todas_olas==1, row chi2 
tab tipos_trab_dependencia_2mes tipos_trab_dependencia_6mes if todas_olas==1, row chi2 
tab tipos_trab_dependencia_6mes tipos_trab_dependencia_12mes if todas_olas==1, row chi2 nofreq

// 3.F /////////////////////////////////////
// Trabajo remunerado
// Jornada dos meses
tab1 eaf_trcp1_1_3_w2 eaf_trcp1_2_3_w2 eaf_trcp1_3_3_w2 eaf_trcp1_4_3_w2
recode eaf_trcp1_1_3_w2 eaf_trcp1_2_3_w2 eaf_trcp1_3_3_w2 eaf_trcp1_4_3_w2 (-7=7) (-8=8) (-9=9)
sort reg_folio
egen max_jornada_rem_2mes=rowmin(eaf_trcp1_1_3_w2  eaf_trcp1_2_3_w2 eaf_trcp1_3_3_w2 eaf_trcp1_4_3_w2) // Rowmin porque mayor jornada tiene menor atributo
tab max_jornada_rem_2mes
replace max_jornada_rem_2mes=-1 if reg_fecha_w2==""

// Jornada 6 meses
tab1 eaf_trcp1_1_3_w3 eaf_trcp1_2_3_w3 eaf_trcp1_3_3_w3 eaf_trcp1_4_3_w3
recode eaf_trcp1_1_3_w3 eaf_trcp1_2_3_w3 eaf_trcp1_3_3_w3 eaf_trcp1_4_3_w3 (-7=7) (-8=8) (-9=9)
egen max_jornada_rem_6mes=rowmin(eaf_trcp1_1_3_w3 eaf_trcp1_2_3_w3 eaf_trcp1_3_3_w3 eaf_trcp1_4_3_w3) // Rowmin porque mayor jornada tiene menor atributo
tab max_jornada_rem_6mes
replace max_jornada_rem_6mes=-1 if reg_fecha_w3==""

// Jornada 12 meses
tab1 eaf_trcp1_1_3_w4 eaf_trcp1_2_3_w4 eaf_trcp1_3_3_w4 eaf_trcp1_4_3_w4
recode eaf_trcp1_1_3_w4 eaf_trcp1_2_3_w4 eaf_trcp1_3_3_w4 eaf_trcp1_4_3_w4 (-7=7) (-8=8) (-9=9)
egen max_jornada_rem_12mes=rowmin(eaf_trcp1_1_3_w4 eaf_trcp1_2_3_w4 eaf_trcp1_3_3_w4 eaf_trcp1_4_3_w4) // Rowmin porque mayor jornada tiene menor atributo
tab max_jornada_rem_12mes
replace max_jornada_rem_12mes=-1 if reg_fecha_w4==""

tab1 max_jornada_rem_2mes max_jornada_rem_6mes max_jornada_rem_12mes if todas_olas==1

// Trabajo cuenta propia
// Jornada dos meses
tab1 eaf_trcp2_1_2_w2 eaf_trcp2_2_2_w2 eaf_trcp2_3_2_w2 eaf_trcp2_4_2_w2
recode eaf_trcp2_1_2_w2 eaf_trcp2_2_2_w2 eaf_trcp2_3_2_w2 eaf_trcp2_4_2_w2 (-7=7) (-8=8) (-9=9)
sort reg_folio
egen max_jornada_cp_2mes=rowmin(eaf_trcp2_1_2_w2 eaf_trcp2_2_2_w2 eaf_trcp2_3_2_w2 eaf_trcp2_4_2_w2) // Rowmin porque mayor jornada tiene menor atributo
tab max_jornada_cp_2mes
replace max_jornada_cp_2mes=-1 if reg_fecha_w2==""

// Jornada 6 meses
tab1 eaf_trcp2_1_2_w3 eaf_trcp2_2_2_w3 eaf_trcp2_3_2_w3 eaf_trcp2_4_2_w3
recode eaf_trcp2_1_2_w3 eaf_trcp2_2_2_w3 eaf_trcp2_3_2_w3 eaf_trcp2_4_2_w3 (-7=7) (-8=8) (-9=9)
egen max_jornada_cp_6mes=rowmin(eaf_trcp2_1_2_w3 eaf_trcp2_2_2_w3 eaf_trcp2_3_2_w3 eaf_trcp2_4_2_w3) // Rowmin porque mayor jornada tiene menor atributo
tab max_jornada_cp_6mes
replace max_jornada_cp_6mes=-1 if reg_fecha_w3==""

// Jornada 12 meses
tab1 eaf_trcp2_1_2_w4 eaf_trcp2_2_2_w4 eaf_trcp2_3_2_w4 eaf_trcp2_4_2_w4
recode eaf_trcp2_1_2_w4 eaf_trcp2_2_2_w4 eaf_trcp2_3_2_w4 eaf_trcp2_4_2_w4 (-7=7) (-8=8) (-9=9)
egen max_jornada_cp_12mes=rowmin(eaf_trcp2_1_2_w4 eaf_trcp2_2_2_w4 eaf_trcp2_3_2_w4 eaf_trcp2_4_2_w4) // Rowmin porque mayor jornada tiene menor atributo
tab max_jornada_cp_12mes 
replace max_jornada_cp_12mes=-1 if reg_fecha_w4==""

tab1 max_jornada_cp_2mes max_jornada_cp_6mes max_jornada_cp_12mes if todas_olas==1

// Máxima jornada por ola
egen max_jornada_2mes=rowmin(max_jornada_cp_2mes max_jornada_rem_2mes) // Rowmin porque mayor jornada tiene menor atributo
replace max_jornada_2mes=0 if trab_2mes==0
egen max_jornada_6mes=rowmin(max_jornada_cp_6mes max_jornada_rem_6mes) // Rowmin porque mayor jornada tiene menor atributo
replace max_jornada_6mes=0 if trab_6mes==0
egen max_jornada_12mes=rowmin(max_jornada_cp_12mes max_jornada_rem_12mes) // Rowmin porque mayor jornada tiene menor atributo
replace max_jornada_12mes=0 if trab_12mes==0


tab1 max_jornada_2mes max_jornada_6mes max_jornada_12mes if todas_olas==1 & trabajo==1

// Diferencias entre rem y cta propia
gen trab_dep_2mes=tipos_trab_dependencia_2mes 
gen trab_dep_6mes=tipos_trab_dependencia_6mes 
gen trab_dep_12mes=tipos_trab_dependencia_12mes

recode trab_dep_2mes trab_dep_6mes trab_dep_12mes (1=.)

tab max_jornada_2mes trab_dep_2mes if todas_olas==1 & trabajo==1, col chi2 
tab max_jornada_6mes trab_dep_6mes if todas_olas==1 & trabajo==1, col chi2 
tab max_jornada_12mes trab_dep_12mes if todas_olas==1 & trabajo==1, col chi2 

tab max_jornada_2mes if trab_dep_2mes!=. & todas_olas==1
tab max_jornada_6mes if trab_dep_6mes!=. & todas_olas==1
tab max_jornada_12mes if trab_dep_12mes!=. & todas_olas==1

// Proyeccion en trabajo
// Remunerados
codebook eaf_36_w2 eaf_36_w3 eaf_36_w4

// Cuenta propia
codebook eaf_56_3_w2 eaf_57_3_w2  
codebook eaf_56_3_w3 eaf_57_3_w3
codebook eaf_56_3_w4 eaf_57_3_w4

* Máxima proyeccion en empleo por ola
* 2 meses
egen max_proy_trab_2mes=rowmax(eaf_36_w2 eaf_56_3_w2 eaf_57_3_w2) // Rowmax porque mayor número es mayor proyección
replace max_proy_trab_2mes=-1 if trab_2mes==0
replace max_proy_trab_2mes=-2 if reg_fecha_w2==""
tab max_proy_trab_2mes

* 6 meses
egen max_proy_trab_6mes=rowmax(eaf_36_w3 eaf_56_3_w3 eaf_57_3_w3) // Rowmax porque mayor número es mayor proyección
replace max_proy_trab_6mes=-1 if trab_6mes==0
replace max_proy_trab_6mes=-2 if reg_fecha_w3==""
tab max_proy_trab_6mes

* 12 meses
egen max_proy_trab_12mes=rowmax(eaf_36_w4 eaf_56_3_w4 eaf_57_3_w4) // Rowmax porque mayor número es mayor proyección
replace max_proy_trab_12mes=-1 if trab_12mes==0
replace max_proy_trab_12mes=-2 if reg_fecha_w4==""
tab max_proy_trab_12mes

tab1 max_proy_trab_2mes max_proy_trab_6mes max_proy_trab_12mes if todas_olas==1 & trabajo==1

list eaf_36_w2 eaf_56_3_w2 eaf_57_3_w2 trab_2mes if max_proy_trab_2mes==-7
list eaf_36_w3 eaf_56_3_w3 eaf_57_3_w3 trab_6mes if max_proy_trab_6mes==-7
list eaf_36_w4 eaf_56_3_w4 eaf_57_3_w4 trab_12mes if max_proy_trab_12mes==-7

tab max_proy_trab_2mes trab_dep_2mes if todas_olas==1 & trabajo==1, col chi2 nofreq
tab max_proy_trab_6mes trab_dep_6mes if todas_olas==1 & trabajo==1, col chi2 nofreq 
tab max_proy_trab_12mes trab_dep_12mes if todas_olas==1 & trabajo==1, col chi2 nofreq

tab max_proy_trab_2mes if trab_dep_2mes!=. & todas_olas==1
tab max_proy_trab_6mes if trab_dep_6mes!=. & todas_olas==1
tab max_proy_trab_12mes if trab_dep_12mes!=. & todas_olas==1

// Evolución satisfacción con trabajo actual.
// Trabajo remunerado // 1=Muy satisfecha; 2=Satisfecha; 3=No muy satisfecha; 4=Para nada satisfecha; -7=NA; -8=NS; -9=NR
// Trabajo en general
tab1 eaf_41_1_w2 eaf_41_1_w3 eaf_41_1_w4 if todas_olas==1 & trabajo==1
replace eaf_41_1_w2=-1 if reg_fecha_w2==""
replace eaf_41_1_w3=-1 if reg_fecha_w3==""
replace eaf_41_1_w4=-1 if reg_fecha_w4==""
recode eaf_41_1_w2 eaf_41_1_w3 eaf_41_1_w4 (-7=7) (-8=8) (-9=9)

// Salario
tab1 eaf_41_2_w2 eaf_41_2_w3 eaf_41_2_w4
replace eaf_41_2_w2=-1 if reg_fecha_w2==""
replace eaf_41_2_w3=-1 if reg_fecha_w3==""
replace eaf_41_2_w4=-1 if reg_fecha_w4==""
recode eaf_41_2_w2 eaf_41_2_w3 eaf_41_2_w4 (-7=7) (-8=8) (-9=9)

// Trabajo por cuenta propia // 1=Muy satisfecha; 2=Satisfecha; 3=No muy satisfecha; 4=Para nada satisfecha; -7=NA; -8=NS; -9=NR
// Trabajo en general
tab1 eaf_58_1_w2 eaf_58_1_w3 eaf_58_1_w4
replace eaf_58_1_w2=-1 if reg_fecha_w2==""
replace eaf_58_1_w3=-1 if reg_fecha_w3==""
replace eaf_58_1_w4=-1 if reg_fecha_w4==""
recode eaf_58_1_w2 eaf_58_1_w3 eaf_58_1_w4 (-7=7) (-8=8) (-9=9)

// Salario
recode eaf_58_2_w2 eaf_58_2_w4 (-7=7) (-8=8) (-9=9)

// EMPLEO PRINCIPAL (declarado primero)
// Trabajo en general 2 meses
egen satis_trab_gen_w2=rowmin(eaf_41_1_w2 eaf_58_1_w2) // Rowmin porque menor valor es mayor satisfacción
replace satis_trab_gen_w2=-1 if reg_fecha_w2==""
tab satis_trab_gen_w2

// Trabajo en general 12 meses
egen satis_trab_gen_w4=rowmin(eaf_41_1_w4 eaf_58_1_w4) // Rowmin porque menor valor es mayor satisfacción
replace satis_trab_gen_w4=-1 if reg_fecha_w4==""
tab satis_trab_gen_w4
 
// Salario en general 2 meses
egen satis_trab_sal_w2=rowmin(eaf_41_2_w2 eaf_58_2_w2) // Rowmin porque menor valor es mayor satisfacción
replace satis_trab_sal_w2=-1 if reg_fecha_w2==""
tab satis_trab_sal_w2

// Salario en general 12 meses
egen satis_trab_sal_w4=rowmin(eaf_41_2_w4 eaf_58_2_w4)
replace satis_trab_sal_w4=-1 if reg_fecha_w4==""
tab satis_trab_sal_w4

tabout satis_trab_gen_w2 satis_trab_gen_w4 satis_trab_sal_w2 satis_trab_sal_w4 uno using asd.xls, replace cells(freq col) format(1p) stats(chi2)
tabout satis_trab_gen_w2 satis_trab_sal_w2 uno if trab_2mes==1 & todas_olas==1 using asd1.xls, replace cells(freq col) format(1p) stats(chi2)
tabout satis_trab_gen_w4 satis_trab_sal_w4 uno if trab_12mes==1 & todas_olas==1 using asd2.xls, replace cells(freq col) format(1p) stats(chi2)

tab satis_trab_gen_w2 trab_dep_2mes if trab_dep_2mes!=-1 & todas_olas==1, col 
tab satis_trab_gen_w4 trab_dep_12mes if trab_dep_12mes!=-1 & todas_olas==1, col 

tab satis_trab_sal_w2 trab_dep_2mes if trab_dep_2mes!=-1 & todas_olas==1, col nofreq
tab satis_trab_sal_w4 trab_dep_12mes if trab_dep_12mes!=-1 & todas_olas==1, col nofreq

// 4.A //////////////
*Describir a quienes no trabajan - razones de no trabajo
tab1 eaf_19_1_w2 eaf_19_1_w3 eaf_19_1_w4 if todas_olas==1
tab eaf_19_1_w2 if todas_olas==1
replace eaf_19_1_w2=0 if trab_2mes==1
replace eaf_19_1_w3=0 if trab_6mes==1
replace eaf_19_1_w4=0 if trab_12mes==1

* Razones para no trabajo, realizado de manera cualitativa a partir del texto declarado
* 12 meses
gen razon_no_trab_w4=.
replace razon_no_trab_w4=1 if reg_folio==30001
replace razon_no_trab_w4=1 if reg_folio==20161
replace razon_no_trab_w4=1 if reg_folio==30193
replace razon_no_trab_w4=1 if reg_folio==30237
replace razon_no_trab_w4=1 if reg_folio==20201
replace razon_no_trab_w4=1 if reg_folio==30139
replace razon_no_trab_w4=1 if reg_folio==20176
replace razon_no_trab_w4=1 if reg_folio==20257
replace razon_no_trab_w4=1 if reg_folio==50160
replace razon_no_trab_w4=1 if reg_folio==50249
replace razon_no_trab_w4=1 if reg_folio==20219
replace razon_no_trab_w4=1 if reg_folio==20256
replace razon_no_trab_w4=1 if reg_folio==30092
replace razon_no_trab_w4=1 if reg_folio==40199
replace razon_no_trab_w4=1 if reg_folio==40012
replace razon_no_trab_w4=1 if reg_folio==40156
replace razon_no_trab_w4=2 if reg_folio==50154
replace razon_no_trab_w4=2 if reg_folio==40046
replace razon_no_trab_w4=2 if reg_folio==40054
replace razon_no_trab_w4=2 if reg_folio==40091
replace razon_no_trab_w4=2 if reg_folio==50190
replace razon_no_trab_w4=2 if reg_folio==10197
replace razon_no_trab_w4=2 if reg_folio==20279
replace razon_no_trab_w4=2 if reg_folio==10244
replace razon_no_trab_w4=2 if reg_folio==20032
replace razon_no_trab_w4=3 if reg_folio==10087
replace razon_no_trab_w4=3 if reg_folio==40040
replace razon_no_trab_w4=3 if reg_folio==40166
replace razon_no_trab_w4=3 if reg_folio==40068
replace razon_no_trab_w4=3 if reg_folio==40122
replace razon_no_trab_w4=3 if reg_folio==20071
replace razon_no_trab_w4=5 if reg_folio==20048
replace razon_no_trab_w4=6 if reg_folio==30178
replace razon_no_trab_w4=6 if reg_folio==40194
replace razon_no_trab_w4=6 if reg_folio==20028
replace razon_no_trab_w4=6 if reg_folio==20157
replace razon_no_trab_w4=6 if reg_folio==10153
replace razon_no_trab_w4=6 if reg_folio==50049
replace razon_no_trab_w4=6 if reg_folio==30207
replace razon_no_trab_w4=6 if reg_folio==30266
replace razon_no_trab_w4=6 if reg_folio==40084
replace razon_no_trab_w4=6 if reg_folio==30029
replace razon_no_trab_w4=7 if reg_folio==20081
replace razon_no_trab_w4=7 if reg_folio==10075
replace razon_no_trab_w4=7 if reg_folio==10285
replace razon_no_trab_w4=7 if reg_folio==10192
replace razon_no_trab_w4=7 if reg_folio==20198
replace razon_no_trab_w4=7 if reg_folio==50209
replace razon_no_trab_w4=8 if reg_folio==10052
replace razon_no_trab_w4=8 if reg_folio==50180
replace razon_no_trab_w4=8 if reg_folio==50236
replace razon_no_trab_w4=8 if reg_folio==30264
replace razon_no_trab_w4=8 if reg_folio==20042
replace razon_no_trab_w4=8 if reg_folio==30269
replace razon_no_trab_w4=8 if reg_folio==30206
replace razon_no_trab_w4=8 if reg_folio==50150
replace razon_no_trab_w4=8 if reg_folio==10007
replace razon_no_trab_w4=8 if reg_folio==40002
replace razon_no_trab_w4=9 if reg_folio==50124
replace razon_no_trab_w4=9 if reg_folio==10272
replace razon_no_trab_w4=9 if reg_folio==30082
replace razon_no_trab_w4=10 if reg_folio==50104
replace razon_no_trab_w4=10 if reg_folio==10202
replace razon_no_trab_w4=10 if reg_folio==10053
replace razon_no_trab_w4=10 if reg_folio==40050
replace razon_no_trab_w4=10 if reg_folio==10044
replace razon_no_trab_w4=10 if reg_folio==50063
replace razon_no_trab_w4=11 if reg_folio==20208
replace razon_no_trab_w4=11 if reg_folio==10158
replace razon_no_trab_w4=11 if reg_folio==40152
replace razon_no_trab_w4=11 if reg_folio==20181
replace razon_no_trab_w4=11 if reg_folio==40010
replace razon_no_trab_w4=11 if reg_folio==20239
replace razon_no_trab_w4=11 if reg_folio==40215
replace razon_no_trab_w4=11 if reg_folio==50230
replace razon_no_trab_w4=11 if reg_folio==10293
replace razon_no_trab_w4=11 if reg_folio==40271
replace razon_no_trab_w4=11 if reg_folio==40169
replace razon_no_trab_w4=11 if reg_folio==40235
replace razon_no_trab_w4=11 if reg_folio==50205
replace razon_no_trab_w4=12 if reg_folio==30047
replace razon_no_trab_w4=12 if reg_folio==40006
replace razon_no_trab_w4=12 if reg_folio==40113
replace razon_no_trab_w4=12 if reg_folio==30300
replace razon_no_trab_w4=12 if reg_folio==10172
replace razon_no_trab_w4=12 if reg_folio==40184
replace razon_no_trab_w4=13 if reg_folio==50086
replace razon_no_trab_w4=13 if reg_folio==40268
replace razon_no_trab_w4=13 if reg_folio==30168
replace razon_no_trab_w4=13 if reg_folio==40284
replace razon_no_trab_w4=13 if reg_folio==40210
replace razon_no_trab_w4=13 if reg_folio==10288
replace razon_no_trab_w4=13 if reg_folio==40299
replace razon_no_trab_w4=14 if reg_folio==20262
replace razon_no_trab_w4=14 if reg_folio==20229
replace razon_no_trab_w4=14 if reg_folio==50251
replace razon_no_trab_w4=14 if reg_folio==20255
replace razon_no_trab_w4=14 if reg_folio==40287
replace razon_no_trab_w4=14 if reg_folio==20260
replace razon_no_trab_w4=14 if reg_folio==30116
replace razon_no_trab_w4=14 if reg_folio==30021
replace razon_no_trab_w4=14 if reg_folio==10233
replace razon_no_trab_w4=14 if reg_folio==30241
replace razon_no_trab_w4=14 if reg_folio==20224
replace razon_no_trab_w4=14 if reg_folio==10003
replace razon_no_trab_w4=16 if reg_folio==10055
replace razon_no_trab_w4=16 if reg_folio==20261
replace razon_no_trab_w4=16 if reg_folio==30051
replace razon_no_trab_w4=16 if reg_folio==10282

* dos meses
gen razon_no_trab_w2=.
replace razon_no_trab_w2=1 if reg_folio==30039
replace razon_no_trab_w2=1 if reg_folio==30300
replace razon_no_trab_w2=1 if reg_folio==30193
replace razon_no_trab_w2=1 if reg_folio==10003
replace razon_no_trab_w2=1 if reg_folio==50124
replace razon_no_trab_w2=1 if reg_folio==20261
replace razon_no_trab_w2=1 if reg_folio==30139
replace razon_no_trab_w2=1 if reg_folio==50271
replace razon_no_trab_w2=1 if reg_folio==30073
replace razon_no_trab_w2=1 if reg_folio==30164
replace razon_no_trab_w2=1 if reg_folio==10172
replace razon_no_trab_w2=1 if reg_folio==10153
replace razon_no_trab_w2=1 if reg_folio==20208
replace razon_no_trab_w2=1 if reg_folio==20096
replace razon_no_trab_w2=1 if reg_folio==40166
replace razon_no_trab_w2=1 if reg_folio==10197
replace razon_no_trab_w2=1 if reg_folio==10187
replace razon_no_trab_w2=1 if reg_folio==10202
replace razon_no_trab_w2=1 if reg_folio==20042
replace razon_no_trab_w2=1 if reg_folio==10293
replace razon_no_trab_w2=1 if reg_folio==30173
replace razon_no_trab_w2=2 if reg_folio==30069
replace razon_no_trab_w2=2 if reg_folio==10043
replace razon_no_trab_w2=2 if reg_folio==40284
replace razon_no_trab_w2=2 if reg_folio==40040
replace razon_no_trab_w2=2 if reg_folio==10119
replace razon_no_trab_w2=2 if reg_folio==20032
replace razon_no_trab_w2=2 if reg_folio==30092
replace razon_no_trab_w2=2 if reg_folio==40030
replace razon_no_trab_w2=3 if reg_folio==30222
replace razon_no_trab_w2=3 if reg_folio==50129
replace razon_no_trab_w2=5 if reg_folio==40091
replace razon_no_trab_w2=5 if reg_folio==50056
replace razon_no_trab_w2=5 if reg_folio==30146
replace razon_no_trab_w2=6 if reg_folio==30082
replace razon_no_trab_w2=7 if reg_folio==10288
replace razon_no_trab_w2=7 if reg_folio==10285
replace razon_no_trab_w2=7 if reg_folio==20262
replace razon_no_trab_w2=7 if reg_folio==40152
replace razon_no_trab_w2=7 if reg_folio==40010
replace razon_no_trab_w2=8 if reg_folio==30057
replace razon_no_trab_w2=8 if reg_folio==30047
replace razon_no_trab_w2=8 if reg_folio==10074
replace razon_no_trab_w2=8 if reg_folio==40002
replace razon_no_trab_w2=8 if reg_folio==10015
replace razon_no_trab_w2=8 if reg_folio==30118
replace razon_no_trab_w2=9 if reg_folio==30183
replace razon_no_trab_w2=9 if reg_folio==10027
replace razon_no_trab_w2=9 if reg_folio==10213
replace razon_no_trab_w2=10 if reg_folio==40210
replace razon_no_trab_w2=10 if reg_folio==10099
replace razon_no_trab_w2=10 if reg_folio==30241
replace razon_no_trab_w2=10 if reg_folio==30001
replace razon_no_trab_w2=10 if reg_folio==10044
replace razon_no_trab_w2=10 if reg_folio==40199
replace razon_no_trab_w2=10 if reg_folio==30206
replace razon_no_trab_w2=10 if reg_folio==40050
replace razon_no_trab_w2=10 if reg_folio==10177
replace razon_no_trab_w2=10 if reg_folio==50060
replace razon_no_trab_w2=11 if reg_folio==40194
replace razon_no_trab_w2=11 if reg_folio==40012
replace razon_no_trab_w2=11 if reg_folio==40299
replace razon_no_trab_w2=11 if reg_folio==40084
replace razon_no_trab_w2=11 if reg_folio==40179
replace razon_no_trab_w2=11 if reg_folio==20239
replace razon_no_trab_w2=11 if reg_folio==40184
replace razon_no_trab_w2=11 if reg_folio==20256
replace razon_no_trab_w2=11 if reg_folio==40271
replace razon_no_trab_w2=11 if reg_folio==40169
replace razon_no_trab_w2=11 if reg_folio==40156
replace razon_no_trab_w2=11 if reg_folio==50251
replace razon_no_trab_w2=12 if reg_folio==10077
replace razon_no_trab_w2=12 if reg_folio==40113
replace razon_no_trab_w2=12 if reg_folio==10007
replace razon_no_trab_w2=12 if reg_folio==40107
replace razon_no_trab_w2=12 if reg_folio==40287
replace razon_no_trab_w2=12 if reg_folio==40065
replace razon_no_trab_w2=12 if reg_folio==50180
replace razon_no_trab_w2=12 if reg_folio==40026
replace razon_no_trab_w2=12 if reg_folio==10102
replace razon_no_trab_w2=12 if reg_folio==40215
replace razon_no_trab_w2=12 if reg_folio==10233
replace razon_no_trab_w2=12 if reg_folio==20224
replace razon_no_trab_w2=12 if reg_folio==30207
replace razon_no_trab_w2=13 if reg_folio==10158
replace razon_no_trab_w2=13 if reg_folio==20260
replace razon_no_trab_w2=13 if reg_folio==10272
replace razon_no_trab_w2=14 if reg_folio==20257
replace razon_no_trab_w2=14 if reg_folio==30264
replace razon_no_trab_w2=14 if reg_folio==30051
replace razon_no_trab_w2=14 if reg_folio==30168
replace razon_no_trab_w2=14 if reg_folio==30021
replace razon_no_trab_w2=14 if reg_folio==30266
replace razon_no_trab_w2=14 if reg_folio==50126
replace razon_no_trab_w2=15 if reg_folio==10055
replace razon_no_trab_w2=15 if reg_folio==10053
replace razon_no_trab_w2=16 if reg_folio==30134
replace razon_no_trab_w2=16 if reg_folio==20061
replace razon_no_trab_w2=16 if reg_folio==20004


* seis meses
gen razon_no_trab_w3=.
replace razon_no_trab_w3=1 if reg_folio==30001
replace razon_no_trab_w3=1 if reg_folio==30173
replace razon_no_trab_w3=1 if reg_folio==40091
replace razon_no_trab_w3=1 if reg_folio==50129
replace razon_no_trab_w3=1 if reg_folio==50105
replace razon_no_trab_w3=1 if reg_folio==50049
replace razon_no_trab_w3=1 if reg_folio==30139
replace razon_no_trab_w3=1 if reg_folio==30168
replace razon_no_trab_w3=1 if reg_folio==50076
replace razon_no_trab_w3=1 if reg_folio==50271
replace razon_no_trab_w3=1 if reg_folio==10099
replace razon_no_trab_w3=1 if reg_folio==10202
replace razon_no_trab_w3=1 if reg_folio==50231
replace razon_no_trab_w3=1 if reg_folio==30073
replace razon_no_trab_w3=1 if reg_folio==10197
replace razon_no_trab_w3=1 if reg_folio==20201
replace razon_no_trab_w3=1 if reg_folio==20279
replace razon_no_trab_w3=1 if reg_folio==20259
replace razon_no_trab_w3=1 if reg_folio==20261
replace razon_no_trab_w3=1 if reg_folio==30222
replace razon_no_trab_w3=1 if reg_folio==30092
replace razon_no_trab_w3=2 if reg_folio==10087
replace razon_no_trab_w3=2 if reg_folio==20229
replace razon_no_trab_w3=2 if reg_folio==40113
replace razon_no_trab_w3=3 if reg_folio==40022
replace razon_no_trab_w3=3 if reg_folio==40046
replace razon_no_trab_w3=5 if reg_folio==20274
replace razon_no_trab_w3=5 if reg_folio==30273
replace razon_no_trab_w3=6 if reg_folio==40194
replace razon_no_trab_w3=6 if reg_folio==40179
replace razon_no_trab_w3=6 if reg_folio==10192
replace razon_no_trab_w3=6 if reg_folio==30178
replace razon_no_trab_w3=6 if reg_folio==20181
replace razon_no_trab_w3=6 if reg_folio==10153
replace razon_no_trab_w3=6 if reg_folio==10250
replace razon_no_trab_w3=6 if reg_folio==10074
replace razon_no_trab_w3=7 if reg_folio==40210
replace razon_no_trab_w3=7 if reg_folio==10075
replace razon_no_trab_w3=7 if reg_folio==10285
replace razon_no_trab_w3=7 if reg_folio==40019
replace razon_no_trab_w3=7 if reg_folio==40012
replace razon_no_trab_w3=8 if reg_folio==50150
replace razon_no_trab_w3=8 if reg_folio==10023
replace razon_no_trab_w3=8 if reg_folio==50160
replace razon_no_trab_w3=8 if reg_folio==50086
replace razon_no_trab_w3=8 if reg_folio==50180
replace razon_no_trab_w3=8 if reg_folio==30206
replace razon_no_trab_w3=8 if reg_folio==30188
replace razon_no_trab_w3=8 if reg_folio==40284
replace razon_no_trab_w3=8 if reg_folio==30082
replace razon_no_trab_w3=8 if reg_folio==50249
replace razon_no_trab_w3=9 if reg_folio==30047
replace razon_no_trab_w3=10 if reg_folio==10053
replace razon_no_trab_w3=10 if reg_folio==10272
replace razon_no_trab_w3=10 if reg_folio==10003
replace razon_no_trab_w3=10 if reg_folio==50281
replace razon_no_trab_w3=10 if reg_folio==40050
replace razon_no_trab_w3=10 if reg_folio==40065
replace razon_no_trab_w3=10 if reg_folio==10027
replace razon_no_trab_w3=10 if reg_folio==20081
replace razon_no_trab_w3=11 if reg_folio==20255
replace razon_no_trab_w3=11 if reg_folio==20239
replace razon_no_trab_w3=11 if reg_folio==40084
replace razon_no_trab_w3=11 if reg_folio==40271
replace razon_no_trab_w3=11 if reg_folio==10244
replace razon_no_trab_w3=11 if reg_folio==20260
replace razon_no_trab_w3=11 if reg_folio==10158
replace razon_no_trab_w3=11 if reg_folio==30116
replace razon_no_trab_w3=11 if reg_folio==40215
replace razon_no_trab_w3=11 if reg_folio==40156
replace razon_no_trab_w3=12 if reg_folio==20017
replace razon_no_trab_w3=12 if reg_folio==40199
replace razon_no_trab_w3=12 if reg_folio==30264
replace razon_no_trab_w3=12 if reg_folio==30207
replace razon_no_trab_w3=12 if reg_folio==30266
replace razon_no_trab_w3=12 if reg_folio==30300
replace razon_no_trab_w3=12 if reg_folio==30193
replace razon_no_trab_w3=12 if reg_folio==30183
replace razon_no_trab_w3=12 if reg_folio==20195
replace razon_no_trab_w3=12 if reg_folio==30301
replace razon_no_trab_w3=12 if reg_folio==20262
replace razon_no_trab_w3=12 if reg_folio==20176
replace razon_no_trab_w3=13 if reg_folio==40235
replace razon_no_trab_w3=13 if reg_folio==40166
replace razon_no_trab_w3=13 if reg_folio==10043
replace razon_no_trab_w3=13 if reg_folio==10293
replace razon_no_trab_w3=14 if reg_folio==30021
replace razon_no_trab_w3=14 if reg_folio==30237
replace razon_no_trab_w3=14 if reg_folio==40169
replace razon_no_trab_w3=14 if reg_folio==30241
replace razon_no_trab_w3=14 if reg_folio==20198
replace razon_no_trab_w3=14 if reg_folio==40268
replace razon_no_trab_w3=14 if reg_folio==20042
replace razon_no_trab_w3=14 if reg_folio==20224
replace razon_no_trab_w3=14 if reg_folio==30269
replace razon_no_trab_w3=15 if reg_folio==50209
replace razon_no_trab_w3=15 if reg_folio==10223
replace razon_no_trab_w3=16 if reg_folio==50230
replace razon_no_trab_w3=16 if reg_folio==20008
replace razon_no_trab_w3=16 if reg_folio==10102
replace razon_no_trab_w3=16 if reg_folio==10233

label define razon_no_trab -9"NR" -7"NA" 0"Trabajo en esa ola" 1"Antecedentes" 2"No ha encontrado" 3"Dejo el trabajo o Despedida" ///
5"Esta esperando comenzar o respuesta" 6"Carcel" 7"Consumo de drogas/alcohol" 8"Cuidado de hijos o familiares o casa" ///
9"Pareja no le permite" 10"Salud" 11"Delinque" 12"No ha buscado" 13"No lo necesita" 14"No quiere" 15"Esta trabajando" ///
16"Otro"

label value razon_no_trab_w4 razon_no_trab_w2 razon_no_trab_w3 razon_no_trab
replace razon_no_trab_w2=-7 if eaf_19_2_w2=="-7" & missing(razon_no_trab_w2)
replace razon_no_trab_w2=-9 if eaf_19_2_w2=="-9" & missing(razon_no_trab_w2)
replace razon_no_trab_w3=-7 if eaf_19_2_w3=="-7" & missing(razon_no_trab_w3)
replace razon_no_trab_w3=-9 if eaf_19_2_w3=="-9" & missing(razon_no_trab_w3)
replace razon_no_trab_w4=-7 if eaf_19_2_w4=="-7" & missing(razon_no_trab_w4)
replace razon_no_trab_w4=-9 if eaf_19_2_w4=="-9" & missing(razon_no_trab_w4)
replace razon_no_trab_w2=0 if trab_2mes==1
replace razon_no_trab_w3=0 if trab_6mes==1
replace razon_no_trab_w4=0 if trab_12mes==1

tab1 razon_no_trab_w2 razon_no_trab_w3 razon_no_trab_w4 if todas_olas==1

list eaf_19_2_w2 if razon_no_trab_w2==. & todas_olas==1 
list eaf_19_2_w3 if razon_no_trab_w3==. & todas_olas==1
list eaf_19_2_w4 if razon_no_trab_w4==. & todas_olas==1

list reg_folio eaf_19_1_w2 eaf_19_2_w2 if razon_no_trab_w2==-9
list reg_folio eaf_19_1_w3 eaf_19_2_w3 if razon_no_trab_w3==-9

replace razon_no_trab_w2=2 if reg_folio==10244
replace razon_no_trab_w2=2 if reg_folio==20008
replace razon_no_trab_w2=-9 if reg_folio==20157
replace razon_no_trab_w2=1 if reg_folio==20161
replace razon_no_trab_w2=16 if reg_folio==20176
replace razon_no_trab_w2=8 if reg_folio==20195
replace razon_no_trab_w2=1 if reg_folio==20201
replace razon_no_trab_w2=1 if reg_folio==20219
replace razon_no_trab_w2=1 if reg_folio==20255
replace razon_no_trab_w2=14 if reg_folio==20259
replace razon_no_trab_w2=2 if reg_folio==20279
replace razon_no_trab_w2=1 if reg_folio==50049
replace razon_no_trab_w2=1 if reg_folio==50086
replace razon_no_trab_w2=10 if reg_folio==50133
replace razon_no_trab_w2=1 if reg_folio==50190
replace razon_no_trab_w2=14 if reg_folio==50205
replace razon_no_trab_w2=1 if reg_folio==50249

replace razon_no_trab_w3=16 if reg_folio==20157
replace razon_no_trab_w3=1 if reg_folio==20161
replace razon_no_trab_w3=16 if reg_folio==20208
replace razon_no_trab_w3=16 if reg_folio==20257
replace razon_no_trab_w3=1 if reg_folio==50214
replace razon_no_trab_w3=8 if reg_folio==50236
replace razon_no_trab_w3=14 if reg_folio==50251

//// Algunos cruces
tab spg_10_8_w2 trab_2mes if todas_olas==1, col chi2 nofreq
tab spg_10_8_w3 trab_6mes if todas_olas==1, col chi2 nofreq
tab spg_10_8_w4 trab_12mes if todas_olas==1, col chi2 nofreq

/// 4.B CARACTERIZACION DE QUIENES NO TRABAJAN.
** Edad
gen age_gr=hdv_1_w0
recode age_gr (19/25=1) (26/35=2) (36/45=3) (46/55=4) (56/68=5)
lab def age 1 "19-25" 2 "26-35" 3 "36-45" 4 "46-55" 5 "56+"
lab val age_gr age
lab var age_gr "Age Groups"
tab age_gr

** Educación
gen educ_gr3=hdv_7_w0
recode educ_gr3 (0/7=0) (8/11=1) (12/16=2)
lab def educ4 0"Sin básica completa" 1"Media incompleta" 2"Media completa o más"
lab val educ_gr3 educ4
tab educ_gr3

** Tipo de delito condena actual
gen typeCrime=del_10_w0 /*otros pasan a prop dado que incluyen receptacion, estafa*/
recode typeCrime (7=1) (1/6=2) (8/11=2) (12/14=3) (15/16=4)  (17/18=2) (19/20=3) (21/22=2) (23=5) (-9=.)  
recode typeCrime (24=3) if reg_folio==30039   /*Otro =Parricidio*/
recode typeCrime (24=3) if reg_folio==30303   /*Otro=Ley Emilia**/
recode typeCrime (.=3) if reg_folio==50129   /*Imputo delito de Other Property a folio2==50129 que aparecía como missing por haber reportado dos tips de robo con intimidación**/
lab def tcrime  1 "Theft" 2 "Other Property" 3 "Person" 4 "Drug" 5 "Other" 
lab val typeCrime tcrime
lab val typeCrime tcrime 
lab var typeCrime "Current offense"
tab typeCrime

** Probabilidad de que vuelva a delinquir LB
tab del_49_w0

**** 4.2 DEPENDENCIA Y ABUSO  **

**Imputar missing como 0
foreach i of varlist dro_7_1_w0-dro_7_11_w4 dro_6_1_w0-dro_6_11_w4  {
	gen Rec_`i' = `i' 
	recode Rec_`i' (-7=.) (-8=0) (-9=0)
}

*Dependencia con missing=0 para LB
gen Dependence_Sust1=Rec_dro_6_1_w0 + Rec_dro_6_2_w0 + Rec_dro_6_4_w0 + Rec_dro_6_5_w0 + Rec_dro_6_6_w0 + Rec_dro_6_7_w0 
recode Dependence_Sust1 (0/2=0) (3/6=1)

gen Dependence_Sust2=Rec_dro_7_1_w0 + Rec_dro_7_2_w0 + Rec_dro_7_4_w0 + Rec_dro_7_5_w0 + Rec_dro_7_6_w0 + Rec_dro_7_7_w0
recode Dependence_Sust2 (0/2=0) (3/6=1)

gen Abuse_Sust1=Rec_dro_6_8_w0 + Rec_dro_6_9_w0 + Rec_dro_6_10_w0 + Rec_dro_6_11_w0 
recode Abuse_Sust1 (0=0) (1/4=1)

gen Abuse_Sust2=Rec_dro_7_8_w0 + Rec_dro_7_9_w0 + Rec_dro_7_10_w0 + Rec_dro_7_11_w0 
recode Abuse_Sust2 (0=0) (1/4=1)

tab1 Dependence_Sust1 Dependence_Sust2 Abuse_Sust1 Abuse_Sust2

gen AnyDependence=1 if (Dependence_Sust1==1 | Dependence_Sust2==1)
recode AnyDependence (.=0) 

gen AnyAbuse=1 if (Abuse_Sust1==1 | Abuse_Sust2==1) 
recode AnyAbuse (.=0)  

gen dep_Abuse=1 if (AnyDependence==1 | AnyAbuse==1) 
recode dep_Abuse (.=0) 

gen dep_abuse2=0 if (AnyDependence==0 & AnyAbuse==0) 
recode dep_abuse2 (.=1) if (AnyDependence==0 & AnyAbuse==1) 
recode dep_abuse2 (.=2) if (AnyDependence==1) 

lab def dep_abuse2  0 "Sin consumo problemático" 1 "síntomas abuso" 2 "síntomas dependencia" 
lab val dep_abuse2 dep_abuse2 
tab dep_abuse2

**  Estabilidad residencial dicotomizada
gen vivido_un_lugar=viv_12_w4
recode vivido_un_lugar (1=1) (2/7=0)
lab def sino 0"No" 1"Si"
lab val vivido_un_lugar sino
tab vivido_un_lugar

* Relevancia empleo LB
tab eaf_14_w0

* Control sobre la vida en LB
tab1 car_1_10_w0 car_1_11_w0 car_1_12_w0 car_1_13_w0 car_1_14_w0 car_1_15_w0 car_1_16_w0
recode car_1_10_w0 car_1_11_w0 car_1_12_w0 car_1_13_w0 car_1_14_w0 car_1_15_w0 car_1_16_w0 (-7 -8 -9=.)
recode car_1_11_w0 car_1_15_w0 (1=4) (2=3) (3=2) (4=1)
alpha car_1_10_w0 car_1_11_w0 car_1_12_w0 car_1_13_w0 car_1_14_w0 car_1_15_w0 car_1_16_w0, i gen(ControlVida) 
tab ControlVida

* Disposición al cambio en LB
tab1 spg_8_1_w0 spg_8_2_w0 spg_8_3_w0 spg_8_4_w0
recode spg_8_1_w0 spg_8_2_w0 spg_8_3_w0 spg_8_4_w0 (-7 -8 -9=.)
alpha spg_8_1_w0 spg_8_2_w0 spg_8_3_w0 spg_8_4_w0, i gen(DispCambio)
tab DispCambio

* Obligaciones económicas reportadas último mes - 2 meses
tab1 eaf_70_1_4_w2 eaf_70_2_4_w2 eaf_70_3_4_w2 eaf_70_4_4_w2 eaf_70_5_4_w2 eaf_70_6_4_w2 eaf_70_7_4_w2 eaf_70_8_4_w2 eaf_70_9_4_w2 eaf_70_10_4_w2
recode eaf_70_1_4_w2 eaf_70_2_4_w2 eaf_70_3_4_w2 eaf_70_4_4_w2 eaf_70_5_4_w2 eaf_70_6_4_w2 eaf_70_7_4_w2 eaf_70_8_4_w2 eaf_70_9_4_w2 eaf_70_10_4_w2 (-7 -8 -9=.)
egen obli_eco_monto_2mes=rowtotal(eaf_70_1_4_w2 eaf_70_2_4_w2 eaf_70_3_4_w2 eaf_70_4_4_w2 eaf_70_5_4_w2 eaf_70_6_4_w2 eaf_70_7_4_w2 eaf_70_8_4_w2 eaf_70_9_4_w2 eaf_70_10_4_w2)
tab obli_eco_monto_2mes
replace obli_eco_monto_2mes=-1 if reg_fecha_w2==""
tab obli_eco_monto_2mes if todas_olas==1
sum obli_eco_monto_2mes if todas_olas==1

pctile obl_eco_3a=obli_eco_monto_2mes if todas_olas==1, nquantiles(3)
xtile obl_eco_3=obli_eco_monto_2mes if todas_olas==1, nquantiles(3)
tab obl_eco_3a
 
lab def obl_eco 1"Menos de 40.000" 2"Entre 40.001 y 180.000" 3"180.001 o mas"
lab val obl_eco_3 obl_eco
tab obl_eco_3

tab obl_eco_3 trabajo, row chi2

* Tiene hijos menores de edad en LB
gen hijo_menor=h_num_hijos_menores_w0
recode hijo_menor (-7 0=0) (1/7=1)
lab val hijo_menor sino 
tab hijo_menor

tabout age_gr educ_gr3 typeCrime del_49_w0 dep_abuse2 vivido_un_lugar eaf_14_w0 hijo_menor trabajo if todas_olas==1 using asd1.xls, replace cells(freq col) format(1p) stats(chi2)  
robvar ControlVida if todas_olas==1, by(trabajo)
ttest ControlVida if todas_olas==1, by(trabajo)
robvar DispCambio if todas_olas==1, by(trabajo)
ttest DispCambio if todas_olas==1, by(trabajo)
robvar obli_eco_monto_2mes if todas_olas==1, by(trabajo)
ttest obli_eco_monto_2mes if todas_olas==1, by(trabajo) unequal

**SALUD MENTAL**

foreach i of varlist sal_31_1_w0 sal_31_2_w0 sal_31_3_w0 sal_31_4_w0 sal_31_5_w0 sal_31_6_w0 sal_31_7_w0 sal_31_8_w0 sal_31_9_w0 sal_31_10_w0 sal_31_11_w0 sal_31_12_w0 sal_31_13_w0 sal_31_14_w0 sal_31_15_w0 sal_31_16_w0 sal_31_17_w0 sal_31_18_w0 sal_31_19_w0 sal_31_20_w0 sal_31_21_w0 sal_31_22_w0 sal_31_23_w0 sal_31_24_w0 sal_31_25_w0 sal_31_26_w0 sal_31_27_w0 sal_31_28_w0 sal_31_29_w0 sal_31_30_w0 sal_31_31_w0 sal_31_32_w0 sal_31_33_w0 sal_31_34_w0 sal_31_35_w0 sal_31_36_w0 sal_31_37_w0 sal_31_38_w0 sal_31_39_w0 sal_31_40_w0 sal_31_41_w0 sal_31_42_w0 sal_31_43_w0 sal_31_44_w0 sal_31_45_w0 sal_31_46_w0 sal_31_47_w1 sal_31_48_w0 sal_31_49_w0 sal_31_50_w0 sal_31_51_w0 sal_31_52_w0 sal_31_53_w0 sal_31_54_w0 sal_31_55_w0 sal_31_56_w0 sal_31_57_w0 sal_31_58_w0 sal_31_59_w0 sal_31_60_w0 sal_31_61_w0 sal_31_62_w0 sal_31_63_w0 sal_31_64_w0 sal_31_65_w0 sal_31_66_w0 sal_31_67_w0 sal_31_68_w0 sal_31_69_w0 sal_31_70_w0 sal_31_71_w0 sal_31_72_w0 sal_31_73_w0 sal_31_74_w0 sal_31_75_w0  sal_31_76_w0  sal_31_77_w0  sal_31_78_w0  sal_31_79_w0  sal_31_80_w0  sal_31_81_w0  sal_31_82_w0  sal_31_83_w0  sal_31_84_w0  sal_31_85_w0  sal_31_86_w0  sal_31_87_w0  sal_31_88_w0  sal_31_89_w0  sal_31_90_w0  {

            gen `i'rec = `i'

}


recode sal_31_1_w0rec sal_31_2_w0rec sal_31_3_w0rec sal_31_4_w0rec sal_31_5_w0rec sal_31_6_w0rec sal_31_7_w0rec sal_31_8_w0rec sal_31_9_w0rec sal_31_10_w0rec sal_31_11_w0rec sal_31_12_w0rec sal_31_13_w0rec sal_31_14_w0rec sal_31_15_w0rec sal_31_16_w0rec sal_31_17_w0rec sal_31_18_w0rec sal_31_19_w0rec sal_31_20_w0rec sal_31_21_w0rec sal_31_22_w0rec sal_31_23_w0rec sal_31_24_w0rec sal_31_25_w0rec sal_31_26_w0rec sal_31_27_w0rec sal_31_28_w0rec sal_31_29_w0rec sal_31_30_w0rec sal_31_31_w0rec sal_31_32_w0rec sal_31_33_w0rec sal_31_34_w0rec sal_31_35_w0rec sal_31_36_w0rec sal_31_37_w0rec sal_31_38_w0rec sal_31_39_w0rec sal_31_40_w0rec sal_31_41_w0rec sal_31_42_w0rec sal_31_43_w0rec sal_31_44_w0rec sal_31_45_w0rec sal_31_46_w0rec sal_31_47_w1rec sal_31_48_w0rec sal_31_49_w0rec sal_31_50_w0rec sal_31_51_w0rec sal_31_52_w0rec sal_31_53_w0rec sal_31_54_w0rec sal_31_55_w0rec sal_31_56_w0rec sal_31_57_w0rec sal_31_58_w0rec sal_31_59_w0rec sal_31_60_w0rec sal_31_61_w0rec sal_31_62_w0rec sal_31_63_w0rec sal_31_64_w0rec sal_31_65_w0rec sal_31_66_w0rec sal_31_67_w0rec sal_31_68_w0rec sal_31_69_w0rec sal_31_70_w0rec sal_31_71_w0rec sal_31_72_w0rec sal_31_73_w0rec sal_31_74_w0rec sal_31_75_w0rec sal_31_76_w0rec sal_31_77_w0rec sal_31_78_w0rec sal_31_79_w0rec sal_31_80_w0rec sal_31_81_w0rec sal_31_82_w0rec sal_31_83_w0rec sal_31_84_w0rec sal_31_85_w0rec sal_31_86_w0rec sal_31_87_w0rec sal_31_88_w0rec sal_31_89_w0rec sal_31_90_w0rec (-9=.)

egen nmis=rowmiss(sal_31_1_w0rec-sal_31_90_w0rec)

gen nvalid= 89- nmis

egen severidad_global= rowmean(sal_31_1_w0rec-sal_31_90_w0rec)

replace severidad_global=. if nvalid < 80

gen malestar_smental=severidad_global

recode malestar_smental  (0 / 1.42 = 0) (1.421 / 4 = 1)

lab def malestar 0 "sin malestar severo" 1 "malestar severo smental"

lab val malestar_smental malestar
tab malestar_smental

egen somatizaciones= rowmean (sal_31_1_w0rec sal_31_4_w0rec sal_31_12_w0rec sal_31_27_w0rec sal_31_40_w0rec sal_31_42_w0rec sal_31_48_w0rec sal_31_49_w0rec sal_31_52_w0rec sal_31_53_w0rec sal_31_56_w0rec sal_31_58_w0rec)
egen obsesiones= rowmean (sal_31_3_w0rec sal_31_9_w0rec sal_31_10_w0rec sal_31_28_w0rec sal_31_38_w0rec sal_31_45_w0rec sal_31_46_w0rec sal_31_51_w0rec sal_31_55_w0rec sal_31_65_w0rec)
egen sensitividad = rowmean (sal_31_6_w0rec sal_31_21_w0rec sal_31_34_w0rec sal_31_36_w0rec sal_31_37_w0rec sal_31_41_w0rec sal_31_61_w0rec sal_31_69_w0rec sal_31_73_w0rec)
egen depresion =  rowmean (sal_31_5_w0rec sal_31_14_w0rec sal_31_15_w0rec sal_31_20_w0rec sal_31_22_w0rec sal_31_26_w0rec sal_31_29_w0rec sal_31_30_w0rec sal_31_31_w0rec sal_31_32_w0rec sal_31_54_w0rec sal_31_71_w0rec sal_31_79_w0rec)
egen ansiedad =  rowmean (sal_31_2_w0rec sal_31_17_w0rec sal_31_23_w0rec sal_31_33_w0rec sal_31_39_w0rec sal_31_57_w0rec sal_31_72_w0rec sal_31_78_w0rec sal_31_80_w0rec sal_31_86_w0rec)
egen hostilidad =  rowmean (sal_31_11_w0rec sal_31_24_w0rec sal_31_63_w0rec sal_31_67_w0rec sal_31_74_w0rec sal_31_81_w0rec)
egen ansiedadfobica =  rowmean (sal_31_13_w0rec sal_31_25_w0rec sal_31_47_w1rec sal_31_50_w0rec sal_31_75_w0rec sal_31_82_w0rec)
egen paranoide =  rowmean (sal_31_8_w0rec sal_31_18_w0rec sal_31_43_w0rec sal_31_68_w0rec sal_31_76_w0rec sal_31_83_w0rec)
egen psicotismo =  rowmean (sal_31_7_w0rec sal_31_16_w0rec sal_31_35_w0rec sal_31_62_w0rec sal_31_77_w0rec sal_31_84_w0rec sal_31_85_w0rec sal_31_87_w0rec sal_31_88_w0rec sal_31_90_w0rec) 

** Cruces tercera vez
tab busco_trab trabajo if todas_olas==1, col chi2 nofreq

tab1 eaf_21_w2 eaf_21_w3 eaf_21_w4 if todas_olas==1 // un NR en 6 meses
recode eaf_21_w3 (-9=.)

tab eaf_21_w2 trab_6mes if todas_olas==1, row chi2 nofreq
tab eaf_21_w3 trab_12mes if todas_olas==1, row chi2 nofreq

tab malestar_smental trabajo if todas_olas==1, col chi2 nofreq

ttest somatizaciones if todas_olas==1, by(trabajo)
ttest obsesiones if todas_olas==1, by(trabajo)
ttest sensitividad if todas_olas==1, by(trabajo)
robvar depresion if todas_olas==1, by(trabajo)
ttest depresion if todas_olas==1, by(trabajo)
ttest ansiedad if todas_olas==1, by(trabajo)
ttest hostilidad if todas_olas==1, by(trabajo)
ttest ansiedadfobica if todas_olas==1, by(trabajo)
ttest paranoide if todas_olas==1, by(trabajo)
ttest psicotismo if todas_olas==1, by(trabajo)

* Cuali de trabajos por tarea
list eaf_trcp1_1_w2 if todas_olas==1 & eaf_trcp1_1_3_w2==6
list eaf_trcp1_1_w3 if todas_olas==1 & eaf_trcp1_1_3_w3==6
list eaf_trcp1_1_w4 if todas_olas==1 & eaf_trcp1_1_3_w4==6

list eaf_trcp2_1_w2 if todas_olas==1 & eaf_trcp2_1_2_w2==6
list eaf_trcp2_1_w3 if todas_olas==1 & eaf_trcp2_1_2_w3==6
list eaf_trcp2_1_w4 if todas_olas==1 & eaf_trcp2_1_2_w4==6

** Trabajo antes de estar presa
tab eaf_1_w0
tab eaf_6_w0
tab eaf_43_w0
recode eaf_1_w0 eaf_6_w0 eaf_43_w0 (-7 -8 -9=.)

gen trab_6mes_antes=.
replace trab_6mes_antes=1 if eaf_6_w0==1 | eaf_43_w0==1
replace trab_6mes_antes=0 if missing(trab_6mes_antes)
replace trab_6mes_antes=. if eaf_6_w0==. & eaf_43_w0==.
tab trab_6mes_antes

tab trabajo trab_6mes_antes if todas_olas==1, col chi2 
tab trabajo eaf_1_w0 if todas_olas==1, col chi2 nofreq
tab trabajo exc_10_w0 if todas_olas==1, col chi2 

* Guardo la base de datos con todos las variables generadas
* save "C:\Users\Usuario\Dropbox\calendarios_reinsercion\Reporte Empleo\base_general_wide_convariables.dta", replace

//// PAPER SECUENCIAS LABORALES/DELICTUALES
//// Calendarios
* use base_general_wide_convariables.dta, clear

// Presente en cada ola
gen dos_meses=1 if reg_fecha_w2!=""
replace dos_meses=0 if missing(dos_meses)
label value dos_meses muestra

gen seis_meses=1 if reg_fecha_w3!=""
replace seis_meses=0 if missing(seis_meses)
label value seis_meses muestra

gen doce_meses=1 if reg_fecha_w4!=""
replace doce_meses=0 if missing(doce_meses)
label value doce_meses muestra

egen dos_seis_doce=rowtotal(dos_meses  seis_meses doce_meses)
tab dos_seis_doce, m
drop if dos_seis_doce==0 // 18 casos que no están en 2M, 6M o 12M

/// TIPO DE TRABAJO POR OLA 1=Dependiente 0=Cuenta propia
/// FORMALIDAD 1=Formar 0=Informal

* Ola 2, tipo de trabajo
* Homogeinizo las variables creadas anteriormente de manera que 1=dependiente 0=cta propia
tab1 tipotrabajo_po1_2mes tipotrabajo_po2_2mes tipotrabajo_po3_2mes tipotrabajo_cp1_2mes tipotrabajo_cp2_2mes tipotrabajo_cp3_2mes
codebook tipotrabajo_po1_2mes tipotrabajo_po2_2mes tipotrabajo_po3_2mes tipotrabajo_cp1_2mes tipotrabajo_cp2_2mes tipotrabajo_cp3_2mes
recode tipotrabajo_po1_2mes tipotrabajo_po2_2mes tipotrabajo_po3_2mes (1=1) (0=0)
recode tipotrabajo_cp1_2mes tipotrabajo_cp2_2mes tipotrabajo_cp3_2mes (0=1) (1=0)

* Ola 2, formalidad. Esta vez es POR TRABAJO, y no agregado por ola
* Contrato: trabajo remunerado
tab eaf_trcp1_1_1_w2
gen contrato_1_2mes=eaf_trcp1_1_1_w2
recode contrato_1_2mes (-7=.)
tab contrato_1_2mes

tab eaf_trcp1_2_1_w2
gen contrato_2_2mes=eaf_trcp1_2_1_w2
recode contrato_2_2mes (-7=.)
tab contrato_2_2mes

tab eaf_trcp1_3_1_w2
gen contrato_3_2mes=eaf_trcp1_3_1_w2
recode contrato_3_2mes (-7=.)
tab contrato_3_2mes

tab eaf_trcp1_4_1_w2
gen contrato_4_2mes=eaf_trcp1_4_1_w2
recode contrato_4_2mes (-7=.)
tab contrato_4_2mes

* Cotiza: trabajo remunerado
tab eaf_trcp1_1_2_w2
gen cotiza_1_2mes=eaf_trcp1_1_2_w2
recode cotiza_1_2mes (-7 -8 -9=.)
tab cotiza_1_2mes

tab eaf_trcp1_2_2_w2
gen cotiza_2_2mes=eaf_trcp1_2_2_w2
recode cotiza_2_2mes (-7 -8 -9=.)
tab cotiza_2_2mes

tab eaf_trcp1_3_2_w2
gen cotiza_3_2mes=eaf_trcp1_3_2_w2
recode cotiza_3_2mes (-7 -8 -9=.)
tab cotiza_3_2mes

tab eaf_trcp1_4_2_w2
gen cotiza_4_2mes=eaf_trcp1_4_2_w2
recode cotiza_4_2mes (-7 -8 -9=.)
tab cotiza_4_2mes

* Formalidad: trabajo remunera
egen forma_1_2mes=rowtotal(contrato_1_2mes cotiza_1_2mes) // Si es mayor a 1 tenía al menos una de las características de formal.
egen forma_2_2mes=rowtotal(contrato_2_2mes cotiza_2_2mes) // Si es mayor a 1 tenía al menos una de las características de formal.
egen forma_3_2mes=rowtotal(contrato_3_2mes cotiza_3_2mes) // Si es mayor a 1 tenía al menos una de las características de formal.
egen forma_4_2mes=rowtotal(contrato_4_2mes cotiza_4_2mes) // Si es mayor a 1 tenía al menos una de las características de formal.
recode forma_1_2mes forma_2_2mes forma_3_2mes forma_4_2mes (1/2=1) (0=0)
replace forma_1_2mes=. if dos_meses==0
replace forma_2_2mes=. if dos_meses==0
replace forma_3_2mes=. if dos_meses==0
replace forma_4_2mes=. if dos_meses==0
replace forma_1_2mes=. if contrato_1_2mes==. & cotiza_1_2mes==.
replace forma_2_2mes=. if contrato_2_2mes==. & cotiza_2_2mes==.
replace forma_3_2mes=. if contrato_3_2mes==. & cotiza_3_2mes==.
replace forma_4_2mes=. if contrato_4_2mes==. & cotiza_4_2mes==.

* Formalidad: trabajo por cuenta propia
gen boleta_1_2mes=eaf_trcp2_1_1_w2 
recode boleta_1_2mes (-7 -8 -9=.)
tab boleta_1_2mes

gen boleta_2_2mes=eaf_trcp2_2_1_w2 
recode boleta_2_2mes (-7 -8 -9=.)
tab boleta_2_2mes

gen boleta_3_2mes=eaf_trcp2_3_1_w2 
recode boleta_3_2mes (-7 -8 -9=.)
tab boleta_3_2mes

gen boleta_4_2mes=eaf_trcp2_4_1_w2 
recode boleta_4_2mes (-7 -8 -9=.)
tab boleta_4_2mes

* Tipo de trabajo 2 meses - declarados como remunerados
gen tipo_trabajo_po_1_mes2=tipotrabajo_po1_2mes*10+forma_1_2mes
gen tipo_trabajo_po_2_mes2=tipotrabajo_po2_2mes*10+forma_2_2mes
gen tipo_trabajo_po_3_mes2=tipotrabajo_po3_2mes*10+forma_3_2mes
lab def tip_trab 11"Dependiente Formal" 10"Dependiente informal" 1"Cuenta propia formal" 0"Cuenta propia informal"
lab val tipo_trabajo_po_1_mes2 tipo_trabajo_po_2_mes2 tipo_trabajo_po_3_mes2 tip_trab
tab1 tipo_trabajo_po_1_mes2 tipo_trabajo_po_2_mes2 tipo_trabajo_po_3_mes2

* Tipo de trabajo 2 meses - declarados como cuenta propia
gen tipo_trabajo_cp_1_mes2=tipotrabajo_cp1_2mes*10+boleta_1_2mes
gen tipo_trabajo_cp_2_mes2=tipotrabajo_cp2_2mes*10+boleta_2_2mes
gen tipo_trabajo_cp_3_mes2=tipotrabajo_cp3_2mes*10+boleta_3_2mes
lab val tipo_trabajo_cp_1_mes2 tipo_trabajo_cp_2_mes2 tipo_trabajo_cp_3_mes2  tip_trab
tab1 tipo_trabajo_cp_1_mes2 tipo_trabajo_cp_2_mes2 tipo_trabajo_cp_3_mes2 

************************************************
* Ola 3, tipo de trabajo. Esta vez es POR TRABAJO, y no agregado por ola
tab1 tipotrabajo_po1_6mes tipotrabajo_po2_6mes tipotrabajo_po3_6mes tipotrabajo_cp1_6mes tipotrabajo_cp2_6mes tipotrabajo_cp3_6mes
codebook tipotrabajo_po1_6mes tipotrabajo_po2_6mes tipotrabajo_po3_6mes tipotrabajo_cp1_6mes tipotrabajo_cp2_6mes tipotrabajo_cp3_6mes
recode tipotrabajo_po1_6mes tipotrabajo_po2_6mes tipotrabajo_po3_6mes (1=1) (0=0)
recode tipotrabajo_cp1_6mes tipotrabajo_cp2_6mes tipotrabajo_cp3_6mes (0=1) (1=0)

* Ola 3, formalidad
* Contrato: trabajo remunerado
tab eaf_trcp1_1_1_w3
gen contrato_1_6mes=eaf_trcp1_1_1_w3
recode contrato_1_6mes (-7 -9=.)
tab contrato_1_6mes

tab eaf_trcp1_2_1_w3
gen contrato_2_6mes=eaf_trcp1_2_1_w3
recode contrato_2_6mes (-7 -9=.)
tab contrato_2_6mes

tab eaf_trcp1_3_1_w3
gen contrato_3_6mes=eaf_trcp1_3_1_w3
recode contrato_3_6mes (-7 -9=.)
tab contrato_3_6mes

tab eaf_trcp1_4_1_w3
gen contrato_4_6mes=eaf_trcp1_4_1_w3
recode contrato_4_6mes (-7 -9=.)
tab contrato_4_6mes

* Cotiza: trabajo remunerado
tab eaf_trcp1_1_2_w3
gen cotiza_1_6mes=eaf_trcp1_1_2_w3
recode cotiza_1_6mes (-7 -8 -9=.)
tab cotiza_1_6mes

tab eaf_trcp1_2_2_w3
gen cotiza_2_6mes=eaf_trcp1_2_2_w3
recode cotiza_2_6mes (-7 -8 -9=.)
tab cotiza_2_6mes

tab eaf_trcp1_3_2_w3
gen cotiza_3_6mes=eaf_trcp1_3_2_w3
recode cotiza_3_6mes (-7 -8 -9=.)
tab cotiza_3_6mes

tab eaf_trcp1_4_2_w3
gen cotiza_4_6mes=eaf_trcp1_4_2_w3
recode cotiza_4_6mes (-7 -8 -9=.)
tab cotiza_4_6mes

* Formalidad: trabajo remunera
egen forma_1_6mes=rowtotal(contrato_1_6mes cotiza_1_6mes)
egen forma_2_6mes=rowtotal(contrato_2_6mes cotiza_2_6mes)
egen forma_3_6mes=rowtotal(contrato_3_6mes cotiza_3_6mes)
egen forma_4_6mes=rowtotal(contrato_4_6mes cotiza_4_6mes)
recode forma_1_6mes forma_2_6mes forma_3_6mes forma_4_6mes (1/2=1) (0=0)
replace forma_1_6mes=. if seis_meses==0
replace forma_2_6mes=. if seis_meses==0
replace forma_3_6mes=. if seis_meses==0
replace forma_4_6mes=. if seis_meses==0
replace forma_1_6mes=. if contrato_1_6mes==. & cotiza_1_6mes==.
replace forma_2_6mes=. if contrato_2_6mes==. & cotiza_2_6mes==.
replace forma_3_6mes=. if contrato_3_6mes==. & cotiza_3_6mes==.
replace forma_4_6mes=. if contrato_4_6mes==. & cotiza_4_6mes==.

* Formalidad: trabajo por cuenta propia
gen boleta_1_6mes=eaf_trcp2_1_1_w3 
recode boleta_1_6mes (-7 -8 -9=.)
tab boleta_1_6mes

gen boleta_2_6mes=eaf_trcp2_2_1_w3 
recode boleta_2_6mes (-7 -8 -9=.)
tab boleta_2_6mes

gen boleta_3_6mes=eaf_trcp2_3_1_w3 
recode boleta_3_6mes (-7 -8 -9=.)
tab boleta_3_6mes

gen boleta_4_6mes=eaf_trcp2_4_1_w3 
recode boleta_4_6mes (-7 -8 -9=.)
tab boleta_4_6mes

* Tipo de trabajo 6 meses - para otros
gen tipo_trabajo_po_1_6mes=tipotrabajo_po1_6mes*10+forma_1_6mes
gen tipo_trabajo_po_2_6mes=tipotrabajo_po2_6mes*10+forma_2_6mes
gen tipo_trabajo_po_3_6mes=tipotrabajo_po3_6mes*10+forma_3_6mes
lab val tipo_trabajo_po_1_6mes tipo_trabajo_po_2_6mes tipo_trabajo_po_3_6mes tip_trab
tab1 tipo_trabajo_po_1_6mes tipo_trabajo_po_2_6mes tipo_trabajo_po_3_6mes

* Tipo de trabajo 6 meses - cuenta propia
gen tipo_trabajo_cp_1_6mes=tipotrabajo_cp1_6mes*10+boleta_1_6mes
gen tipo_trabajo_cp_2_6mes=tipotrabajo_cp2_6mes*10+boleta_2_6mes
gen tipo_trabajo_cp_3_6mes=tipotrabajo_cp3_6mes*10+boleta_3_6mes
lab val tipo_trabajo_cp_1_6mes tipo_trabajo_cp_2_6mes tipo_trabajo_cp_3_6mes  tip_trab
tab1 tipo_trabajo_cp_1_6mes tipo_trabajo_cp_2_6mes tipo_trabajo_cp_3_6mes 

************************************************
* Ola 4, tipo de trabajo
tab1 tipotrabajo_po1_12mes tipotrabajo_po2_12mes tipotrabajo_po3_12mes tipotrabajo_cp1_12mes tipotrabajo_cp2_12mes tipotrabajo_cp3_12mes
codebook tipotrabajo_po1_12mes tipotrabajo_po2_12mes tipotrabajo_po3_12mes tipotrabajo_cp1_12mes tipotrabajo_cp2_12mes tipotrabajo_cp3_12mes
recode tipotrabajo_po1_12mes tipotrabajo_po2_12mes tipotrabajo_po3_12mes (1=1) (0=0)
recode tipotrabajo_cp1_12mes tipotrabajo_cp2_12mes tipotrabajo_cp3_12mes (0=1) (1=0)

* Ola 4, formalidad
* Contrato: trabajo remunerado
tab eaf_trcp1_1_1_w4
gen contrato_1_12mes=eaf_trcp1_1_1_w4
recode contrato_1_12mes (-7 -8 -9=.)
tab contrato_1_12mes

tab eaf_trcp1_2_1_w4
gen contrato_2_12mes=eaf_trcp1_2_1_w4
recode contrato_2_12mes (-7 -8 -9=.)
tab contrato_2_12mes

tab eaf_trcp1_3_1_w4
gen contrato_3_12mes=eaf_trcp1_3_1_w4
recode contrato_3_12mes (-7 -8 -9=.)
tab contrato_3_12mes

tab eaf_trcp1_4_1_w4
gen contrato_4_12mes=eaf_trcp1_4_1_w4
recode contrato_4_12mes (-7 -8 -9=.)
tab contrato_4_12mes

* Cotiza: trabajo remunerado
tab eaf_trcp1_1_2_w4
gen cotiza_1_12mes=eaf_trcp1_1_2_w4
recode cotiza_1_12mes (-7 -8 -9=.)
tab cotiza_1_12mes

tab eaf_trcp1_2_2_w4
gen cotiza_2_12mes=eaf_trcp1_2_2_w4
recode cotiza_2_12mes (-7 -8 -9=.)
tab cotiza_2_12mes

tab eaf_trcp1_3_2_w4
gen cotiza_3_12mes=eaf_trcp1_3_2_w4
recode cotiza_3_12mes (-7 -8 -9=.)
tab cotiza_3_12mes

tab eaf_trcp1_4_2_w4
gen cotiza_4_12mes=eaf_trcp1_4_2_w4
recode cotiza_4_12mes (-7 -8 -9=.)
tab cotiza_4_12mes

* Formalidad: trabajo remunera
egen forma_1_12mes=rowtotal(contrato_1_12mes cotiza_1_12mes)
egen forma_2_12mes=rowtotal(contrato_2_12mes cotiza_2_12mes)
egen forma_3_12mes=rowtotal(contrato_3_12mes cotiza_3_12mes)
egen forma_4_12mes=rowtotal(contrato_4_12mes cotiza_4_12mes)
recode forma_1_12mes forma_2_12mes forma_3_12mes forma_4_12mes (1/2=1) (0=0)
replace forma_1_12mes=. if doce_meses==0
replace forma_2_12mes=. if doce_meses==0
replace forma_3_12mes=. if doce_meses==0
replace forma_4_12mes=. if doce_meses==0
replace forma_1_12mes=. if contrato_1_12mes==. & cotiza_1_12mes==.
replace forma_2_12mes=. if contrato_2_12mes==. & cotiza_2_12mes==.
replace forma_3_12mes=. if contrato_3_12mes==. & cotiza_3_12mes==.
replace forma_4_12mes=. if contrato_4_12mes==. & cotiza_4_12mes==.

* Formalidad: trabajo por cuenta propia
gen boleta_1_12mes=eaf_trcp2_1_1_w4 
recode boleta_1_12mes (-7 -8 -9=.)
tab boleta_1_12mes

gen boleta_2_12mes=eaf_trcp2_2_1_w4 
recode boleta_2_12mes (-7 -8 -9=.)
tab boleta_2_12mes

gen boleta_3_12mes=eaf_trcp2_3_1_w4 
recode boleta_3_12mes (-7 -8 -9=.)
tab boleta_3_12mes

gen boleta_4_12mes=eaf_trcp2_4_1_w4 
recode boleta_4_12mes (-7 -8 -9=.)
tab boleta_4_12mes

* Tipo de trabajo 12 meses - para otros
gen tipo_trabajo_po_1_12mes=tipotrabajo_po1_12mes*10+forma_1_12mes
gen tipo_trabajo_po_2_12mes=tipotrabajo_po2_12mes*10+forma_2_12mes
gen tipo_trabajo_po_3_12mes=tipotrabajo_po3_12mes*10+forma_3_12mes
gen tipo_trabajo_po_4_12mes=tipotrabajo_po4_12mes*10+forma_4_12mes
lab val tipo_trabajo_po_1_12mes tipo_trabajo_po_2_12mes tipo_trabajo_po_3_12mes tipo_trabajo_po_4_12mes tip_trab 
tab1 tipo_trabajo_po_1_12mes tipo_trabajo_po_2_12mes tipo_trabajo_po_3_12mes tipo_trabajo_po_4_12mes

* Tipo de trabajo 12 meses - cuenta propia
gen tipo_trabajo_cp_1_12mes=tipotrabajo_cp1_12mes*10+boleta_1_12mes
gen tipo_trabajo_cp_2_12mes=tipotrabajo_cp2_12mes*10+boleta_2_12mes
gen tipo_trabajo_cp_3_12mes=tipotrabajo_cp3_12mes*10+boleta_3_12mes
lab val tipo_trabajo_cp_1_12mes tipo_trabajo_cp_2_12mes tipo_trabajo_cp_3_12mes  tip_trab
tab1 tipo_trabajo_cp_1_12mes tipo_trabajo_cp_2_12mes tipo_trabajo_cp_3_12mes 

/// % Mujeres que declaró más de 1 trabajo
gen mas_un_trab=1 if num_trab_2mes>=2 | num_trab_6mes>=2 | num_trab_12mes>=2 
replace mas_un_trab=0 if missing(mas_un_trab)

/// Merge with cluster membership
merge 1:1 reg_folio using cluster_membership.dta

/// Descriptive

// Education
gen educ_gr2=educ_gr3
recode educ_gr2 (0/1=0) (2=1)
lab def educ_gr2 0"Less than highschool" 1"Highschool or more"
lab val educ_gr2 educ_gr2
tab educ_gr2

// Work six months before prision
tab1 eaf_6_w0 eaf_43_w0

gen trab_6months_prior=1 if eaf_6_w0==1 | eaf_43_w0==1
replace trab_6months_prior=0 if missing(trab_6months_prior)

// Re-imprisioment during follow up
codebook del_69_w2 del_69_w3 del_69_w4
gen prison=1 if del_69_w2==1 | del_69_w3==1 | del_69_w4==1
replace prison=0 if missing(prison)
replace prison=. if del_69_w2==-9 & del_69_w3==-9 & del_69_w4==-9
tab prison

/// Jobs clusters
tabout age_gr educ_gr2 typeCrime del_49_w0 dep_abuse2 vivido_un_lugar eaf_14_w0 hijo_menor trab_6months_prior eaf_6_w0 eaf_43_w0 prison malestar_smental cluster_job_ind_4 using a.xls, replace cells(freq col) format(1p) stats(chi2)  
tabout age_gr educ_gr2 typeCrime del_49_w0 dep_abuse2 vivido_un_lugar eaf_14_w0 hijo_menor trab_6months_prior eaf_6_w0 eaf_43_w0 prison malestar_smental cluster_job_ind_4 using a.xls, replace cells(freq row) format(1p) stats(chi2)  
sort cluster_job_ind_4
by cluster_job_ind_4: sum ControlVida
sum ControlVida
anova ControlVida cluster_job_ind_4

by cluster_job_ind_4: sum DispCambio
sum DispCambio
anova DispCambio cluster_job_ind_4

by cluster_job_ind_4: sum hdv_1_w0
sum hdv_1_w0
anova hdv_1_w0 cluster_job_ind_4

/// Jobs crime-job
tabout age_gr educ_gr2 typeCrime del_49_w0 dep_abuse2 vivido_un_lugar eaf_14_w0 hijo_menor trab_6months_prior eaf_6_w0 eaf_43_w0 prison malestar_smental cluster_jobv2_4 using crosstab_jobs_crime.xls, replace cells(freq col) format(1p) stats(chi2)  
tabout age_gr educ_gr2 typeCrime del_49_w0 dep_abuse2 vivido_un_lugar eaf_14_w0 hijo_menor trab_6months_prior eaf_6_w0 eaf_43_w0 prison malestar_smental cluster_jobv2_4 using a.xls, replace cells(freq row) format(1p) stats(chi2)  

sort cluster_jobv2_4
by cluster_jobv2_4: sum ControlVida
sum ControlVida
anova ControlVida cluster_jobv2_4

by cluster_jobv2_4: sum DispCambio
sum DispCambio
anova DispCambio cluster_jobv2_4

by cluster_jobv2_4: sum hdv_1_w0
sum hdv_1_w0
anova hdv_1_w0 cluster_jobv2_4

/// Jornada según tipo de trabajo
tabout eaf_trcp1_1_3_w2 uno if 

eaf_trcp1_1_3_w3 eaf_trcp1_1_3_w4 
eaf_trcp1_2_3_w2 eaf_trcp1_2_3_w3 eaf_trcp1_2_3_w4 
eaf_trcp1_3_3_w2 eaf_trcp1_3_3_w3 eaf_trcp1_3_3_w4 
eaf_trcp1_4_3_w2 eaf_trcp1_4_3_w3 eaf_trcp1_4_3_w4 
eaf_trcp2_1_2_w2 eaf_trcp2_1_2_w3 eaf_trcp2_1_2_w4 
eaf_trcp2_2_2_w2 eaf_trcp2_2_2_w3 eaf_trcp2_2_2_w4 
eaf_trcp2_3_2_w2 eaf_trcp2_3_2_w3 eaf_trcp2_3_2_w4 
eaf_trcp2_4_2_w2 eaf_trcp2_4_2_w3 eaf_trcp2_4_2_w4



