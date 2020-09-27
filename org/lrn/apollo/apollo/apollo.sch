EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L apollo:5461AS-1 U2
U 1 1 5F3E9087
P 9550 4650
F 0 "U2" H 10580 4696 50  0000 L CNN
F 1 "5461AS-1" H 10580 4605 50  0000 L CNN
F 2 "apollo:5461AS-1" H 9500 4950 50  0001 C CNN
F 3 "http://www.kingbright.com/attachments/file/psearch/000/00/00/CC56-12SURKWA(Ver.7A).pdf" H 9120 4680 50  0001 C CNN
	1    9550 4650
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74HC595 U1
U 1 1 5F3E9A2E
P 7050 4550
F 0 "U1" H 7250 5250 50  0000 C CNN
F 1 "74HC595" H 7250 5150 50  0000 C CNN
F 2 "Package_DIP:DIP-16_W7.62mm" H 7050 4550 50  0001 C CNN
F 3 "http://www.ti.com/lit/ds/symlink/sn74hc595.pdf" H 7050 4550 50  0001 C CNN
	1    7050 4550
	1    0    0    -1  
$EndComp
$Comp
L Device:R R7
U 1 1 5F3F86B4
P 7950 4650
F 0 "R7" V 7950 4700 50  0000 C CNN
F 1 "R" V 7950 4600 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7880 4650 50  0001 C CNN
F 3 "~" H 7950 4650 50  0001 C CNN
	1    7950 4650
	0    1    1    0   
$EndComp
$Comp
L Device:R R5
U 1 1 5F3FC201
P 7950 4250
F 0 "R5" V 7950 4300 50  0000 C CNN
F 1 "R" V 7950 4200 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7880 4250 50  0001 C CNN
F 3 "~" H 7950 4250 50  0001 C CNN
	1    7950 4250
	0    1    1    0   
$EndComp
Wire Wire Line
	7450 4150 7550 4150
Wire Wire Line
	7850 4150 7950 4150
Wire Wire Line
	7950 4150 7950 3900
Wire Wire Line
	7950 3900 9300 3900
Wire Wire Line
	9300 3900 9300 4050
Wire Wire Line
	7450 4250 7800 4250
Wire Wire Line
	9900 3750 9900 4050
Wire Wire Line
	8100 4250 8200 4250
Wire Wire Line
	8200 4250 8200 3750
Wire Wire Line
	8200 3750 9900 3750
Wire Wire Line
	7450 4650 7800 4650
Wire Wire Line
	8100 4650 8350 4650
Wire Wire Line
	8350 4650 8350 3600
Wire Wire Line
	8350 3600 9450 3600
Wire Wire Line
	9450 3600 9450 4050
$Comp
L Device:R R2
U 1 1 5F4141F1
P 7700 4350
F 0 "R2" V 7700 4400 50  0000 C CNN
F 1 "R" V 7700 4300 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7630 4350 50  0001 C CNN
F 3 "~" H 7700 4350 50  0001 C CNN
	1    7700 4350
	0    1    1    0   
$EndComp
$Comp
L Device:R R1
U 1 1 5F3F3B44
P 7700 4150
F 0 "R1" V 7700 4200 50  0000 C CNN
F 1 "R" V 7700 4100 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7630 4150 50  0001 C CNN
F 3 "~" H 7700 4150 50  0001 C CNN
	1    7700 4150
	0    1    1    0   
$EndComp
Wire Wire Line
	7450 4350 7550 4350
Wire Wire Line
	9600 5350 9600 5250
Wire Wire Line
	7850 4350 8200 4350
Wire Wire Line
	9600 5350 8200 5350
Wire Wire Line
	8200 4350 8200 5350
$Comp
L Device:R R6
U 1 1 5F425E22
P 7950 4450
F 0 "R6" V 7950 4500 50  0000 C CNN
F 1 "R" V 7950 4400 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7880 4450 50  0001 C CNN
F 3 "~" H 7950 4450 50  0001 C CNN
	1    7950 4450
	0    1    1    0   
$EndComp
Wire Wire Line
	7450 4450 7800 4450
Wire Wire Line
	8100 4450 8300 4450
Wire Wire Line
	8300 4450 8300 5450
Wire Wire Line
	8300 5450 9300 5450
Wire Wire Line
	9300 5450 9300 5250
$Comp
L Device:R R3
U 1 1 5F42B82D
P 7700 4550
F 0 "R3" V 7700 4600 50  0000 C CNN
F 1 "R" V 7700 4500 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7630 4550 50  0001 C CNN
F 3 "~" H 7700 4550 50  0001 C CNN
	1    7700 4550
	0    1    1    0   
$EndComp
Wire Wire Line
	7450 4550 7550 4550
Wire Wire Line
	7850 4550 8400 4550
$Comp
L Device:R R4
U 1 1 5F43077A
P 7700 4750
F 0 "R4" V 7700 4800 50  0000 C CNN
F 1 "R" V 7700 4700 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7630 4750 50  0001 C CNN
F 3 "~" H 7700 4750 50  0001 C CNN
	1    7700 4750
	0    1    1    0   
$EndComp
Wire Wire Line
	7450 4750 7550 4750
Wire Wire Line
	7850 4750 8500 4750
$Comp
L Device:R R8
U 1 1 5F436A2A
P 7950 4850
F 0 "R8" V 7950 4900 50  0000 C CNN
F 1 "R" V 7950 4800 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7880 4850 50  0001 C CNN
F 3 "~" H 7950 4850 50  0001 C CNN
	1    7950 4850
	0    1    1    0   
$EndComp
Wire Wire Line
	7450 4850 7800 4850
Wire Wire Line
	8400 4550 8400 5550
Wire Wire Line
	8400 5550 9150 5550
Wire Wire Line
	9150 5550 9150 5250
Wire Wire Line
	8500 4750 8500 5650
Wire Wire Line
	8500 5650 9750 5650
Wire Wire Line
	9750 5650 9750 5250
Wire Wire Line
	8100 4850 8100 5750
Wire Wire Line
	8100 5750 9450 5750
Wire Wire Line
	9450 5250 9450 5750
$Comp
L Transistor_BJT:PN2222A Q2
U 1 1 5F447704
P 8750 1700
F 0 "Q2" H 8940 1746 50  0000 L CNN
F 1 "PN2222A" H 8940 1655 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 8950 1625 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 8750 1700 50  0001 L CNN
	1    8750 1700
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:PN2222A Q3
U 1 1 5F461F07
P 9100 2300
F 0 "Q3" H 9290 2346 50  0000 L CNN
F 1 "PN2222A" H 9290 2255 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 9300 2225 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 9100 2300 50  0001 L CNN
	1    9100 2300
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:PN2222A Q4
U 1 1 5F462FFD
P 9450 2800
F 0 "Q4" H 9640 2846 50  0000 L CNN
F 1 "PN2222A" H 9640 2755 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 9650 2725 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 9450 2800 50  0001 L CNN
	1    9450 2800
	1    0    0    -1  
$EndComp
Wire Wire Line
	9550 2600 9550 2450
Wire Wire Line
	9550 2450 9750 2450
Wire Wire Line
	9750 2450 9750 4050
Wire Wire Line
	9200 2100 9200 1950
Wire Wire Line
	9200 1950 10100 1950
Wire Wire Line
	10100 1950 10100 3600
Wire Wire Line
	10100 3600 9600 3600
Wire Wire Line
	9600 3600 9600 4050
Wire Wire Line
	8850 1500 8850 1350
Wire Wire Line
	8850 1350 10450 1350
Wire Wire Line
	10450 1350 10450 3450
Wire Wire Line
	9150 3450 10450 3450
$Comp
L Transistor_BJT:PN2222A Q1
U 1 1 5F497894
P 8400 1150
F 0 "Q1" H 8590 1196 50  0000 L CNN
F 1 "PN2222A" H 8590 1105 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 8600 1075 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 8400 1150 50  0001 L CNN
	1    8400 1150
	1    0    0    -1  
$EndComp
Wire Wire Line
	8500 950  8500 800 
Wire Wire Line
	8500 800  10850 800 
Wire Wire Line
	10850 800  10850 5350
Wire Wire Line
	10850 5350 9900 5350
Wire Wire Line
	9900 5350 9900 5250
Wire Wire Line
	9550 3000 9550 3200
Wire Wire Line
	7700 5500 7050 5500
Wire Wire Line
	7050 5500 7050 5400
Wire Wire Line
	9550 3200 7700 3200
Wire Wire Line
	9200 2500 9200 2650
Wire Wire Line
	8850 1900 8850 2050
Wire Wire Line
	8500 1350 8500 1500
Wire Wire Line
	7700 1500 7700 2050
Wire Wire Line
	7700 1500 8500 1500
Connection ~ 7700 3200
Wire Wire Line
	7700 2650 9200 2650
Connection ~ 7700 2650
Wire Wire Line
	7700 2650 7700 3200
Wire Wire Line
	7700 2050 8850 2050
Connection ~ 7700 2050
Wire Wire Line
	7700 2050 7700 2650
Wire Wire Line
	6650 4450 6500 4450
Wire Wire Line
	6500 4450 6500 3800
Wire Wire Line
	6500 3800 7050 3800
Wire Wire Line
	7050 3800 7050 3950
$Comp
L Connector:Conn_01x01_Male J_VCC1
U 1 1 5F4B5699
P 1000 1000
F 0 "J_VCC1" H 800 1000 50  0000 C CNN
F 1 "Conn_01x01_Male" H 950 900 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 1000 1000 50  0001 C CNN
F 3 "~" H 1000 1000 50  0001 C CNN
	1    1000 1000
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J_GND1
U 1 1 5F4C3172
P 1750 800
F 0 "J_GND1" H 1750 750 50  0000 C CNN
F 1 "Conn_01x01_Male" H 1850 650 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 1750 800 50  0001 C CNN
F 3 "~" H 1750 800 50  0001 C CNN
	1    1750 800 
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J3
U 1 1 5F4CD791
P 6100 2300
F 0 "J3" H 6100 2500 50  0000 C CNN
F 1 "Conn_01x01_Male" H 6100 2400 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 6100 2300 50  0001 C CNN
F 3 "~" H 6100 2300 50  0001 C CNN
	1    6100 2300
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J2
U 1 1 5F4D6C66
P 6100 1700
F 0 "J2" H 6100 1900 50  0000 C CNN
F 1 "Conn_01x01_Male" H 6100 1800 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 6100 1700 50  0001 C CNN
F 3 "~" H 6100 1700 50  0001 C CNN
	1    6100 1700
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J1
U 1 1 5F4DE0C6
P 6100 1150
F 0 "J1" H 6100 1350 50  0000 C CNN
F 1 "Conn_01x01_Male" H 6100 1250 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 6100 1150 50  0001 C CNN
F 3 "~" H 6100 1150 50  0001 C CNN
	1    6100 1150
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J_D_DATA_1
U 1 1 5F501F05
P 5500 4150
F 0 "J_D_DATA_1" H 5200 4150 50  0000 C CNN
F 1 "Conn_01x01_Male" H 4450 4150 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 5500 4150 50  0001 C CNN
F 3 "~" H 5500 4150 50  0001 C CNN
	1    5500 4150
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J4
U 1 1 5F4C78EA
P 6100 2800
F 0 "J4" H 6100 3000 50  0000 C CNN
F 1 "Conn_01x01_Male" H 6100 2900 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 6100 2800 50  0001 C CNN
F 3 "~" H 6100 2800 50  0001 C CNN
	1    6100 2800
	1    0    0    -1  
$EndComp
Wire Wire Line
	9150 3450 9150 4050
$Comp
L Device:R Rb1
U 1 1 5F5EB03E
P 7300 1150
F 0 "Rb1" V 7093 1150 50  0000 C CNN
F 1 "R" V 7184 1150 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7230 1150 50  0001 C CNN
F 3 "~" H 7300 1150 50  0001 C CNN
	1    7300 1150
	0    1    1    0   
$EndComp
$Comp
L Device:R Rb2
U 1 1 5F5EB7B3
P 7300 1700
F 0 "Rb2" V 7093 1700 50  0000 C CNN
F 1 "R" V 7184 1700 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7230 1700 50  0001 C CNN
F 3 "~" H 7300 1700 50  0001 C CNN
	1    7300 1700
	0    1    1    0   
$EndComp
$Comp
L Device:R Rb3
U 1 1 5F5EBD6A
P 7300 2300
F 0 "Rb3" V 7093 2300 50  0000 C CNN
F 1 "R" V 7184 2300 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7230 2300 50  0001 C CNN
F 3 "~" H 7300 2300 50  0001 C CNN
	1    7300 2300
	0    1    1    0   
$EndComp
$Comp
L Device:R Rb4
U 1 1 5F5EC2EE
P 7300 2800
F 0 "Rb4" V 7093 2800 50  0000 C CNN
F 1 "R" V 7184 2800 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 7230 2800 50  0001 C CNN
F 3 "~" H 7300 2800 50  0001 C CNN
	1    7300 2800
	0    1    1    0   
$EndComp
Wire Wire Line
	6300 2800 6750 2800
Wire Wire Line
	6300 2300 6750 2300
Wire Wire Line
	6300 1700 6750 1700
Wire Wire Line
	6300 1150 6750 1150
Wire Wire Line
	7450 1150 8200 1150
Wire Wire Line
	7450 1700 8550 1700
Wire Wire Line
	7450 2300 8900 2300
Wire Wire Line
	7450 2800 9250 2800
$Comp
L power:VCC #PWR0101
U 1 1 5F60A86A
P 1350 900
F 0 "#PWR0101" H 1350 750 50  0001 C CNN
F 1 "VCC" H 1367 1073 50  0000 C CNN
F 2 "" H 1350 900 50  0001 C CNN
F 3 "" H 1350 900 50  0001 C CNN
	1    1350 900 
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0102
U 1 1 5F60E32A
P 2300 950
F 0 "#PWR0102" H 2300 700 50  0001 C CNN
F 1 "GND" H 2305 777 50  0000 C CNN
F 2 "" H 2300 950 50  0001 C CNN
F 3 "" H 2300 950 50  0001 C CNN
	1    2300 950 
	1    0    0    -1  
$EndComp
Connection ~ 7050 5500
$Comp
L 74xx:74HC595 U3
U 1 1 5F575A47
P 3600 1900
F 0 "U3" H 3700 2600 50  0000 C CNN
F 1 "74HC595" H 3850 2500 50  0000 C CNN
F 2 "Package_DIP:DIP-16_W7.62mm_LongPads" H 3600 1900 50  0001 C CNN
F 3 "http://www.ti.com/lit/ds/symlink/sn74hc595.pdf" H 3600 1900 50  0001 C CNN
	1    3600 1900
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male JB_1
U 1 1 5F5880D1
P 4300 1600
F 0 "JB_1" H 4150 1600 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 1600 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 1600 50  0001 C CNN
F 3 "~" H 4300 1600 50  0001 C CNN
	1    4300 1600
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JA_15
U 1 1 5F580A07
P 4300 1500
F 0 "JA_15" H 4150 1500 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 1500 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 1500 50  0001 C CNN
F 3 "~" H 4300 1500 50  0001 C CNN
	1    4300 1500
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JC_2
U 1 1 5F58E8BB
P 4300 1700
F 0 "JC_2" H 4150 1700 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 1700 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 1700 50  0001 C CNN
F 3 "~" H 4300 1700 50  0001 C CNN
	1    4300 1700
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JD_3
U 1 1 5F5915FD
P 4300 1800
F 0 "JD_3" H 4150 1800 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 1800 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 1800 50  0001 C CNN
F 3 "~" H 4300 1800 50  0001 C CNN
	1    4300 1800
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JE_4
U 1 1 5F5944F8
P 4300 1900
F 0 "JE_4" H 4150 1900 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 1900 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 1900 50  0001 C CNN
F 3 "~" H 4300 1900 50  0001 C CNN
	1    4300 1900
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JF_5
U 1 1 5F5974B0
P 4300 2000
F 0 "JF_5" H 4150 2000 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 2000 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 2000 50  0001 C CNN
F 3 "~" H 4300 2000 50  0001 C CNN
	1    4300 2000
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JG_6
U 1 1 5F59A202
P 4300 2100
F 0 "JG_6" H 4150 2100 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 2100 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 2100 50  0001 C CNN
F 3 "~" H 4300 2100 50  0001 C CNN
	1    4300 2100
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JH_7
U 1 1 5F59CEC9
P 4300 2200
F 0 "JH_7" H 4150 2200 50  0000 C CNN
F 1 "Conn_01x01_Male" H 3650 2200 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4300 2200 50  0001 C CNN
F 3 "~" H 4300 2200 50  0001 C CNN
	1    4300 2200
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male J_K_DATA_1
U 1 1 5F5E2288
P 2250 1500
F 0 "J_K_DATA_1" H 1950 1500 50  0000 C CNN
F 1 "Conn_01x01_Male" H 1200 1500 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 2250 1500 50  0001 C CNN
F 3 "~" H 2250 1500 50  0001 C CNN
	1    2250 1500
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J_K_CLOCK_1
U 1 1 5F60047B
P 2250 1700
F 0 "J_K_CLOCK_1" H 1950 1700 50  0000 C CNN
F 1 "Conn_01x01_Male" H 1200 1700 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 2250 1700 50  0001 C CNN
F 3 "~" H 2250 1700 50  0001 C CNN
	1    2250 1700
	1    0    0    -1  
$EndComp
Wire Wire Line
	6650 4750 6350 4750
Wire Wire Line
	6350 4750 6350 5400
Wire Wire Line
	6350 5400 7050 5400
Connection ~ 7050 5400
Wire Wire Line
	7050 5400 7050 5250
$Comp
L Connector:Conn_01x01_Male J_D_LATCH_1
U 1 1 5F50B91C
P 5500 4950
F 0 "J_D_LATCH_1" H 5200 4950 50  0000 C CNN
F 1 "Conn_01x01_Male" H 4450 4950 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 5500 4950 50  0001 C CNN
F 3 "~" H 5500 4950 50  0001 C CNN
	1    5500 4950
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J_D_CLOCK_1
U 1 1 5F506A6B
P 5500 4350
F 0 "J_D_CLOCK_1" H 5200 4350 50  0000 C CNN
F 1 "Conn_01x01_Male" H 4450 4350 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 5500 4350 50  0001 C CNN
F 3 "~" H 5500 4350 50  0001 C CNN
	1    5500 4350
	1    0    0    -1  
$EndComp
Wire Wire Line
	6750 1800 6750 1700
Connection ~ 6750 1700
Wire Wire Line
	6750 1700 7150 1700
Wire Wire Line
	6750 2400 6750 2300
Connection ~ 6750 2300
Wire Wire Line
	6750 2300 7150 2300
Wire Wire Line
	6750 2800 6750 2900
Wire Wire Line
	6750 2900 5200 2900
Connection ~ 6750 2800
Wire Wire Line
	6750 2800 7150 2800
Wire Wire Line
	6750 1250 6750 1150
Connection ~ 6750 1150
Wire Wire Line
	6750 1150 7150 1150
$Comp
L Connector:Conn_01x01_Male J_SCK_1
U 1 1 5F6656CD
P 4450 3750
F 0 "J_SCK_1" H 4400 3750 50  0000 R CNN
F 1 "Conn_01x01_Male" H 4000 3750 50  0000 R CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4450 3750 50  0001 C CNN
F 3 "~" H 4450 3750 50  0001 C CNN
	1    4450 3750
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male J_MISO_1
U 1 1 5F66EA40
P 4450 3650
F 0 "J_MISO_1" H 4400 3650 50  0000 R CNN
F 1 "Conn_01x01_Male" H 4000 3650 50  0000 R CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4450 3650 50  0001 C CNN
F 3 "~" H 4450 3650 50  0001 C CNN
	1    4450 3650
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male J_MOSI_1
U 1 1 5F675F94
P 4450 3550
F 0 "J_MOSI_1" H 4400 3550 50  0000 R CNN
F 1 "Conn_01x01_Male" H 4000 3550 50  0000 R CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 4450 3550 50  0001 C CNN
F 3 "~" H 4450 3550 50  0001 C CNN
	1    4450 3550
	-1   0    0    1   
$EndComp
Wire Wire Line
	5100 4250 5100 2400
Wire Wire Line
	5100 2400 6750 2400
Wire Wire Line
	5000 4150 5000 1800
Wire Wire Line
	5000 1800 6750 1800
Wire Wire Line
	4900 1250 4900 4450
Wire Wire Line
	4900 1250 6750 1250
Wire Wire Line
	5200 2900 5200 4350
Wire Wire Line
	6150 4650 6650 4650
Wire Wire Line
	6000 4350 6650 4350
Wire Wire Line
	6000 4650 6000 4350
Wire Wire Line
	5850 4150 6650 4150
Wire Wire Line
	5850 4550 5850 4150
$Comp
L MCU_Microchip_ATmega:ATmega168P-20PU U4
U 1 1 5F6705AC
P 1350 4450
F 0 "U4" H 707 4496 50  0000 R CNN
F 1 "ATmega168P-20PU" H 707 4405 50  0000 R CNN
F 2 "Package_DIP:DIP-28_W7.62mm" H 1350 4450 50  0001 C CIN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-8025-8-bit-AVR-Microcontroller-ATmega48P-88P-168P_Datasheet.pdf" H 1350 4450 50  0001 C CNN
	1    1350 4450
	1    0    0    -1  
$EndComp
Wire Wire Line
	3600 2700 3600 2600
Wire Wire Line
	4250 3550 1950 3550
Wire Wire Line
	1950 3650 4250 3650
Wire Wire Line
	1950 3750 4250 3750
Wire Wire Line
	1950 4150 5000 4150
Wire Wire Line
	1950 4250 5100 4250
Wire Wire Line
	1950 4350 5200 4350
Wire Wire Line
	1950 4450 4900 4450
Wire Wire Line
	1950 4550 5850 4550
Wire Wire Line
	1950 4650 6000 4650
Wire Wire Line
	2450 1700 2900 1700
Wire Wire Line
	3200 2100 3200 2700
Wire Wire Line
	3200 2700 3600 2700
Wire Wire Line
	2450 2000 3000 2000
Connection ~ 2900 1700
Wire Wire Line
	2900 1700 3200 1700
Wire Wire Line
	2450 1500 2800 1500
Connection ~ 3000 2000
Connection ~ 2800 1500
Wire Wire Line
	2800 1500 3200 1500
Wire Wire Line
	1350 5950 1650 5950
Wire Wire Line
	5700 5950 5700 5500
Wire Wire Line
	5700 5500 7050 5500
Wire Wire Line
	1200 1000 1350 1000
Wire Wire Line
	1350 1000 1350 1250
Wire Wire Line
	1350 900  1350 1000
Connection ~ 1350 1000
Connection ~ 7700 1500
Wire Wire Line
	7700 3200 7700 3600
Wire Wire Line
	1350 2650 6500 3800
Connection ~ 1350 2650
Wire Wire Line
	1350 2650 1350 2950
Connection ~ 6500 3800
Wire Wire Line
	1950 800  2300 800 
Wire Wire Line
	7700 800  7700 1500
Wire Wire Line
	2300 950  2300 800 
Connection ~ 2300 800 
Wire Wire Line
	2300 800  7700 800 
Wire Wire Line
	3600 2700 7700 3600
Connection ~ 3600 2700
Connection ~ 7700 3600
Wire Wire Line
	7700 3600 7700 5500
Wire Wire Line
	4000 1500 4100 1500
Wire Wire Line
	4000 1600 4100 1600
Wire Wire Line
	4000 1700 4100 1700
Wire Wire Line
	4000 1800 4100 1800
Wire Wire Line
	4000 1900 4100 1900
Wire Wire Line
	4000 2000 4100 2000
Wire Wire Line
	4000 2100 4100 2100
Wire Wire Line
	4000 2200 4100 2200
$Comp
L Connector:Conn_01x01_Male J_K_LATCH_1
U 1 1 5F5F78E6
P 2250 2000
F 0 "J_K_LATCH_1" H 1950 2000 50  0000 C CNN
F 1 "Conn_01x01_Male" H 1200 2000 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 2250 2000 50  0001 C CNN
F 3 "~" H 2250 2000 50  0001 C CNN
	1    2250 2000
	1    0    0    -1  
$EndComp
Wire Wire Line
	5700 4350 6000 4350
Connection ~ 6000 4350
Wire Wire Line
	5700 4150 5850 4150
Connection ~ 5850 4150
Wire Wire Line
	5700 4950 6150 4950
Wire Wire Line
	6150 3450 6150 4650
Connection ~ 6150 4650
Wire Wire Line
	1950 3450 6150 3450
Wire Wire Line
	6150 4650 6150 4950
Wire Wire Line
	3600 1300 3600 1250
Connection ~ 1350 1250
Wire Wire Line
	1350 1250 1350 2650
Wire Wire Line
	1350 1250 3100 1250
Wire Wire Line
	3000 2000 3200 2000
Wire Wire Line
	3100 1800 3100 1250
Wire Wire Line
	3100 1800 3200 1800
Connection ~ 3100 1250
Wire Wire Line
	3100 1250 3600 1250
Wire Wire Line
	1950 3350 2900 3350
Wire Wire Line
	2900 1700 2900 3350
Wire Wire Line
	1950 3250 3000 3250
Wire Wire Line
	3000 3250 3000 2000
Wire Wire Line
	1950 5650 2800 5650
Wire Wire Line
	2800 1500 2800 5650
$Comp
L Device:Buzzer BZ1
U 1 1 5F6E5FBC
P 3500 6400
F 0 "BZ1" H 3652 6429 50  0000 L CNN
F 1 "Buzzer" H 3652 6338 50  0000 L CNN
F 2 "Buzzer_Beeper:Buzzer_12x9.5RM7.6" V 3475 6500 50  0001 C CNN
F 3 "~" V 3475 6500 50  0001 C CNN
	1    3500 6400
	0    1    1    0   
$EndComp
$Comp
L Device:LED D_RED_1
U 1 1 5F70CBF3
P 2100 6300
F 0 "D_RED_1" H 2093 6516 50  0000 C CNN
F 1 "LED" H 2093 6425 50  0000 C CNN
F 2 "LED_THT:LED_D5.0mm_FlatTop" H 2100 6300 50  0001 C CNN
F 3 "~" H 2100 6300 50  0001 C CNN
	1    2100 6300
	1    0    0    -1  
$EndComp
Wire Wire Line
	1950 6300 1650 6300
Wire Wire Line
	1650 6300 1650 5950
Connection ~ 1650 5950
Wire Wire Line
	1650 5950 3600 5950
$Comp
L Device:R R_RED_1
U 1 1 5F710263
P 2650 6300
F 0 "R_RED_1" V 2443 6300 50  0000 C CNN
F 1 "R" V 2534 6300 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 2580 6300 50  0001 C CNN
F 3 "~" H 2650 6300 50  0001 C CNN
	1    2650 6300
	0    1    1    0   
$EndComp
Wire Wire Line
	3400 5550 3400 6300
Wire Wire Line
	1950 5550 3400 5550
Wire Wire Line
	3400 6300 2800 6300
Connection ~ 3400 6300
Wire Wire Line
	2250 6300 2500 6300
Wire Wire Line
	3600 6300 3600 5950
Connection ~ 3600 5950
Wire Wire Line
	3600 5950 5700 5950
$EndSCHEMATC
