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
P 6950 13700
F 0 "U2" H 7980 13746 50  0000 L CNN
F 1 "5461AS-1" H 7980 13655 50  0000 L CNN
F 2 "apollo:5461AS-1" H 6900 14000 50  0001 C CNN
F 3 "http://www.kingbright.com/attachments/file/psearch/000/00/00/CC56-12SURKWA(Ver.7A).pdf" H 6520 13730 50  0001 C CNN
	1    6950 13700
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74HC595 U1
U 1 1 5F3E9A2E
P 4450 13600
F 0 "U1" H 4650 14300 50  0000 C CNN
F 1 "74HC595" H 4650 14200 50  0000 C CNN
F 2 "Package_DIP:DIP-16_W7.62mm" H 4450 13600 50  0001 C CNN
F 3 "http://www.ti.com/lit/ds/symlink/sn74hc595.pdf" H 4450 13600 50  0001 C CNN
	1    4450 13600
	1    0    0    -1  
$EndComp
$Comp
L Device:R R7
U 1 1 5F3F86B4
P 5350 13700
F 0 "R7" V 5350 13750 50  0000 C CNN
F 1 "R" V 5350 13650 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5280 13700 50  0001 C CNN
F 3 "~" H 5350 13700 50  0001 C CNN
	1    5350 13700
	0    1    1    0   
$EndComp
$Comp
L Device:R R5
U 1 1 5F3FC201
P 5350 13300
F 0 "R5" V 5350 13350 50  0000 C CNN
F 1 "R" V 5350 13250 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5280 13300 50  0001 C CNN
F 3 "~" H 5350 13300 50  0001 C CNN
	1    5350 13300
	0    1    1    0   
$EndComp
Wire Wire Line
	4850 13200 4950 13200
Wire Wire Line
	5250 13200 5350 13200
Wire Wire Line
	5350 13200 5350 12950
Wire Wire Line
	5350 12950 6700 12950
Wire Wire Line
	6700 12950 6700 13100
Wire Wire Line
	4850 13300 5200 13300
Wire Wire Line
	7300 12800 7300 13100
Wire Wire Line
	5500 13300 5600 13300
Wire Wire Line
	5600 13300 5600 12800
Wire Wire Line
	5600 12800 7300 12800
Wire Wire Line
	4850 13700 5200 13700
Wire Wire Line
	5500 13700 5750 13700
Wire Wire Line
	5750 13700 5750 12650
Wire Wire Line
	5750 12650 6850 12650
Wire Wire Line
	6850 12650 6850 13100
$Comp
L Device:R R2
U 1 1 5F4141F1
P 5100 13400
F 0 "R2" V 5100 13450 50  0000 C CNN
F 1 "R" V 5100 13350 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5030 13400 50  0001 C CNN
F 3 "~" H 5100 13400 50  0001 C CNN
	1    5100 13400
	0    1    1    0   
$EndComp
$Comp
L Device:R R1
U 1 1 5F3F3B44
P 5100 13200
F 0 "R1" V 5100 13250 50  0000 C CNN
F 1 "R" V 5100 13150 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5030 13200 50  0001 C CNN
F 3 "~" H 5100 13200 50  0001 C CNN
	1    5100 13200
	0    1    1    0   
$EndComp
Wire Wire Line
	4850 13400 4950 13400
Wire Wire Line
	7000 14400 7000 14300
Wire Wire Line
	5250 13400 5600 13400
Wire Wire Line
	7000 14400 5600 14400
Wire Wire Line
	5600 13400 5600 14400
$Comp
L Device:R R6
U 1 1 5F425E22
P 5350 13500
F 0 "R6" V 5350 13550 50  0000 C CNN
F 1 "R" V 5350 13450 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5280 13500 50  0001 C CNN
F 3 "~" H 5350 13500 50  0001 C CNN
	1    5350 13500
	0    1    1    0   
$EndComp
Wire Wire Line
	4850 13500 5200 13500
Wire Wire Line
	5500 13500 5700 13500
Wire Wire Line
	5700 13500 5700 14500
Wire Wire Line
	5700 14500 6700 14500
Wire Wire Line
	6700 14500 6700 14300
$Comp
L Device:R R3
U 1 1 5F42B82D
P 5100 13600
F 0 "R3" V 5100 13650 50  0000 C CNN
F 1 "R" V 5100 13550 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5030 13600 50  0001 C CNN
F 3 "~" H 5100 13600 50  0001 C CNN
	1    5100 13600
	0    1    1    0   
$EndComp
Wire Wire Line
	4850 13600 4950 13600
Wire Wire Line
	5250 13600 5800 13600
$Comp
L Device:R R4
U 1 1 5F43077A
P 5100 13800
F 0 "R4" V 5100 13850 50  0000 C CNN
F 1 "R" V 5100 13750 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5030 13800 50  0001 C CNN
F 3 "~" H 5100 13800 50  0001 C CNN
	1    5100 13800
	0    1    1    0   
$EndComp
Wire Wire Line
	4850 13800 4950 13800
Wire Wire Line
	5250 13800 5900 13800
$Comp
L Device:R R8
U 1 1 5F436A2A
P 5350 13900
F 0 "R8" V 5350 13950 50  0000 C CNN
F 1 "R" V 5350 13850 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 5280 13900 50  0001 C CNN
F 3 "~" H 5350 13900 50  0001 C CNN
	1    5350 13900
	0    1    1    0   
$EndComp
Wire Wire Line
	4850 13900 5200 13900
Wire Wire Line
	5800 13600 5800 14600
Wire Wire Line
	5800 14600 6550 14600
Wire Wire Line
	6550 14600 6550 14300
Wire Wire Line
	5900 13800 5900 14700
Wire Wire Line
	5900 14700 7150 14700
Wire Wire Line
	7150 14700 7150 14300
Wire Wire Line
	5500 13900 5500 14800
Wire Wire Line
	5500 14800 6850 14800
Wire Wire Line
	6850 14300 6850 14800
$Comp
L Transistor_BJT:PN2222A Q2
U 1 1 5F447704
P 6150 10750
F 0 "Q2" H 6340 10796 50  0000 L CNN
F 1 "PN2222A" H 6340 10705 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 6350 10675 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 6150 10750 50  0001 L CNN
	1    6150 10750
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:PN2222A Q3
U 1 1 5F461F07
P 6500 11350
F 0 "Q3" H 6690 11396 50  0000 L CNN
F 1 "PN2222A" H 6690 11305 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 6700 11275 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 6500 11350 50  0001 L CNN
	1    6500 11350
	1    0    0    -1  
$EndComp
$Comp
L Transistor_BJT:PN2222A Q4
U 1 1 5F462FFD
P 6850 11850
F 0 "Q4" H 7040 11896 50  0000 L CNN
F 1 "PN2222A" H 7040 11805 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 7050 11775 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 6850 11850 50  0001 L CNN
	1    6850 11850
	1    0    0    -1  
$EndComp
Wire Wire Line
	6950 11650 6950 11500
Wire Wire Line
	6950 11500 7150 11500
Wire Wire Line
	7150 11500 7150 13100
Wire Wire Line
	6600 11150 6600 11000
Wire Wire Line
	6600 11000 7500 11000
Wire Wire Line
	7500 11000 7500 12650
Wire Wire Line
	7500 12650 7000 12650
Wire Wire Line
	7000 12650 7000 13100
Wire Wire Line
	6250 10550 6250 10400
Wire Wire Line
	6250 10400 7850 10400
Wire Wire Line
	7850 10400 7850 12500
Wire Wire Line
	6550 12500 7850 12500
$Comp
L Transistor_BJT:PN2222A Q1
U 1 1 5F497894
P 5800 10200
F 0 "Q1" H 5990 10246 50  0000 L CNN
F 1 "PN2222A" H 5990 10155 50  0000 L CNN
F 2 "apollo:2N222A-TO-92_Wide" H 6000 10125 50  0001 L CIN
F 3 "http://www.fairchildsemi.com/ds/PN/PN2222A.pdf" H 5800 10200 50  0001 L CNN
	1    5800 10200
	1    0    0    -1  
$EndComp
Wire Wire Line
	5900 10000 5900 9850
Wire Wire Line
	5900 9850 8250 9850
Wire Wire Line
	8250 9850 8250 14400
Wire Wire Line
	8250 14400 7300 14400
Wire Wire Line
	7300 14400 7300 14300
Wire Wire Line
	6950 12050 6950 12250
Wire Wire Line
	5100 12250 5100 12400
Wire Wire Line
	5100 14550 4450 14550
Wire Wire Line
	4450 14550 4450 14450
Wire Wire Line
	6950 12250 5100 12250
Wire Wire Line
	6600 11550 6600 11700
Wire Wire Line
	6250 10950 6250 11100
Wire Wire Line
	5900 10400 5900 10550
Wire Wire Line
	5100 10550 5100 11100
Wire Wire Line
	5100 10550 5900 10550
Connection ~ 5100 12250
Wire Wire Line
	5100 11700 6600 11700
Connection ~ 5100 11700
Wire Wire Line
	5100 11700 5100 12250
Wire Wire Line
	5100 11100 6250 11100
Connection ~ 5100 11100
Wire Wire Line
	5100 11100 5100 11700
Wire Wire Line
	4050 13500 3900 13500
Wire Wire Line
	3900 13500 3900 12850
Wire Wire Line
	3900 12850 4450 12850
Wire Wire Line
	4450 12850 4450 13000
$Comp
L Connector:Conn_01x01_Male J_VCC1
U 1 1 5F4B5699
P 3500 12850
F 0 "J_VCC1" H 3300 12850 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2450 12850 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3500 12850 50  0001 C CNN
F 3 "~" H 3500 12850 50  0001 C CNN
	1    3500 12850
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J_GND1
U 1 1 5F4C3172
P 3500 12400
F 0 "J_GND1" H 3300 12400 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2450 12350 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3500 12400 50  0001 C CNN
F 3 "~" H 3500 12400 50  0001 C CNN
	1    3500 12400
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J3
U 1 1 5F4CD791
P 3500 11350
F 0 "J3" H 3200 11350 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2450 11350 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3500 11350 50  0001 C CNN
F 3 "~" H 3500 11350 50  0001 C CNN
	1    3500 11350
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J2
U 1 1 5F4D6C66
P 3500 10750
F 0 "J2" H 3200 10750 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2450 10750 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3500 10750 50  0001 C CNN
F 3 "~" H 3500 10750 50  0001 C CNN
	1    3500 10750
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male J1
U 1 1 5F4DE0C6
P 3500 10200
F 0 "J1" H 3200 10200 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2450 10200 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3500 10200 50  0001 C CNN
F 3 "~" H 3500 10200 50  0001 C CNN
	1    3500 10200
	1    0    0    -1  
$EndComp
Connection ~ 5100 12400
Wire Wire Line
	5100 12400 5100 14550
Wire Wire Line
	3700 12400 5100 12400
Wire Wire Line
	3700 12850 3900 12850
Connection ~ 3900 12850
$Comp
L Connector:Conn_01x01_Male J_SER1
U 1 1 5F501F05
P 2750 15750
F 0 "J_SER1" H 2550 15750 50  0000 C CNN
F 1 "Conn_01x01_Male" H 1700 15750 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 2750 15750 50  0001 C CNN
F 3 "~" H 2750 15750 50  0001 C CNN
	1    2750 15750
	0    -1   -1   0   
$EndComp
$Comp
L Connector:Conn_01x01_Male J4
U 1 1 5F4C78EA
P 3500 11850
F 0 "J4" H 3200 11850 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2450 11800 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3500 11850 50  0001 C CNN
F 3 "~" H 3500 11850 50  0001 C CNN
	1    3500 11850
	1    0    0    -1  
$EndComp
Wire Wire Line
	6550 12500 6550 13100
$Comp
L Device:R Rb1
U 1 1 5F5EB03E
P 4700 10200
F 0 "Rb1" V 4493 10200 50  0000 C CNN
F 1 "R" V 4584 10200 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 4630 10200 50  0001 C CNN
F 3 "~" H 4700 10200 50  0001 C CNN
	1    4700 10200
	0    1    1    0   
$EndComp
$Comp
L Device:R Rb2
U 1 1 5F5EB7B3
P 4700 10750
F 0 "Rb2" V 4493 10750 50  0000 C CNN
F 1 "R" V 4584 10750 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 4630 10750 50  0001 C CNN
F 3 "~" H 4700 10750 50  0001 C CNN
	1    4700 10750
	0    1    1    0   
$EndComp
$Comp
L Device:R Rb3
U 1 1 5F5EBD6A
P 4700 11350
F 0 "Rb3" V 4493 11350 50  0000 C CNN
F 1 "R" V 4584 11350 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 4630 11350 50  0001 C CNN
F 3 "~" H 4700 11350 50  0001 C CNN
	1    4700 11350
	0    1    1    0   
$EndComp
$Comp
L Device:R Rb4
U 1 1 5F5EC2EE
P 4700 11850
F 0 "Rb4" V 4493 11850 50  0000 C CNN
F 1 "R" V 4584 11850 50  0000 C CNN
F 2 "apollo:R__Horizontal" V 4630 11850 50  0001 C CNN
F 3 "~" H 4700 11850 50  0001 C CNN
	1    4700 11850
	0    1    1    0   
$EndComp
Wire Wire Line
	3700 11850 4150 11850
Wire Wire Line
	3700 11350 4150 11350
Wire Wire Line
	3700 10750 4150 10750
Wire Wire Line
	3700 10200 4150 10200
Wire Wire Line
	4850 10200 5600 10200
Wire Wire Line
	4850 10750 5950 10750
Wire Wire Line
	4850 11350 6300 11350
Wire Wire Line
	4850 11850 6650 11850
$Comp
L power:VCC #PWR0101
U 1 1 5F60A86A
P 3900 12700
F 0 "#PWR0101" H 3900 12550 50  0001 C CNN
F 1 "VCC" H 3917 12873 50  0000 C CNN
F 2 "" H 3900 12700 50  0001 C CNN
F 3 "" H 3900 12700 50  0001 C CNN
	1    3900 12700
	1    0    0    -1  
$EndComp
Wire Wire Line
	3900 12700 3900 12850
$Comp
L power:GND #PWR0102
U 1 1 5F60E32A
P 4450 14750
F 0 "#PWR0102" H 4450 14500 50  0001 C CNN
F 1 "GND" H 4455 14577 50  0000 C CNN
F 2 "" H 4450 14750 50  0001 C CNN
F 3 "" H 4450 14750 50  0001 C CNN
	1    4450 14750
	1    0    0    -1  
$EndComp
Wire Wire Line
	4450 14750 4450 14550
Connection ~ 4450 14550
$Comp
L 74xx:74HC595 U3
U 1 1 5F575A47
P 10750 12000
F 0 "U3" H 10850 12700 50  0000 C CNN
F 1 "74HC595" H 11000 12600 50  0000 C CNN
F 2 "Package_DIP:DIP-16_W7.62mm_LongPads" H 10750 12000 50  0001 C CNN
F 3 "http://www.ti.com/lit/ds/symlink/sn74hc595.pdf" H 10750 12000 50  0001 C CNN
	1    10750 12000
	1    0    0    -1  
$EndComp
$Comp
L Connector:Conn_01x01_Male JB_1
U 1 1 5F5880D1
P 11800 11700
F 0 "JB_1" H 11500 11700 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 11700 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 11700 50  0001 C CNN
F 3 "~" H 11800 11700 50  0001 C CNN
	1    11800 11700
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JA_15
U 1 1 5F580A07
P 11800 11600
F 0 "JA_15" H 11500 11600 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 11600 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 11600 50  0001 C CNN
F 3 "~" H 11800 11600 50  0001 C CNN
	1    11800 11600
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JC_2
U 1 1 5F58E8BB
P 11800 11800
F 0 "JC_2" H 11500 11800 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 11800 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 11800 50  0001 C CNN
F 3 "~" H 11800 11800 50  0001 C CNN
	1    11800 11800
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JD_3
U 1 1 5F5915FD
P 11800 11900
F 0 "JD_3" H 11500 11900 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 11900 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 11900 50  0001 C CNN
F 3 "~" H 11800 11900 50  0001 C CNN
	1    11800 11900
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JE_4
U 1 1 5F5944F8
P 11800 12000
F 0 "JE_4" H 11500 12000 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 12000 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 12000 50  0001 C CNN
F 3 "~" H 11800 12000 50  0001 C CNN
	1    11800 12000
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JF_5
U 1 1 5F5974B0
P 11800 12100
F 0 "JF_5" H 11500 12100 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 12100 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 12100 50  0001 C CNN
F 3 "~" H 11800 12100 50  0001 C CNN
	1    11800 12100
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JG_6
U 1 1 5F59A202
P 11800 12200
F 0 "JG_6" H 11500 12200 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 12200 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 12200 50  0001 C CNN
F 3 "~" H 11800 12200 50  0001 C CNN
	1    11800 12200
	-1   0    0    1   
$EndComp
$Comp
L Connector:Conn_01x01_Male JH_7
U 1 1 5F59CEC9
P 11800 12300
F 0 "JH_7" H 11500 12300 50  0000 C CNN
F 1 "Conn_01x01_Male" H 10750 12300 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 11800 12300 50  0001 C CNN
F 3 "~" H 11800 12300 50  0001 C CNN
	1    11800 12300
	-1   0    0    1   
$EndComp
Wire Wire Line
	11150 11600 11600 11600
Wire Wire Line
	11150 11700 11600 11700
Wire Wire Line
	11150 11800 11600 11800
Wire Wire Line
	11150 11900 11600 11900
Wire Wire Line
	11150 12000 11600 12000
Wire Wire Line
	11150 12100 11600 12100
Wire Wire Line
	11150 12200 11600 12200
Wire Wire Line
	11150 12300 11600 12300
Wire Wire Line
	4450 14550 3750 14550
Wire Wire Line
	3750 14550 3750 15150
Wire Wire Line
	3750 15150 10750 15150
Wire Wire Line
	10750 15150 10750 12950
Wire Wire Line
	10350 12200 10100 12200
Wire Wire Line
	10100 12200 10100 12950
Wire Wire Line
	10100 12950 10750 12950
Connection ~ 10750 12950
Wire Wire Line
	10750 12950 10750 12700
Wire Wire Line
	3900 12700 1700 12700
Wire Wire Line
	1700 12700 1700 9500
Wire Wire Line
	1700 9500 10750 9500
Wire Wire Line
	10750 9500 10750 11100
Connection ~ 3900 12700
Wire Wire Line
	10350 11900 10100 11900
Wire Wire Line
	10100 11900 10100 11100
Wire Wire Line
	10100 11100 10750 11100
Connection ~ 10750 11100
Wire Wire Line
	10750 11100 10750 11400
$Comp
L Connector:Conn_01x01_Male JDS_14
U 1 1 5F5E2288
P 9650 11600
F 0 "JDS_14" H 9350 11600 50  0000 C CNN
F 1 "Conn_01x01_Male" H 8600 11600 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 9650 11600 50  0001 C CNN
F 3 "~" H 9650 11600 50  0001 C CNN
	1    9650 11600
	1    0    0    -1  
$EndComp
Wire Wire Line
	9850 11600 10350 11600
$Comp
L Connector:Conn_01x01_Male JST_CP_RCLK_12
U 1 1 5F5F78E6
P 9650 12100
F 0 "JST_CP_RCLK_12" H 9300 12100 50  0000 C CNN
F 1 "Conn_01x01_Male" H 8600 12100 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 9650 12100 50  0001 C CNN
F 3 "~" H 9650 12100 50  0001 C CNN
	1    9650 12100
	1    0    0    -1  
$EndComp
Wire Wire Line
	9850 12100 10350 12100
$Comp
L Connector:Conn_01x01_Male JSH_CP_SRCLK_11
U 1 1 5F60047B
P 9650 11800
F 0 "JSH_CP_SRCLK_11" H 9300 11800 50  0000 C CNN
F 1 "Conn_01x01_Male" H 8600 11800 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 9650 11800 50  0001 C CNN
F 3 "~" H 9650 11800 50  0001 C CNN
	1    9650 11800
	1    0    0    -1  
$EndComp
Wire Wire Line
	9850 11800 10350 11800
Wire Wire Line
	4050 13800 3750 13800
Wire Wire Line
	3750 13800 3750 14450
Wire Wire Line
	3750 14450 4450 14450
Connection ~ 4450 14450
Wire Wire Line
	4450 14450 4450 14300
$Comp
L Connector:Conn_01x01_Male J_CLK1
U 1 1 5F50B91C
P 3400 15750
F 0 "J_CLK1" H 3200 15750 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2350 15750 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3400 15750 50  0001 C CNN
F 3 "~" H 3400 15750 50  0001 C CNN
	1    3400 15750
	0    -1   -1   0   
$EndComp
Wire Wire Line
	3400 13700 3400 15550
Wire Wire Line
	3400 13700 4050 13700
$Comp
L Connector:Conn_01x01_Male J_CLR1
U 1 1 5F506A6B
P 3050 15750
F 0 "J_CLR1" H 2850 15750 50  0000 C CNN
F 1 "Conn_01x01_Male" H 2000 15750 50  0000 C CNN
F 2 "Connector_Pin:Pin_D0.7mm_L6.5mm_W1.8mm_FlatFork" H 3050 15750 50  0001 C CNN
F 3 "~" H 3050 15750 50  0001 C CNN
	1    3050 15750
	0    -1   -1   0   
$EndComp
$Comp
L MCU_Microchip_ATmega:ATmega168P-20PU U4
U 1 1 5F6705AC
P 250 13500
F 0 "U4" H -393 13546 50  0000 R CNN
F 1 "ATmega168P-20PU" H -393 13455 50  0000 R CNN
F 2 "Package_DIP:DIP-28_W7.62mm" H 250 13500 50  0001 C CIN
F 3 "http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-8025-8-bit-AVR-Microcontroller-ATmega48P-88P-168P_Datasheet.pdf" H 250 13500 50  0001 C CNN
	1    250  13500
	1    0    0    -1  
$EndComp
Wire Wire Line
	3400 13700 850  13700
Connection ~ 3400 13700
Wire Wire Line
	3050 13400 3050 13600
Wire Wire Line
	3050 13400 4050 13400
Wire Wire Line
	850  13600 3050 13600
Connection ~ 3050 13600
Wire Wire Line
	3050 13600 3050 15550
Wire Wire Line
	2750 13200 2750 13500
Wire Wire Line
	2750 13200 4050 13200
Wire Wire Line
	850  13500 2750 13500
Connection ~ 2750 13500
Wire Wire Line
	2750 13500 2750 15550
Wire Wire Line
	850  13400 1250 13400
Wire Wire Line
	1250 13400 1250 11050
Wire Wire Line
	1250 11050 4150 11050
Wire Wire Line
	4150 11050 4150 10750
Connection ~ 4150 10750
Wire Wire Line
	4150 10750 4550 10750
Wire Wire Line
	850  13300 1550 13300
Wire Wire Line
	1550 13300 1550 11600
Wire Wire Line
	1550 11600 4150 11600
Wire Wire Line
	4150 11600 4150 11350
Connection ~ 4150 11350
Wire Wire Line
	4150 11350 4550 11350
Wire Wire Line
	4150 11850 4150 12100
Wire Wire Line
	4150 12100 1100 12100
Wire Wire Line
	1100 12100 1100 13200
Wire Wire Line
	1100 13200 850  13200
Connection ~ 4150 11850
Wire Wire Line
	4150 11850 4550 11850
Wire Wire Line
	850  12400 950  12400
Wire Wire Line
	950  12400 950  10450
Wire Wire Line
	950  10450 4150 10450
Wire Wire Line
	4150 10450 4150 10200
Connection ~ 4150 10200
Wire Wire Line
	4150 10200 4550 10200
$EndSCHEMATC
