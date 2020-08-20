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
P 6100 3600
F 0 "U2" H 7130 3646 50  0000 L CNN
F 1 "5461AS-1" H 7130 3555 50  0000 L CNN
F 2 "apollo:5461AS-1" H 6050 3900 50  0001 C CNN
F 3 "http://www.kingbright.com/attachments/file/psearch/000/00/00/CC56-12SURKWA(Ver.7A).pdf" H 5670 3630 50  0001 C CNN
	1    6100 3600
	1    0    0    -1  
$EndComp
$Comp
L 74xx:74HC595 U1
U 1 1 5F3E9A2E
P 3600 3500
F 0 "U1" H 3600 4281 50  0000 C CNN
F 1 "74HC595" H 3600 4190 50  0000 C CNN
F 2 "Package_DIP:DIP-16_W7.62mm" H 3600 3500 50  0001 C CNN
F 3 "http://www.ti.com/lit/ds/symlink/sn74hc595.pdf" H 3600 3500 50  0001 C CNN
	1    3600 3500
	1    0    0    -1  
$EndComp
Wire Wire Line
	4000 3100 4100 3100
Wire Wire Line
	4100 3100 4100 2550
Wire Wire Line
	4100 2550 5850 2550
Wire Wire Line
	5850 2550 5850 3000
Wire Wire Line
	4000 3200 4150 3200
Wire Wire Line
	4150 3200 4150 2600
Wire Wire Line
	4150 2600 6450 2600
Wire Wire Line
	6450 2600 6450 3000
Wire Wire Line
	6150 4200 6150 4650
Wire Wire Line
	6150 4650 4150 4650
Wire Wire Line
	4150 4650 4150 3300
Wire Wire Line
	4150 3300 4000 3300
Wire Wire Line
	4000 3400 4200 3400
Wire Wire Line
	4200 3400 4200 4600
Wire Wire Line
	4200 4600 5850 4600
Wire Wire Line
	5850 4600 5850 4200
Wire Wire Line
	4000 3500 4250 3500
Wire Wire Line
	4250 3500 4250 4550
Wire Wire Line
	4250 4550 5700 4550
Wire Wire Line
	5700 4550 5700 4200
Wire Wire Line
	4000 3600 4300 3600
Wire Wire Line
	4300 3600 4300 2650
Wire Wire Line
	4300 2650 6000 2650
Wire Wire Line
	6000 2650 6000 3000
Wire Wire Line
	4000 3700 4300 3700
Wire Wire Line
	4300 3700 4300 4500
Wire Wire Line
	4300 4500 6300 4500
Wire Wire Line
	6300 4500 6300 4200
Wire Wire Line
	4000 3800 4350 3800
Wire Wire Line
	4350 3800 4350 4450
Wire Wire Line
	4350 4450 6000 4450
Wire Wire Line
	6000 4450 6000 4200
$EndSCHEMATC
