package main

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
	//"time"
)

func getNumbersFromFile(slice []byte) []int {

	var curNum int = 0
	var numsSlice []int

	for i := 0; i < len(slice); i++ {

		if slice[i] == 10 || slice[i] == 32 {
			if curNum != 0 {
				numsSlice = append(numsSlice, curNum)
				//fmt.Println(" curNum : ",  curNum)
				curNum = 0
			}
		} else {
			curN, errr := (strconv.Atoi(string(slice[i])))
			if errr != nil {
				fmt.Println(" errr : ", errr)
				os.Exit(1)
			}
			curNum = int(curN) + (curNum * 10)
		}
	}
	//fmt.Println("return getNumbersFromFile ")
	return numsSlice
}

func findSum(array []int, target int) {

	var arrLen = len(array)

	//fmt.Println("arrLen: ", len(array))

	file, err := os.Create("output.txt")

	if err != nil {
		fmt.Println("Unable to open file:", err)
		os.Exit(1)
	}

	// пока не кончится массив
	for i := 0; i < arrLen; i++ {
		// вычисляем второе слагаемое (первое текущий элемент массива)
		term := target - array[i]
		// ищем второе слагаемое в массиве
		indx := sort.Search(arrLen, func(indx int) bool { return array[indx] >= term })

		// если нашли
		if indx < arrLen && array[indx] == term && array[indx]+array[i] == target {
			// печатаем на выход 1 и выходим
			fmt.Fprintln(file, 1, "\n")
			// fmt.Println("sum :", term, array[indx], array[i], target)
			os.Exit(0)
		}
	}

	// иначе ничего не нашли, выходим
	fmt.Fprintln(file, 0, "\n")
}

func main() {
	//start := time.Now()
	file, err := os.Open("input.txt")
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	data := make([]byte, 100)
	var buf []byte

	for {
		n, er := file.Read(data)

		if er == io.EOF {
			break
		} else {
			for i := 0; i < n; i++ {
				buf = append(buf, data[i])
			}
		}
	}

	fmt.Printf("%s\n", buf)

	//  трансформируем строки в числа
	var allNumbersSlice []int = getNumbersFromFile(buf)
	// первое число в файле  всегда таргет
	var target = allNumbersSlice[0]
	//var target = 5678
	var numbers []int

	//fmt.Println("len allNumbers:", len(allNumbersSlice))

	//fmt.Println("target:", target)

	//убираем из массива все числабольше таргета
	for i := 1; i < len(allNumbersSlice); i++ {
		if allNumbersSlice[i] <= target {
			numbers = append(numbers, allNumbersSlice[i])
		}
	}

	// сортируем массив по возрастанию
	sort.Ints(numbers)

	// for i:=0; i < len(numbers); i++ {
	// 	fmt.Println("elt :", numbers[i], "i ", i)
	// }

	// fmt.Println("len numbers:", len(numbers))

	// ищем сумму
	findSum(numbers, target)

	//end := time.Now()
	// if indx >=0 {
	// 	fmt.Println("res:", numbers[indx])
	// }else{
	// 	fmt.Println("indx ", indx)
	// }

	//dur := end.Sub(start)
	//fmt.Println("time: ", dur)
}
