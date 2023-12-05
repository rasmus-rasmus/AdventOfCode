package utils

import (
	"os"
	"bufio"
	"errors"
	"strings"
	"strconv"
)

func ReadInputLines(fileName string, ch chan string) error {
	f, err := os.Open(fileName)
	if err != nil {
		return errors.New("Could not open file")
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)

	for scanner.Scan() {
		line := scanner.Text()
		ch <- line
	}
	close(ch)
	return nil
}


func GetNumeralsFromString(s string) []int {
	numeralsOut := []int{}
	numberStrings := strings.Fields(s)
	for _, strNum := range numberStrings {
		numeral, _ := strconv.Atoi(strNum)
		numeralsOut = append(numeralsOut, numeral)
	}
	return numeralsOut
}