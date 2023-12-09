package main

import (
	"fmt"
	"utils"
)

func isAllZeros(seq []int) bool {
	for _, v := range seq {
		if v != 0 {
			return false
		}
	}
	return true
}

func getDiffSequences(seq []int) [][]int {
	diffSeqs := [][]int{seq}
	for !isAllZeros(diffSeqs[len(diffSeqs)-1]) {
		prevDiffSeq := diffSeqs[len(diffSeqs)-1]
		currDiffSeq := make([]int, len(prevDiffSeq)-1)
		for i:= 0; i < len(currDiffSeq); i++ {
			currDiffSeq[i] = prevDiffSeq[i+1] - prevDiffSeq[i]
		}
		diffSeqs = append(diffSeqs, currDiffSeq)
	}
	return diffSeqs
}

func extrapolateNextSeqValue(seqs [][]int) int {

	nextVal := 0
	for i := len(seqs)-2; i >= 0; i-- {
		currSeq := seqs[i]
		nextVal += currSeq[len(currSeq)-1]
	}
	return nextVal
}

func extrapolatePreviousSeqValue(seqs [][]int) int {
	fmt.Println("Extrapolating for sequence:")
	for _, seq := range seqs {
		fmt.Println(seq)
	}
	prevVal := 0
	fmt.Println(prevVal)
	for i := len(seqs)-2; i >= 0; i-- {
		currSeq := seqs[i]
		prevVal = currSeq[0] - prevVal
		fmt.Println(prevVal)
	}
	return prevVal
}

func sol1() int {
	lineChan := make(chan string)

	go utils.ReadInputLines("input.txt", lineChan)

	sum := 0

	for line := range lineChan {
		seq := utils.GetNumeralsFromString(line)
		diffSeqs := getDiffSequences(seq)
		sum += extrapolateNextSeqValue(diffSeqs)
	}

	return sum
}

func sol2() int {
	lineChan := make(chan string)

	go utils.ReadInputLines("input.txt", lineChan)

	sum := 0

	for line := range lineChan {
		seq := utils.GetNumeralsFromString(line)
		diffSeqs := getDiffSequences(seq)
		sum += extrapolatePreviousSeqValue(diffSeqs)
	}

	return sum
}

func main() {
	fmt.Println(sol2())
}