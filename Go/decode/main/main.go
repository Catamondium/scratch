package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strconv"
	"strings"
)

const source = "../example.tsv"

func check(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	f, err := os.Open(source)
	check(err)
	defer f.Close()

	reader := csv.NewReader(f)
	reader.Comma = '|'
	reader.Comment = '#'

	records, err := reader.ReadAll()
	check(err)
	for i, record := range records[1:] {
		for i, r := range record {
			record[i] = strings.TrimSpace(r)
		}

		reci, err := strconv.ParseInt(record[1], 0, 64)
		check(err)

		recf, err := strconv.ParseFloat(record[2], 64)
		check(err)

		fmt.Printf("%d: [%s, %d, %.1f]\n", i, record[0], reci, recf)
	}
}
