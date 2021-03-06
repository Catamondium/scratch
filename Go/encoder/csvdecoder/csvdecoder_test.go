package csvdecoder

import (
	"bytes"
	"encoding/csv"
	"fmt"
	"io"
	"reflect"
	"strings"
	"testing"
)

type Simple struct {
	Name string
	Freq int
	Val  float64
	int  // anonymous field, ignored
}

func (s Simple) GoString() string {
	return fmt.Sprintf("Simple{%q, %d, %.2f, %d}", s.Name, s.Freq, s.Val, s.int)
}

// Return st&ardised option set
func genReader(r io.Reader) *csv.Reader {
	ret := csv.NewReader(r)
	ret.Comma = '|'
	ret.Comment = '#'
	return ret
}

// Return st&ardised option set
func genWriter(r io.Writer) *csv.Writer {
	ret := csv.NewWriter(r)
	ret.Comma = '|'
	return ret
}

func sliceEqual(t *testing.T, exp, rec interface{}) {
	t.Helper()
	if !reflect.DeepEqual(exp, rec) {
		t.Errorf("Expected: %#v\nRecieved: %#v", exp, rec)
	}
}

func assertEqual(t *testing.T, exp, rec interface{}) {
	t.Helper()
	if !reflect.DeepEqual(exp, rec) {
		t.Errorf("Expected: %#v Recieved: %#v", exp, rec)
	}
}

const badPanic = "Failed to panic"

func assertPanics(t *testing.T, f func()) {
	t.Helper()
	defer func() {
		if err := recover(); err != nil {
		}
	}()

	f()
	t.Errorf(badPanic)
}

func TestHeaderGen(t *testing.T) {
	t.Run("Simple struct, 3 primitve, 1 anon field", func(t *testing.T) {
		expected := Header{
			"Name",
			"Freq",
			"Val",
		}

		recieved := deriveHeader(Simple{})
		sliceEqual(t, expected, recieved)
	})

	t.Run("Panic on primative & ptr types", func(t *testing.T) {
		assertPanics(t, func() {
			deriveHeader(50)
		})

		assertPanics(t, func() {
			deriveHeader(&Simple{})
		})
	})
}

var (
	sampleSimple       = Simple{"Adam", 50, 1.00, 0}
	sampleSimpleRecord = []string{"Adam", "50", "1E+00"}
)

func TestRecordGen(t *testing.T) {
	t.Run("Simple record generation", func(t *testing.T) {
		source := sampleSimple
		expected := sampleSimpleRecord

		heading := deriveHeader(source)
		recieved := makeRecord(source, heading)

		sliceEqual(t, expected, recieved)
	})
}

func TestRecordsGen(t *testing.T) {
	t.Run("Simple records gen", func(t *testing.T) {
		source := []Simple{
			sampleSimple,
			sampleSimple,
			sampleSimple,
		}

		expected := [][]string{
			sampleSimpleRecord,
			sampleSimpleRecord,
			sampleSimpleRecord,
		}

		heading := deriveHeader(source[0])
		recieved := makeRecords(source, heading)

		sliceEqual(t, expected, recieved)
	})

	t.Run("Panic on non-slice", func(t *testing.T) {
		header := make([]Heading, 0)
		assertPanics(t, func() {
			makeRecords(Simple{}, header)
		})

		assertPanics(t, func() {
			makeRecords(&Simple{}, header)
		})
	})
}

func TestSimpleGen(t *testing.T) {
	t.Run("Single Simple decode", func(t *testing.T) {
		expected := sampleSimple
		header := []string{"Name", "Freq", "Val"}
		record := sampleSimpleRecord

		recieved := Simple{}
		makeStruct(&recieved, header, record)

		assertEqual(t, expected, recieved)
	})

	t.Run("Anonymous fields ignored", func(t *testing.T) {
		expected := Simple{}
		header := []string{"int"}
		record := []string{"50"}

		recieved := Simple{}
		makeStruct(&recieved, header, record)

		assertEqual(t, expected, recieved)
	})

	t.Run("Absent fields ignored", func(t *testing.T) {
		expected := Simple{}
		header := []string{"NoneField"}
		record := []string{"50"}

		recieved := Simple{}
		makeStruct(&recieved, header, record)

		assertEqual(t, expected, recieved)
	})
}

func TestSimplesGen(t *testing.T) {
	t.Run("Simples from mutiple records", func(t *testing.T) {
		expected := []Simple{
			sampleSimple,
			sampleSimple,
			sampleSimple,
		}
		header := []string{"Name", "Freq", "Val"}
		records := [][]string{
			sampleSimpleRecord,
			sampleSimpleRecord,
			sampleSimpleRecord,
		}

		recieved := make([]Simple, 0)
		makeStructs(&recieved, header, records)

		assertEqual(t, expected, recieved)
	})
}

const (
	realInt   int     = 50
	intString         = "50"
	realFlt   float64 = 1.00
	fltString         = "1E+00"
)

func TestToString(t *testing.T) {
	t.Run("valid types", func(t *testing.T) {
		assertEqual(t, "ABCD", toString("ABCD"))
		assertEqual(t, "int", toString(reflect.TypeOf(realInt)))

		assertEqual(t, "true", toString(true))

		assertEqual(t, intString, toString(realInt))
		assertEqual(t, intString, toString(int8(realInt)))
		assertEqual(t, intString, toString(int16(realInt)))
		assertEqual(t, intString, toString(int32(realInt)))
		assertEqual(t, intString, toString(int64(realInt)))

		assertEqual(t, intString, toString(uint(realInt)))
		assertEqual(t, intString, toString(uint8(realInt)))
		assertEqual(t, intString, toString(uint16(realInt)))
		assertEqual(t, intString, toString(uint32(realInt)))
		assertEqual(t, intString, toString(uint64(realInt)))

		assertEqual(t, fltString, toString(realFlt))
		assertEqual(t, fltString, toString(float32(realFlt)))

	})

	t.Run("panic on invalid type", func(t *testing.T) {
		assertPanics(t, func() {
			toString(Simple{})
		})
	})
}

func TestFromString(t *testing.T) {
	t.Run("Valid conversions", func(t *testing.T) {
		assertEqual(t, "ABCD", fromString("string{}", "ABCD").Interface())

		assertEqual(t, true, fromString(true, "true").Interface())

		assertEqual(t, realInt, fromString(realInt, intString).Interface())
		assertEqual(t, int8(realInt), fromString(int8(realInt), intString).Interface())
		assertEqual(t, int16(realInt), fromString(int16(realInt), intString).Interface())
		assertEqual(t, int32(realInt), fromString(int32(realInt), intString).Interface())
		assertEqual(t, int64(realInt), fromString(int64(realInt), intString).Interface())

		assertEqual(t, uint(realInt), fromString(uint(realInt), intString).Interface())
		assertEqual(t, uint8(realInt), fromString(uint8(realInt), intString).Interface())
		assertEqual(t, uint16(realInt), fromString(uint16(realInt), intString).Interface())
		assertEqual(t, uint32(realInt), fromString(uint32(realInt), intString).Interface())
		assertEqual(t, uint64(realInt), fromString(uint64(realInt), intString).Interface())

		assertEqual(t, realFlt, fromString(realFlt, fltString).Interface())
		assertEqual(t, float32(realFlt), fromString(float32(realFlt), fltString).Interface())
	})

	t.Run("Panic on parseError", func(t *testing.T) {
		assertPanics(t, func() {
			fromString(true, "100")
		})
	})

	t.Run("Panic on invalid type", func(t *testing.T) {
		assertPanics(t, func() {
			fromString(Simple{}, "ABCD")
		})
	})
}

const headedCSV = `Name|Freq|Val
Dan|11|1.111E+00
Jen|22|2.222E+00`

var headedRecords = []Simple{
	Simple{"Dan", 11, 1.111E+00, 0},
	Simple{"Jen", 22, 2.222E+00, 0},
}

func TestDecoder(t *testing.T) {
	t.Run("Prefer fileHeader, decode single", func(t *testing.T) {
		expected := headedRecords[0]
		recieved := Simple{}

		decoder, _ := NewDecoder(genReader(strings.NewReader(headedCSV)))
		decoder.One(&recieved)

		assertEqual(t, expected, recieved)
	})

	t.Run("Decode all success", func(t *testing.T) {
		expected := headedRecords
		recieved := make([]Simple, 0)

		decoder, _ := NewDecoder(genReader(strings.NewReader(headedCSV)))
		decoder.All(&recieved)

		assertEqual(t, expected, recieved)
	})

	t.Run("Err on empty", func(t *testing.T) {
		_, err := NewDecoder(genReader(strings.NewReader("")))

		if err == nil {
			t.Error("Failed to produce error")
		}
	})
}

func TestEncoder(t *testing.T) {
	t.Run("Prefer fields, encode single", func(t *testing.T) {
		recieved := bytes.NewBufferString("")
		encoder := NewEncoder(genWriter(recieved))
		encoder.One(Simple{"Dan", 11, 1.111E+00, 0})
		encoder.Csvwriter.Flush()

		sliceEqual(t, headedCSV[:31], recieved.String())
	})

	t.Run("Encode all", func(t *testing.T) {
		recieved := bytes.NewBufferString("")
		encoder := NewEncoder(genWriter(recieved))
		encoder.All(headedRecords)

		sliceEqual(t, headedCSV, strings.TrimSpace(recieved.String()))
	})
}
