package encoding

import (
	"encoding/csv"
	"io"
	"reflect"
	"testing"
)

type Simple struct {
	Name string
	Freq int
	Val  float64
	int  // anonymous field, ignored
}

const simpleSample = `
Name|Freq|Val # REQUIRED
Dan|5|55
Jen|8|9.5`

// Return standardised option set
func genReader(r io.Reader) *csv.Reader {
	ret := csv.NewReader(r)
	ret.Comma = '|'
	ret.Comment = '#'
	return ret
}

// Return standardised option set
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
			{"Name", reflect.TypeOf("string()")},
			{"Freq", reflect.TypeOf(int(0))},
			{"Val", reflect.TypeOf(float64(0))},
		}

		recieved := DeriveHeader(Simple{})
		sliceEqual(t, expected, recieved)
	})

	t.Run("Panic on int & ptr types", func(t *testing.T) {
		assertPanics(t, func() {
			DeriveHeader(50)
		})

		assertPanics(t, func() {
			addr := &Simple{}
			DeriveHeader(addr)
		})
	})
}

func TestRecordGen(t *testing.T) {
	t.Run("Simple record generation", func(t *testing.T) {
		source := Simple{"Adam", 50, 1.00, 0}
		expected := []string{"Adam", intResult, floatResult}

		heading := DeriveHeader(source)
		recieved := MakeRecord(source, heading)

		sliceEqual(t, expected, recieved)
	})
}

const testInt = 50
const intResult = "50"
const testFloat = 1.00
const floatResult = "1E+00"

func TestToString(t *testing.T) {
	t.Run("valid types", func(t *testing.T) {
		assertEqual(t, "ABCD", toString("ABCD"))

		assertEqual(t, "true", toString(true))

		assertEqual(t, intResult, toString(testInt))
		assertEqual(t, intResult, toString(int8(testInt)))
		assertEqual(t, intResult, toString(int16(testInt)))
		assertEqual(t, intResult, toString(int32(testInt)))
		assertEqual(t, intResult, toString(int64(testInt)))

		assertEqual(t, intResult, toString(uint(testInt)))
		assertEqual(t, intResult, toString(uint8(testInt)))
		assertEqual(t, intResult, toString(uint16(testInt)))
		assertEqual(t, intResult, toString(uint32(testInt)))
		assertEqual(t, intResult, toString(uint64(testInt)))

		assertEqual(t, floatResult, toString(1.00))
		assertEqual(t, floatResult, toString(float32(1.00)))

		assertEqual(t, "int", toString(reflect.TypeOf(55)))
	})

	t.Run("panic on invalid type", func(t *testing.T) {
		assertPanics(t, func() {
			toString(Simple{})
		})
	})
}
