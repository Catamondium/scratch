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

const badPanic = "Failed to panic"

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

func TestHeaderGen(t *testing.T) {
	t.Run("Simple struct, 3 primitve, 1 anon field", func(t *testing.T) {
		expected := []Heading{
			{"Name", reflect.TypeOf("string()")},
			{"Freq", reflect.TypeOf(int(0))},
			{"Val", reflect.TypeOf(float64(0))},
		}

		recieved := DeriveHeader(Simple{})

		if !reflect.DeepEqual(expected, recieved) {
			t.Errorf("Bad generation:\nExpected: %v\nRecieved: %v", expected, recieved)
		}
	})

	t.Run("Panic on primitive int", func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
			}
		}()

		DeriveHeader(50) // should test all primatives, optimally
		t.Errorf(badPanic)
	})
}

func TestRecordGen(t *testing.T) {
	t.Run("Simple record generation", func(t *testing.T) {
		source := Simple{"Adam", 50, 1.00, 0}
		expected := []string{"Adam", "50", "1E+00"}

		heading := DeriveHeader(source)
		recieved := MakeRecord(source, heading)

		sliceEqual(t, expected, recieved)
	})
}
func TestToString(t *testing.T) {
	t.Run("valid types", func(t *testing.T) {
		assertEqual(t, "ABCD", toString("ABCD"))

		assertEqual(t, "50", toString(50))
		assertEqual(t, "50", toString(int8(50)))
		assertEqual(t, "50", toString(int16(50)))
		assertEqual(t, "50", toString(int32(50)))
		assertEqual(t, "50", toString(int64(50)))

		assertEqual(t, "1E+00", toString(1.00))
		assertEqual(t, "1E+00", toString(float32(1.00)))
	})

	t.Run("panic on badKind", func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
			}
		}()

		toString(Simple{}) // should test all primatives, optimally
		t.Errorf(badPanic)
	})
}
