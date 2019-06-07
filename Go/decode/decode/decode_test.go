package decoder

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

// Return standardised option set
func genReader(r io.Reader) *csv.Reader {
	ret := csv.NewReader(r)
	ret.Comma = '|'
	ret.Comment = '#'
	return ret
}

func TestHeaderGen(t *testing.T) {

	t.Run("Simple struct, 3 primitve, 1 anon field", func(t *testing.T) {
		expected := []Header{
			{"Name", reflect.ValueOf(Simple{}).Type().Field(0).Type},
			{"Freq", reflect.ValueOf(Simple{}).Type().Field(1).Type},
			{"Val", reflect.ValueOf(Simple{}).Type().Field(2).Type},
		}

		recieved := DeriveHeaders(Simple{})

		if !reflect.DeepEqual(expected, recieved) {
			t.Errorf("Bad generation:\nExpected: %v\nRecieved: %v", expected, recieved)
		}
	})

	t.Run("Panic on primitive int", func(t *testing.T) {
		defer func() {
			if err := recover(); err != nil {
			}
		}()

		DeriveHeaders(50) // should test all primatives, optimally
		t.Errorf("Didn't panic")
	})
}
