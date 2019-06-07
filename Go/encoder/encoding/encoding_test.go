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

func TestHeaderGen(t *testing.T) {
	t.Run("Simple struct, 3 primitve, 1 anon field", func(t *testing.T) {
		expected := []Header{
			{"Name", reflect.TypeOf("string()")},
			{"Freq", reflect.TypeOf(int(0))},
			{"Val", reflect.TypeOf(float64(0))},
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
