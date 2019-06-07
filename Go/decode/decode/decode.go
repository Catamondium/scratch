package decoder

import (
	"fmt"
	"reflect"
)

// Header information, reflected
type Header struct {
	name string
	typ  reflect.Type
}

// DeriveHeaders derive header information
// from named public fields
func DeriveHeaders(v interface{}) []Header {
	out := make([]Header, 0)
	val := reflect.ValueOf(v)
	if val.Kind() != reflect.Struct {
		panic(fmt.Sprintf("Invalid kind %v", val.Kind()))
	}

	for i := 0; i < val.NumField(); i++ {
		f := val.Type().Field(i)
		if !f.Anonymous {
			out = append(out, Header{f.Name, f.Type})
		}
	}

	return out
}
