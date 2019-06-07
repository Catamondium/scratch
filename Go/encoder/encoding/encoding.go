package encoding

/* reflective csv encoder TODO
Decoder type
	newDecoder(r io.Reader) *Decoder
	embed csv.Reader?
	(dec *Decoder) Decode(vs interface{}) error // vs must be slice
Encoder type
	newEncoder(r io.Writer) *Encoder
	embed csv.Writer?
	(enc *Encoder) Encode(vs interface{}) error // vs must be *slice
*/

import (
	"fmt"
	"reflect"
	"strconv"
)

// Heading information, reflected
type Heading struct {
	Name string
	Type reflect.Type
}

// True for named, settable fields
func fieldReadable(v interface{}, f reflect.StructField) bool {
	return !f.Anonymous
}

// DeriveHeader derive header information
// from named public fields
func DeriveHeader(v interface{}) []Heading {
	out := make([]Heading, 0)
	val := reflect.ValueOf(v)
	if val.Kind() != reflect.Struct {
		panic(fmt.Sprintf("Invalid kind %v", val.Kind()))
	}

	for i := 0; i < val.NumField(); i++ {
		f := val.Type().Field(i)
		if fieldReadable(v, f) {
			out = append(out, Heading{f.Name, f.Type})
		}
	}
	return out
}

func toString(v interface{}) string {
	// complex number support?
	// Tag conversion specifiers?
	val := reflect.ValueOf(v)
	switch n := val.Type().Name(); n {
	case "string":
		return v.(string)

	case "int":
		return strconv.FormatInt(int64(v.(int)), 10)
	case "int8":
		return strconv.FormatInt(int64(v.(int8)), 10)
	case "int16":
		return strconv.FormatInt(int64(v.(int16)), 10)
	case "int32":
		return strconv.FormatInt(int64(v.(int32)), 10)
	case "int64":
		return strconv.FormatInt(v.(int64), 10)

	case "float32":
		return strconv.FormatFloat(float64(v.(float32)), 'E', -1, 32)
	case "float64":
		return strconv.FormatFloat(v.(float64), 'E', -1, 64)

	default:
		panic(fmt.Sprintf("Type '%s' cannot strconv to string", n))
	}
}

// MakeRecord create a **single** record from a struct and Headings
func MakeRecord(v interface{}, header []Heading) []string {
	val := reflect.ValueOf(v)
	out := make([]string, 0)

	for _, heading := range header {
		field := val.FieldByName(heading.Name)
		str := toString(field.Interface())
		out = append(out, str)
	}
	return out
}
