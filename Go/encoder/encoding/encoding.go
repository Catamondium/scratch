package encoding

/* reflective csv encoder TODO
Decoder type
	newDecoder(r io.Reader) *Decoder
	embed csv.Reader?
	(dec *Decoder) Decode(vs interface{}) error // vs must be slice
Encoder type
	newEncoder(r io.Writer) *Encoder
	embed csv.Writer?
	(enc *Encoder) Encode(vs interface{}) error // vs must be &slice
*/

import (
	"fmt"
	"reflect"
	"strconv"
)

// Heading information, reflected
type Heading struct {
	Name string
	Type reflect.Type // Not used yet, may be needed for decoding
}

// Header ordered list of Headings
type Header []Heading

// True for named, settable fields
func fieldReadable(v interface{}, f reflect.StructField) bool {
	return !f.Anonymous
}

// DeriveHeader derive header information
// from named public fields
// v is a concrete non-ptr type
func DeriveHeader(v interface{}) Header {
	out := make(Header, 0)
	val := reflect.ValueOf(v)
	if reflect.TypeOf(v).Kind() == reflect.Ptr {
		panic("Must be of value type")
	}

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
	// Tagged conversion specifiers?
	switch val := v.(type) {
	case string:
		return val

	case bool:
		return strconv.FormatBool(val)

	case float32:
		return strconv.FormatFloat(float64(val), 'E', -1, 32)
	case float64:
		return strconv.FormatFloat(val, 'E', -1, 64)

	case int:
		return strconv.FormatInt(int64(val), 10)
	case int8:
		return strconv.FormatInt(int64(val), 10)
	case int16:
		return strconv.FormatInt(int64(val), 10)
	case int32: // distinguish from rune by tag?
		return strconv.FormatInt(int64(val), 10)
	case int64:
		return strconv.FormatInt(val, 10)

	case uint:
		return strconv.FormatUint(uint64(val), 10)
	case uint8: // distinguish from byte by tag?
		return strconv.FormatUint(uint64(val), 10)
	case uint16:
		return strconv.FormatUint(uint64(val), 10)
	case uint32:
		return strconv.FormatUint(uint64(val), 10)
	case uint64:
		return strconv.FormatUint(val, 10)

	default:
		panic(fmt.Sprintf("Type '%s' cannot strconv to string", reflect.TypeOf(v).String()))
	}
}

// MakeRecord create a **single** record from a struct and header
func MakeRecord(v interface{}, header Header) []string {
	val := reflect.ValueOf(v)
	out := make([]string, 0)

	for _, heading := range header {
		field := val.FieldByName(heading.Name)
		str := toString(field.Interface())
		out = append(out, str)
	}
	return out
}
