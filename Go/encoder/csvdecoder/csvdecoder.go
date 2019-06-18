package csvdecoder

/* reflective csv encoder TODO
 * * Decoder/encoder, embed csv reader/writer?
 * Encoder read in header for non-empty files
 * Propagate toString, fromString & reflective errors?
 */

import (
	"encoding/csv"
	"fmt"
	"reflect"
	"strconv"
)

// Heading information, reflected
type Heading = string

// Header ordered list of Headings
type Header = []Heading

// True for named, settable fields
func fieldReadable(v interface{}, f reflect.StructField) bool {
	return !f.Anonymous
}

// Decoder performs decoding via csv.Reader
type Decoder struct {
	Csvreader *csv.Reader
	header    Header
}

// Encoder performs encoding via csv.Writer
type Encoder struct {
	Csvwriter   *csv.Writer
	headWritten bool
}

// NewEncoder create new Encder from csv.Writer
func NewEncoder(r *csv.Writer) *Encoder {
	return &Encoder{r, false}
}

// One encodes struct v to csv writer
func (d *Encoder) One(v interface{}) error {
	header := deriveHeader(v)
	record := makeRecord(v, header)
	if !d.headWritten {
		d.Csvwriter.Write(header)
		d.headWritten = true
	}

	err := d.Csvwriter.Write(record)
	if err != nil {
		return err
	}

	return nil
}

// All encodes []struct v to csv writer
func (d *Encoder) All(v interface{}) error {
	header := deriveHeader(v)
	record := makeRecords(v, header)
	if !d.headWritten {
		d.Csvwriter.Write(header)
		d.headWritten = true
	}

	err := d.Csvwriter.WriteAll(record)
	if err != nil {
		return err
	}
	return nil
}

// NewDecoder create new Decoder from csv.Reader
func NewDecoder(r *csv.Reader) (*Decoder, error) {
	// Headers presence is ASSUMED
	header, err := r.Read()
	if err != nil {
		return nil, err
	}
	return &Decoder{r, header}, nil
}

// One decodes into struct pointed by v
// Reads a single record (after header)
func (d *Decoder) One(v interface{}) error {
	record, err := d.Csvreader.Read()
	if err != nil {
		return err
	}

	makeStruct(v, d.header, record)
	return nil
}

// All decodes into []struct pointed by v
// Reads all remaining records (after header)
func (d *Decoder) All(v interface{}) error {
	records, err := d.Csvreader.ReadAll()
	if err != nil {
		return err
	}

	makeStructs(v, d.header, records)
	return nil
}

// deriveHeader derive header information
// from named public fields
// v is a concrete non-ptr type
func deriveHeader(v interface{}) Header {
	out := make(Header, 0)
	val := reflect.ValueOf(v)
	typ := reflect.TypeOf(v)

	if typ.Kind() == reflect.Slice {
		val = reflect.New(typ.Elem()).Elem()
	}

	for i := 0; i < val.NumField(); i++ {
		f := val.Type().Field(i)
		if fieldReadable(v, f) {
			out = append(out, f.Name)
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
	case fmt.Stringer:
		return val.String()

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
		panic(fmt.Sprintf("Type '%s' does not support strconv or Stringable conversion", reflect.TypeOf(v).String()))
	}
}

// makeRecord create a **single** record from a struct & header
func makeRecord(v interface{}, header Header) []string {
	val := reflect.ValueOf(v)
	out := make([]string, 0)

	for _, heading := range header {
		field := val.FieldByName(heading)
		str := toString(field.Interface())
		out = append(out, str)
	}
	return out
}

// makeRecords create a set of records from []struct & hader
func makeRecords(v interface{}, header Header) [][]string {
	val := reflect.ValueOf(v)

	size := val.Len()
	out := make([][]string, size)
	for i := 0; i < size; i++ {
		record := makeRecord(val.Index(i).Interface(), header)
		out[i] = record
	}
	return out
}

func fromString(v interface{}, entry string) reflect.Value {
	// complex number support?
	// Tagged conversion specifiers?
	var out interface{}
	var err error

	switch _val := v.(type) {
	case string:
		reflect.TypeOf(_val) // nop
		out = entry

	case bool:
		out, err = strconv.ParseBool(entry)

	case float32:
		var flt float64
		flt, err = strconv.ParseFloat(entry, 32)
		out = float32(flt)
	case float64:
		out, err = strconv.ParseFloat(entry, 64)

	case int:
		var i int64
		i, err = strconv.ParseInt(entry, 0, 64)
		out = int(i)
	case int8:
		var i int64
		i, err = strconv.ParseInt(entry, 0, 8)
		out = int8(i)
	case int16:
		var i int64
		i, err = strconv.ParseInt(entry, 0, 16)
		out = int16(i)
	case int32: // distinguish from rune by tag?
		var i int64
		i, err = strconv.ParseInt(entry, 0, 32)
		out = int32(i)
	case int64:
		out, err = strconv.ParseInt(entry, 0, 64)

	case uint:
		var i uint64
		i, err = strconv.ParseUint(entry, 0, 64)
		out = uint(i)
	case uint8: // distinguish from byte by tag?
		var i uint64
		i, err = strconv.ParseUint(entry, 0, 8)
		out = uint8(i)
	case uint16:
		var i uint64
		i, err = strconv.ParseUint(entry, 0, 16)
		out = uint16(i)
	case uint32:
		var i uint64
		i, err = strconv.ParseUint(entry, 0, 32)
		out = uint32(i)
	case uint64:
		out, err = strconv.ParseUint(entry, 0, 64)

	default:
		panic(fmt.Sprintf("Type '%s' does not support strconv or Stringable conversion", reflect.TypeOf(v).String()))
	}

	// Not great h&ling for parser, will do for now
	if err != nil {
		panic(err)
	}

	return reflect.ValueOf(out)
}

// makeStruct create type from record & header into ptr
func makeStruct(v interface{}, header, record []string) {
	val := reflect.ValueOf(v).Elem()
	typ := reflect.TypeOf(v).Elem()
	for i, heading := range header {
		tfield, found := typ.FieldByName(heading)
		if !found || tfield.Anonymous {
			continue
		}

		fieldval := val.FieldByName(heading)
		newval := fromString(fieldval.Interface(), record[i])

		fieldval.Set(newval)
	}
}

// makeStructs create a set of structs from records & hader
func makeStructs(v interface{}, header []string, records [][]string) {
	slce := reflect.ValueOf(v).Elem()
	typ := reflect.TypeOf(v).Elem().Elem()

	for _, record := range records {
		tmp := reflect.New(typ)
		makeStruct(tmp.Interface(), header, record)
		slce.Set(reflect.Append(slce, tmp.Elem()))
	}
}
